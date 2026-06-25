package capybaraclaw.gateway

import capybaraclaw.agent.ClawAgent
import capybaraclaw.gateway.port.{Port, ReplyStream}
import gears.async.{Async, Future, UnboundedChannel}
import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import scala.util.control.NonFatal
import tacit.agents.llm.agentic.{AgentError, AgentRun, AgentStreamEvent}
import tacit.agents.llm.endpoint.{Message, StreamEvent}

private[gateway] final case class RoutedGatewayMessage(
    message: GatewayMessage,
    replyPort: Port
)

private enum RunEvent:
  case Emitted(event: AgentStreamEvent)
  case Failed(error: AgentError)
  case Closed

private final case class TurnResult(
    replyStream: ReplyStream,
    finalText: String,
    aborted: Boolean
)

/** One runner per `sessionId`. Owns an inbox, processes messages one turn at a time
  * on its own fiber. While a turn is running, newly-arriving inbox messages are
  * forwarded as live steers on the active `AgentRun` so the LLM can react to them
  * before finishing its response.
  *
  * Messages are tagged `"[userId] text"` on the way into the LLM so a shared-thread
  * agent can still tell who said what.
  */
class AgentRunner(
    sessionId: SessionId,
    claw: ClawAgent,
    contextProvider: ContextProvider
):
  private val logger = LoggerFactory.getLogger(classOf[AgentRunner])
  private val inbox = UnboundedChannel[RoutedGatewayMessage]()

  def deliver(routed: RoutedGatewayMessage): Unit =
    try inbox.sendImmediately(routed)
    catch case _: gears.async.ChannelClosedException => ()

  def close(): Unit =
    try inbox.close()
    catch case _: Throwable => ()

  def start()(using Async.Spawn): Future[Unit] =
    Future(runLoop())

  @tailrec
  private def runLoop()(using Async.Spawn): Unit =
    inbox.read() match
      case Right(routed) =>
        val msg = routed.message
        val replyPort = routed.replyPort
        val replyStream = replyPort.openReply(sessionId, msg.origin)
        try processTurn(msg, replyStream)
        catch
          case NonFatal(e) =>
            logger.error(s"[runner $sessionId] turn failed", e)
            try replyStream.abort(e.getMessage)
            catch case NonFatal(_) => ()
        finally
          try replyPort.onTurnFinished(sessionId, msg.origin)
          catch
            case NonFatal(e) =>
              logger.error(
                s"[runner $sessionId] onTurnFinished failed",
                e
              )
        runLoop()
      case Left(_) =>
        ()

  private def processTurn(msg: GatewayMessage, replyStream: ReplyStream)(using
      Async.Spawn
  ): Unit =
    val tagged = tag(msg)
    contextProvider.append(sessionId, Message.user(tagged))

    val run: AgentRun = claw.streamAsk(tagged)

    import RunEvent.*
    import AgentStreamEvent.*
    import StreamEvent.*

    @tailrec
    def consume(
        reply: ReplyStream,
        finalText: String,
        aborted: Boolean
    ): TurnResult =
      readEvent(run) match
        case Emitted(Stream(Delta(text))) =>
          val next = reply.delta(text)
          drainSteers(run)
          consume(next, finalText, aborted)
        case Emitted(Stream(Done(response))) =>
          drainSteers(run)
          consume(reply, response.message.text, aborted)
        case Emitted(_) =>
          drainSteers(run)
          consume(reply, finalText, aborted)
        case Failed(error) =>
          if !aborted then
            logger.error(
              s"[runner $sessionId] agent run failed: ${error.description}"
            )
            reply.abort(error.description)
          consume(reply, finalText, aborted = true)
        case Closed =>
          TurnResult(reply, finalText, aborted)

    val result = consume(replyStream, "", aborted = false)
    if !result.aborted then
      if result.finalText.nonEmpty then
        try
          contextProvider.append(sessionId, Message.assistant(result.finalText))
        catch
          case NonFatal(e) =>
            logger.error(
              s"[runner $sessionId] failed to persist assistant reply",
              e
            )
      try result.replyStream.complete(result.finalText)
      catch
        case NonFatal(e) =>
          logger.error(
            s"[runner $sessionId] failed to finalize reply stream",
            e
          )

  private def readEvent(run: AgentRun)(using Async): RunEvent =
    run.events.read() match
      case Right(Right(event)) => RunEvent.Emitted(event)
      case Right(Left(error))  => RunEvent.Failed(error)
      case Left(_)             => RunEvent.Closed

  /** Drain any inbox items that arrived mid-turn, forwarding each as a steer on the
    * active run. Persist only after a successful steer: a rejected steer (race with
    * run termination) is re-delivered to the inbox so the next turn picks it up, and
    * persisting there instead of here keeps the transcript free of duplicates.
    */
  @tailrec
  private def drainSteers(run: AgentRun): Unit =
    inbox.readSource.poll() match
      case Some(Right(m)) =>
        val t = tag(m.message)
        run.steer(t) match
          case tacit.agents.llm.agentic.SteerOutcome.Accepted =>
            try contextProvider.append(sessionId, Message.user(t))
            catch
              case NonFatal(e) =>
                logger.error(s"[runner $sessionId] failed to persist steer", e)
            drainSteers(run)
          case tacit.agents.llm.agentic.SteerOutcome.RejectedRunEnded =>
            try inbox.sendImmediately(m)
            catch case _: gears.async.ChannelClosedException => ()
      case _ =>
        ()

  private def tag(m: GatewayMessage): String =
    s"[${m.origin.user}] ${m.text}"
