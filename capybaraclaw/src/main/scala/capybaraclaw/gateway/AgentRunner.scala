package capybaraclaw.gateway

import capybaraclaw.agent.ClawAgent
import capybaraclaw.gateway.port.{Port, ReplyStream}
import gears.async.{Async, Future, UnboundedChannel}
import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonFatal
import tacit.agents.llm.agentic.{AgentError, AgentRun, AgentStreamEvent}
import tacit.agents.llm.endpoint.{Content, Message, StreamEvent}

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
  * on its own fiber. While a turn is running, newly-arriving messages from the
  * turn's sender are forwarded as live steers on the active `AgentRun` so the LLM
  * can react to them before finishing its response. Messages from anyone else
  * wait for a turn of their own: a turn runs under its sender's permission
  * grants, which nobody else may steer. So a sender who keeps steering keeps
  * everyone else waiting until their turn ends.
  *
  * Messages are tagged `"[userId] text"` on the way into the LLM so a shared-thread
  * agent can still tell who said what.
  */
class AgentRunner(
    sessionId: SessionId,
    claw: ClawAgent,
    contextProvider: ContextProvider,
    approvals: ApprovalBroker
):
  private val logger = LoggerFactory.getLogger(classOf[AgentRunner])
  private val inbox = UnboundedChannel[RoutedGatewayMessage]()

  /** Messages that arrived during someone else's turn, in arrival order; they
    * are taken before the inbox. Only touched on the runner's fiber.
    */
  private val held = mutable.ArrayDeque[RoutedGatewayMessage]()

  def deliver(routed: RoutedGatewayMessage): Unit =
    try inbox.sendImmediately(routed)
    catch case _: gears.async.ChannelClosedException => ()

  def close(): Unit =
    try inbox.close()
    catch case NonFatal(_) => ()

  def start()(using Async.Spawn): Future[Unit] =
    Future(runLoop())

  @tailrec
  private def runLoop()(using Async.Spawn): Unit =
    val next =
      if held.nonEmpty then Right(held.removeHead()) else inbox.read()
    next match
      case Right(routed) =>
        val msg = routed.message
        val replyPort = routed.replyPort
        val replyStream = replyPort.openReply(sessionId, msg.origin)
        approvals.beginTurn(sessionId, replyPort, msg.origin)
        try processTurn(msg, replyPort, replyStream)
        catch
          case NonFatal(e) =>
            logger.error(s"[runner $sessionId] turn failed", e)
            try replyStream.abort(e.getMessage)
            catch case NonFatal(_) => ()
        finally
          approvals.endTurn(sessionId)
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

  private def processTurn(
      msg: GatewayMessage,
      replyPort: Port,
      replyStream: ReplyStream
  )(using Async.Spawn): Unit =
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
        toolInputs: Map[String, String],
        aborted: Boolean
    ): TurnResult =
      readEvent(run) match
        case Emitted(Stream(Delta(text))) =>
          val next = reply.delta(text)
          drainSteers(run, msg.origin)
          consume(next, finalText, toolInputs, aborted)
        case Emitted(Stream(Done(response))) =>
          val nextToolInputs = toolInputs ++ response.message.content.collect:
            case Content.ToolUse(id, _, input) => id -> input
          drainSteers(run, msg.origin)
          consume(reply, response.message.text, nextToolInputs, aborted)
        case Emitted(ToolResult(id, toolName, _)) =>
          val args = toolInputs.getOrElse(id, "")
          try replyPort.sendToolCall(sessionId, msg.origin, toolName, args)
          catch
            case NonFatal(e) =>
              logger.error(s"[runner $sessionId] sendToolCall failed", e)
          drainSteers(run, msg.origin)
          consume(reply, finalText, toolInputs, aborted)
        case Emitted(_) =>
          drainSteers(run, msg.origin)
          consume(reply, finalText, toolInputs, aborted)
        case Failed(error) =>
          if !aborted then
            logger.error(
              s"[runner $sessionId] agent run failed: ${error.description}"
            )
            reply.abort(error.description)
          consume(reply, finalText, toolInputs, aborted = true)
        case Closed =>
          TurnResult(reply, finalText, aborted)

    val result = consume(replyStream, "", Map.empty, aborted = false)
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

  /** Forward messages from `sender` that arrived mid-turn as steers on the
    * active run, in order; everyone else's stay held for a later turn. Persist
    * only after a successful steer: a rejected steer (race with run
    * termination) stays held so the next turn picks it up, and persisting
    * there instead of here keeps the transcript free of duplicates.
    */
  private def drainSteers(run: AgentRun, sender: Origin): Unit =
    @tailrec
    def pollInbox(): Unit =
      inbox.readSource.poll() match
        case Some(Right(m)) =>
          held.append(m)
          pollInbox()
        case _ => ()

    @tailrec
    def steerNext(): Unit =
      held.indexWhere(m => sameSender(m.message.origin, sender)) match
        case -1 => ()
        case i  =>
          val t = tag(held(i).message)
          run.steer(t) match
            case tacit.agents.llm.agentic.SteerOutcome.Accepted =>
              val _ = held.remove(i)
              try contextProvider.append(sessionId, Message.user(t))
              catch
                case NonFatal(e) =>
                  logger.error(
                    s"[runner $sessionId] failed to persist steer",
                    e
                  )
              steerNext()
            case tacit.agents.llm.agentic.SteerOutcome.RejectedRunEnded =>
              ()

    pollInbox()
    steerNext()

  private def sameSender(a: Origin, b: Origin): Boolean =
    a.port == b.port && a.user == b.user

  private def tag(m: GatewayMessage): String =
    s"[${m.origin.user}] ${m.text}"
