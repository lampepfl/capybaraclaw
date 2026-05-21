package capybaraclaw.gateway

import capybaraclaw.agent.ClawAgent
import capybaraclaw.gateway.port.Port
import gears.async.{Async, Future, UnboundedChannel}
import org.slf4j.LoggerFactory
import scala.util.control.NonFatal
import tacit.agents.llm.agentic.{AgentRun, AgentStreamEvent}
import tacit.agents.llm.endpoint.{Message, StreamEvent}

private[gateway] final case class RoutedGatewayMessage(
    message: GatewayMessage,
    replyPort: Port
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

  private def runLoop()(using Async.Spawn): Unit =
    var running = true
    while running do
      inbox.read() match
        case Right(routed) =>
          val msg = routed.message
          val replyPort = routed.replyPort
          try processTurn(msg, replyPort)
          catch
            case NonFatal(e) =>
              logger.error(s"[runner $sessionId] turn failed", e)
              try replyPort.sendError(sessionId, msg.origin, e.getMessage)
              catch case NonFatal(_) => ()
          finally
            try replyPort.onTurnFinished(sessionId, msg.origin)
            catch
              case NonFatal(e) =>
                logger.error(
                  s"[runner $sessionId] onTurnFinished failed",
                  e
                )
        case Left(_) =>
          running = false

  private def processTurn(msg: GatewayMessage, replyPort: Port)(using
      Async.Spawn
  ): Unit =
    val tagged = tag(msg)
    contextProvider.append(sessionId, Message.user(tagged))

    val run: AgentRun = claw.streamAsk(tagged)
    var finalText: String = ""
    var reading = true

    while reading do
      run.events.read() match
        case Right(
              Right(AgentStreamEvent.Stream(StreamEvent.Done(response)))
            ) =>
          finalText = response.message.text
          drainSteers(run)
        case Right(_) =>
          drainSteers(run)
        case Left(_) =>
          reading = false

    if finalText.nonEmpty then
      contextProvider.append(sessionId, Message.assistant(finalText))
      try replyPort.send(sessionId, msg.origin, finalText)
      catch
        case NonFatal(e) =>
          logger.error(s"[runner $sessionId] port.send failed", e)

  /** Drain any inbox items that arrived mid-turn, forwarding each as a steer on the
    * active run. Persist only after a successful steer: a rejected steer (race with
    * run termination) is re-delivered to the inbox so the next turn picks it up, and
    * persisting there instead of here keeps the transcript free of duplicates.
    */
  private def drainSteers(run: AgentRun): Unit =
    var draining = true
    while draining do
      inbox.readSource.poll() match
        case Some(Right(m)) =>
          val t = tag(m.message)
          run.steer(t) match
            case tacit.agents.llm.agentic.SteerOutcome.Accepted =>
              contextProvider.append(sessionId, Message.user(t))
            case tacit.agents.llm.agentic.SteerOutcome.RejectedRunEnded =>
              try inbox.sendImmediately(m)
              catch case _: gears.async.ChannelClosedException => ()
              draining = false
        case _ =>
          draining = false

  private def tag(m: GatewayMessage): String =
    s"[${m.origin.user}] ${m.text}"
