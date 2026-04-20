package capybaraclaw.gateway

import capybaraclaw.agent.ClawAgent
import gears.async.{Async, Future, UnboundedChannel}
import tacit.agents.llm.agentic.{AgentRun, AgentStreamEvent}
import tacit.agents.llm.endpoint.{Message, StreamEvent}

/** One runner per `ContextKey`. Owns an inbox, processes messages one turn at a time
  * on its own fiber. While a turn is running, newly-arriving inbox messages are
  * forwarded as live steers on the active `AgentRun` so the LLM can react to them
  * before finishing its response.
  *
  * Messages are tagged `"[userId] text"` on the way into the LLM so a shared-thread
  * agent can still tell who said what.
  */
class AgentRunner(
  key: ContextKey,
  claw: ClawAgent,
  port: Port,
  contextProvider: ContextProvider,
):
  private val inbox = UnboundedChannel[GatewayMessage]()

  def deliver(msg: GatewayMessage): Unit =
    try inbox.sendImmediately(msg)
    catch case _: gears.async.ChannelClosedException => ()

  def close(): Unit =
    try inbox.close() catch case _: Throwable => ()

  def start()(using Async.Spawn): Future[Unit] =
    Future(runLoop())

  private def runLoop()(using Async.Spawn): Unit =
    var running = true
    while running do
      inbox.read() match
        case Right(msg) =>
          try processTurn(msg)
          catch case e: Exception =>
            System.err.println(s"[runner ${key}] turn failed: ${e.getMessage}")
        case Left(_) =>
          running = false

  private def processTurn(msg: GatewayMessage)(using Async.Spawn): Unit =
    val tagged = tag(msg)
    contextProvider.append(key, Message.user(tagged))

    val run: AgentRun = claw.streamAsk(tagged)
    var finalText: String = ""
    var reading = true

    while reading do
      run.events.read() match
        case Right(Right(AgentStreamEvent.Stream(StreamEvent.Done(response)))) =>
          finalText = response.message.text
          drainSteers(run)
        case Right(_) =>
          drainSteers(run)
        case Left(_) =>
          reading = false

    if finalText.nonEmpty then
      contextProvider.append(key, Message.assistant(finalText))
      try port.send(key, finalText)
      catch case e: Exception =>
        System.err.println(s"[runner ${key}] port.send failed: ${e.getMessage}")

  /** Drain any inbox items that arrived mid-turn, forwarding each as a steer on the
    * active run. Persist only after a successful steer: a rejected steer (race with
    * run termination) is re-delivered to the inbox so the next turn picks it up, and
    * persisting there instead of here keeps the transcript free of duplicates. */
  private def drainSteers(run: AgentRun): Unit =
    var draining = true
    while draining do
      inbox.readSource.poll() match
        case Some(Right(m)) =>
          val t = tag(m)
          run.steer(t) match
            case tacit.agents.llm.agentic.SteerOutcome.Accepted =>
              contextProvider.append(key, Message.user(t))
            case tacit.agents.llm.agentic.SteerOutcome.RejectedRunEnded =>
              try inbox.sendImmediately(m)
              catch case _: gears.async.ChannelClosedException => ()
              draining = false
        case _ =>
          draining = false

  private def tag(m: GatewayMessage): String =
    s"[${m.origin.user}] ${m.text}"
