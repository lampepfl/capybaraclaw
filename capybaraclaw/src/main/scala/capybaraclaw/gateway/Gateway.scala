package capybaraclaw.gateway

import capybaraclaw.agent.ClawAgent
import capybaraclaw.gateway.port.Port
import gears.async.{Async, Future}
import scala.collection.mutable

/** Sender identity of an inbound message. */
case class Origin(port: String, thread: String, user: String)

/** Key under which an agent instance (and its context) is shared. All users in the
  * same (port, thread) converse with the same `ClawAgent` / REPL / history.
  */
case class ContextKey(port: String, thread: String)

object ContextKey:
  def of(o: Origin): ContextKey = ContextKey(o.port, o.thread)

/** A message handed to the Gateway by a Port. */
case class GatewayMessage(origin: Origin, text: String)

/** Routes messages from N ports into per-(port,thread) `AgentRunner`s.
  *
  * Responsibilities:
  *  - spawn one reader fiber per port that pumps `port.incoming` into runners,
  *  - lazily create a runner (with rehydrated history) on first message for a key,
  *  - find the right port when a runner replies and route `send(key, text)` to it,
  *  - clean up on shutdown.
  *
  * Assumes each port's inbound messages carry `origin.port == port.id` so routing
  * replies back is unambiguous.
  */
class Gateway(
    workDir: String,
    ports: List[Port],
    contextProvider: ContextProvider,
    clawFactory: (
        String,
        List[tacit.agents.llm.endpoint.Message]
    ) => ClawAgent = (wd, hist) => ClawAgent(wd, initialMessages = hist)
):
  private val portsById: Map[String, Port] = ports.map(p => p.id -> p).toMap
  require(portsById.size == ports.size, "Port ids must be unique")

  private val runners = mutable.Map[ContextKey, AgentRunner]()
  private val runnersLock = new Object

  /** Pump all ports until cancelled. Spawns one reader fiber per port, then blocks
    * the caller (the outer `Async.blocking`) until the scope is cancelled.
    */
  def run()(using Async.Spawn): Unit =
    val readers = ports.map: port =>
      Future:
        readFromPort(port)
    // Await every reader. These futures end when their port's `incoming` channel closes.
    readers.foreach(_.awaitResult)

  /** Close port inputs and stop all runner fibers. */
  def shutdown(): Unit =
    ports.foreach(_.shutdown())
    runnersLock.synchronized:
      runners.values.foreach(_.close())

  private def readFromPort(port: Port)(using Async.Spawn): Unit =
    var running = true
    while running do
      port.incoming.read() match
        case Right(msg) =>
          if msg.origin.port != port.id then
            System.err.println(
              s"[gateway] dropping message from port '${port.id}' with mismatched origin.port='${msg.origin.port}'"
            )
          else
            val runner = getOrCreateRunner(ContextKey.of(msg.origin))
            runner.deliver(msg)
        case Left(_) =>
          running = false

  private def getOrCreateRunner(key: ContextKey)(using
      Async.Spawn
  ): AgentRunner =
    runnersLock.synchronized:
      runners.get(key) match
        case Some(r) => r
        case None    =>
          val port = portsById.getOrElse(
            key.port,
            throw RuntimeException(s"No port registered with id '${key.port}'")
          )
          val history = contextProvider.load(key)
          val claw = clawFactory(workDir, history)
          val runner = AgentRunner(key, claw, port, contextProvider)
          runner.start()
          runners.update(key, runner)
          runner
