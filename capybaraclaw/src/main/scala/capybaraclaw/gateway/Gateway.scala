package capybaraclaw.gateway

import capybaraclaw.Throwables
import capybaraclaw.agent.ClawAgent
import capybaraclaw.gateway.port.Port
import gears.async.{Async, Future}
import org.slf4j.LoggerFactory
import scala.collection.mutable
import scala.util.control.NonFatal

/** A message handed to the Gateway by a Port. */
case class GatewayMessage(origin: Origin, text: String)

/** Routes messages from N ports into per-session `AgentRunner`s.
  *
  * Responsibilities:
  *  - spawn one reader fiber per port that pumps `port.incoming` into runners,
  *  - resolve external handles to UUID sessions and lazily create runners with
  *    rehydrated history,
  *  - find the right port when a runner replies (via the original Origin.port) and
  *    route the reply to it,
  *  - clean up on shutdown.
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
  private val logger = LoggerFactory.getLogger(classOf[Gateway])
  private val portsById: Map[PortId, Port] = ports.map(p => p.id -> p).toMap
  require(portsById.size == ports.size, "Port ids must be unique")

  private val runners = mutable.Map[SessionId, AgentRunner]()
  private val runnersLock = new Object

  /** Pump all ports until cancelled. Spawns one reader fiber per port, then blocks
    * the caller (the outer `Async.blocking`) until the scope is cancelled.
    */
  def run()(using Async.Spawn): Unit =
    val readers = ports.map: port =>
      Future:
        readFromPort(port)
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
            logger.warn(
              "dropping message from port '{}' with mismatched origin.port='{}'",
              port.id,
              msg.origin.port
            )
            rejectInbound(
              port,
              msg.origin,
              s"origin port '${msg.origin.port}' does not match receiving port '${port.id}'"
            )
          else
            try
              port.validateOriginForReply(msg.origin)
              val sessionId = sessionIdFor(msg.origin)
              val runner = getOrCreateRunner(sessionId)
              runner.deliver(RoutedGatewayMessage(msg, port))
            catch
              case e: IllegalArgumentException =>
                logger.warn(
                  "dropping message with invalid session origin: {}",
                  e.getMessage
                )
                rejectInbound(port, msg.origin, e.getMessage)
              case NonFatal(e) =>
                logger.warn(
                  "dropping message after session resolution failed",
                  e
                )
                rejectInbound(port, msg.origin, Throwables.errorMessage(e))
        case Left(_) =>
          running = false

  private def getOrCreateRunner(sessionId: SessionId)(using
      Async.Spawn
  ): AgentRunner =
    runnersLock.synchronized:
      runners.get(sessionId) match
        case Some(r) => r
        case None    =>
          val history = contextProvider.load(sessionId)
          val claw = clawFactory(workDir, history)
          val runner =
            AgentRunner(sessionId, claw, contextProvider)
          runner.start()
          runners.update(sessionId, runner)
          runner

  private def sessionIdFor(origin: Origin): SessionId =
    origin.session match
      case SessionRef.Direct(sessionId) =>
        contextProvider.verifyAndTouchSession(sessionId, workDir) match
          case Some(metadata) if metadata.workdir == workDir =>
            sessionId
          case Some(metadata) =>
            throw IllegalArgumentException(
              s"session $sessionId belongs to workdir '${metadata.workdir}', not '$workDir'"
            )
          case None =>
            throw IllegalArgumentException(
              s"session not found: $sessionId"
            )
      case SessionRef.External(handle) =>
        if handle.kind != origin.port then
          throw IllegalArgumentException(
            s"external session handle kind '${handle.kind}' does not match origin port '${origin.port}'"
          )
        contextProvider.resolveOrCreateHandle(workDir, handle)

  private def rejectInbound(
      port: Port,
      origin: Origin,
      text: String
  ): Unit =
    try port.rejectInbound(origin, text)
    catch
      case NonFatal(e) =>
        logger.warn(
          "failed to notify port '{}' about rejected message",
          port.id,
          e
        )
