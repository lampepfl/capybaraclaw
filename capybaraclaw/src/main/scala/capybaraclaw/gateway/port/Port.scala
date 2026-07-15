package capybaraclaw.gateway.port

import capybaraclaw.gateway.{GatewayMessage, Origin, PortId, SessionId}
import gears.async.ReadableChannel

/** A message source/sink through which the Gateway talks to the outside world.
  *
  * A `Port` aggregates one logical channel of inbound `GatewayMessage`s and a way to
  * deliver replies back. The Gateway pumps `incoming` into per-session agent runners.
  * Each turn calls `openReply` and streams assistant output through the returned `ReplyStream`.
  */
trait Port extends AutoCloseable:
  /** Unique identifier for this port, used as `Origin.port` on inbound messages. */
  def id: PortId

  /** Messages arriving on this port. Every message's `origin.port` must equal `id`. */
  def incoming: ReadableChannel[GatewayMessage]

  /** Validate that this port can later send a reply for `origin`. Gateway calls
    * this before handing a message to an AgentRunner so malformed origins fail
    * before the agent generates and persists an undeliverable response.
    */
  def validateOriginForReply(origin: Origin): Unit = ()

  /** Open a reply sink for an agent turn. Called at the start of each turn for a
    * message that originated on this port. The runner forwards stream deltas, then
    * calls `complete` or `abort` before the turn ends.
    */
  def openReply(sessionId: SessionId, origin: Origin): ReplyStream

  /** Notify the port that a tool call finished, with its name and arguments. */
  def sendToolCall(
      sessionId: SessionId,
      origin: Origin,
      toolName: String,
      args: String
  ): Unit = ()

  /** Called after a turn for `sessionId` finishes, regardless of whether it
    * produced a reply. Ports that want to block input until a turn is done can
    * override this.
    */
  def onTurnFinished(
      sessionId: SessionId,
      origin: Origin
  ): Unit = ()

  /** Notify a port that an inbound message was rejected before a runner could
    * take ownership of the turn. External ports must map the unresolved origin
    * back to their native address so users are not left without feedback.
    */
  def rejectInbound(origin: Origin, text: String): Unit

  /** Release any resources (network connections, background listeners). */
  def shutdown(): Unit

  /** AutoCloseable bridge so a `Port` can be managed by `scala.util.Using`. */
  override def close(): Unit = shutdown()

trait ReplyStream:
  def delta(text: String): Unit
  def complete(finalText: String): Unit
  def abort(reason: String): Unit
