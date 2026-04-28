package capybaraclaw.gateway.port

import capybaraclaw.gateway.{ContextKey, GatewayMessage}
import gears.async.ReadableChannel

/** A message source/sink through which the Gateway talks to the outside world.
  *
  * A `Port` aggregates one logical channel of inbound `GatewayMessage`s and a way to
  * send replies back. The Gateway pumps `incoming` into per-thread agent runners and
  * calls `send` when a runner produces a reply for this port's threads.
  */
trait Port:
  /** Unique identifier for this port, used as `Origin.port` on inbound messages. */
  def id: String

  /** Messages arriving on this port. Every message's `origin.port` must equal `id`. */
  def incoming: ReadableChannel[GatewayMessage]

  /** Deliver a reply to a thread previously seen on this port. */
  def send(key: ContextKey, text: String): Unit

  /** Deliver an error for a turn (e.g. LLM timeout, tool failure). */
  def sendError(key: ContextKey, text: String): Unit =
    send(key, s"ERROR: $text")

  /** Called after a turn for `key` finishes, regardless of whether it produced
    * a reply. Ports that want to block input until a turn is done can override this.
    */
  def onTurnFinished(key: ContextKey): Unit = ()

  /** Release any resources (network connections, background listeners). */
  def shutdown(): Unit
