package capybaraclaw.gateway

import java.time.Instant
import java.util.UUID

final class SessionId private (val value: String):
  override def equals(other: Any): Boolean =
    other match
      case that: SessionId => value == that.value
      case _               => false

  override def hashCode(): Int = value.hashCode

  override def toString: String = value

object SessionId:
  def apply(value: String): SessionId =
    new SessionId(UUID.fromString(value).toString)

  def random(): SessionId =
    SessionId(UUID.randomUUID().toString)

final case class SessionMetadata(
    id: SessionId,
    workdir: String,
    lastActivity: Instant
)

final case class SessionHandle(kind: PortId, value: String):
  require(value.nonEmpty, "session handle value must not be empty")

sealed trait SessionRef

object SessionRef:
  final case class Direct(id: SessionId) extends SessionRef
  final case class External(handle: SessionHandle) extends SessionRef

/** Sender identity of an inbound message. CLI provides the canonical session UUID
  * directly; external ports provide a handle that resolves to a UUID.
  */
case class Origin(
    port: PortId,
    user: UserId,
    session: SessionRef
)
