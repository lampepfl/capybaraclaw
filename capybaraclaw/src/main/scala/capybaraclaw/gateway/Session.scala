package capybaraclaw.gateway

import java.time.Instant
import java.util.UUID

opaque type SessionId <: String = String

object SessionId:
  def apply(value: String): SessionId =
    UUID.fromString(value).toString

  def random(): SessionId =
    SessionId(UUID.randomUUID().toString)

final case class SessionMetadata(
    id: SessionId,
    workdir: String,
    createdAt: Instant,
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
final case class Origin(
    port: PortId,
    user: UserId,
    session: SessionRef
)
