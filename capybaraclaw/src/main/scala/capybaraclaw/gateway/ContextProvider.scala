package capybaraclaw.gateway

import tacit.agents.llm.endpoint.Message

/** Persistent transcript store, keyed by canonical UUID `SessionId`.
  *
  * External systems can attach handles to a session. A session owns exactly one
  * workdir.
  */
trait ContextProvider:
  /** Create a fresh canonical session UUID for `workdir`. */
  def createSession(workdir: String): SessionId

  /** Look up a canonical session by UUID. */
  def resumeSession(id: SessionId): Option[SessionMetadata]

  /** Resolve an external handle to its canonical session UUID, creating both the
    * session and handle when absent.
    */
  def resolveOrCreateHandle(
      workdir: String,
      handle: SessionHandle
  ): SessionId

  /** Bump `last_activity` for an existing session. */
  def touchSession(sessionId: SessionId): Unit

  /** Load prior conversation for this session. Empty list for never-seen sessions. */
  def load(sessionId: SessionId): List[Message]

  /** Append a single message to this session's transcript. */
  def append(sessionId: SessionId, msg: Message): Unit
