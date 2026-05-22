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

  /** Look up a canonical session by UUID; bump `last_activity` only when the
    * stored workdir matches `expectedWorkdir`. Returns the pre-bump metadata,
    * or `None` when the session does not exist.
    */
  def verifyAndTouchSession(
      id: SessionId,
      expectedWorkdir: String
  ): Option[SessionMetadata]

  /** Resolve an external handle to its canonical session UUID, creating both
    * the session and handle when absent. Bumps `last_activity` on a hit so the
    * caller does not need a follow-up touch.
    */
  def resolveOrCreateHandle(
      workdir: String,
      handle: SessionHandle
  ): SessionId

  /** Load prior conversation for this session. Empty list for never-seen sessions. */
  def load(sessionId: SessionId): List[Message]

  /** Append a single message to this session's transcript. */
  def append(sessionId: SessionId, msg: Message): Unit
