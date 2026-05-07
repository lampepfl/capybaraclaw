package capybaraclaw.gateway

import tacit.agents.llm.endpoint.Message

/** Persistent transcript store, keyed by `ContextKey` so each (port, thread) has its
  * own conversation history. Used by the Gateway to seed a fresh `ClawAgent` on first
  * message and to append each new user/assistant message as the turn proceeds.
  */
trait ContextProvider:
  /** Load prior conversation for this key. Empty list for never-seen keys. */
  def load(key: ContextKey): List[Message]

  /** Append a single message to this key's transcript. */
  def append(key: ContextKey, msg: Message): Unit
