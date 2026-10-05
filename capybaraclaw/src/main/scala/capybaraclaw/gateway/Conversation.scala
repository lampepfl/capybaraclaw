package capybaraclaw.gateway

/** Who can read what is said in a session, as reported by its port. Decides
  * which memory the session's agent sees: private memory is only ever loaded
  * into a [[Conversation.Direct]] session.
  */
enum Conversation:
  /** Only the origin's user and the agent, e.g. the CLI or a Slack DM. */
  case Direct

  /** Several people, e.g. a Slack channel or group DM. `channel` is the
    * port-local id of the place everyone in it shares.
    */
  case Group(channel: String)
