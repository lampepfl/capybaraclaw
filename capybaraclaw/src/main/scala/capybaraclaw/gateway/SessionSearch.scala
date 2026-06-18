package capybaraclaw.gateway

import java.time.Instant

enum SearchSort:
  case Rank, Newest, Oldest

object SearchSort:
  def fromString(s: String): SearchSort = s.trim.toLowerCase match
    case "newest" => Newest
    case "oldest" => Oldest
    case _        => Rank

/** Structured full-text search intent.
  *
  * @param allOf  every term must appear (AND)
  * @param anyOf  at least one term must appear (OR)
  * @param noneOf no term may appear (NOT); requires at least one positive term
  * @param prefix prefix-match the positive terms (allOf/anyOf); exclusions stay exact
  */
final case class SearchTerms(
    allOf: List[String] = Nil,
    anyOf: List[String] = Nil,
    noneOf: List[String] = Nil,
    prefix: Boolean = false
)

/** A single message inside a context window. `anchor` marks the matched message. */
final case class MessageEntry(
    id: Long,
    role: String,
    text: String,
    createdAt: Instant,
    anchor: Boolean = false
)

/** A discover result: one per session, anchored on its best-ranked match. */
final case class SessionHit(
    sessionId: SessionId,
    workdir: String,
    title: String,
    sessionCreatedAt: Instant,
    lastActivity: Instant,
    matchMessageId: Long,
    matchedRole: String,
    snippet: String,
    window: List[MessageEntry],
    bookendStart: List[MessageEntry],
    bookendEnd: List[MessageEntry]
)

/** A scroll result: a window of messages around a given message id. */
final case class SessionWindow(
    sessionId: SessionId,
    workdir: String,
    title: String,
    sessionCreatedAt: Instant,
    lastActivity: Instant,
    aroundMessageId: Long,
    window: List[MessageEntry]
)

/** A browse result: a recent-session summary. */
final case class SessionSummary(
    sessionId: SessionId,
    workdir: String,
    title: String,
    createdAt: Instant,
    lastActivity: Instant,
    messageCount: Int
)

/** Read-model for full-text search over past sessions. Distinct from
  * [[ContextProvider]] (transcript persistence) by responsibility.
  */
trait SessionSearch:
  def discover(
      terms: SearchTerms,
      limit: Int,
      offset: Int,
      window: Int,
      sort: SearchSort,
      excludeSession: Option[SessionId]
  ): List[SessionHit]

  def scroll(
      sessionId: SessionId,
      aroundMessageId: Long,
      window: Int
  ): Option[SessionWindow]

  def browse(
      limit: Int,
      offset: Int,
      excludeSession: Option[SessionId]
  ): List[SessionSummary]

object SessionSearch:
  /** No-op, used by the Gateway default factory and by tests. */
  val empty: SessionSearch = new SessionSearch:
    def discover(
        terms: SearchTerms,
        l: Int,
        o: Int,
        w: Int,
        s: SearchSort,
        ex: Option[SessionId]
    ) = Nil
    def scroll(id: SessionId, around: Long, w: Int) = None
    def browse(l: Int, o: Int, ex: Option[SessionId]) = Nil
