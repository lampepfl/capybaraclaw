package capybaraclaw.gateway

import java.time.Instant

enum SearchSort:
  case Rank, Newest, Oldest

object SearchSort:
  val names: String = "rank, newest or oldest"

  def parse(s: String): Option[SearchSort] = s.trim.toLowerCase match
    case "rank"   => Some(Rank)
    case "newest" => Some(Newest)
    case "oldest" => Some(Oldest)
    case _        => None

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

/** A message without its text; `length` counts characters. */
final case class MessageMeta(
    id: Long,
    role: String,
    createdAt: Instant,
    length: Int
)

/** A matched message: each fragment shows a match with about
  * [[SessionSearch.FragmentRadius]] characters on each side, matches marked
  * `«like this»`. `moreFragments` counts the fragments left out.
  */
final case class MessageMatch(
    message: MessageMeta,
    matchCount: Int,
    fragments: List[String],
    moreFragments: Int
)

final case class SessionInfo(
    sessionId: SessionId,
    workdir: String,
    title: String,
    createdAt: Instant,
    lastActivity: Instant
)

/** One session matching a search: its best-ranked matched messages, and the
  * messages around the best one, without their text.
  */
final case class SessionHit(
    session: SessionInfo,
    matchedMessages: Int,
    matches: List[MessageMatch],
    context: List[MessageMeta]
)

/** A page of matching sessions; `totalSessions` counts all of them. */
final case class Discovery(totalSessions: Int, hits: List[SessionHit])

/** A message in a scroll window; `text` is set only when asked for. */
final case class WindowMessage(message: MessageMeta, text: Option[String])

final case class SessionWindow(
    session: SessionInfo,
    aroundMessageId: Long,
    messages: List[WindowMessage]
)

final case class FullMessage(
    sessionId: SessionId,
    message: MessageMeta,
    text: String
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
  *
  * Search results never carry whole messages, only fragments around the
  * matches and the length of each message, so a long message cannot flood
  * the context and nothing is cut off silently: the agent fetches the
  * messages it needs whole with [[get]].
  */
trait SessionSearch:
  def discover(
      terms: SearchTerms,
      limit: Int,
      offset: Int,
      window: Int,
      sort: SearchSort,
      excludeSession: Option[SessionId]
  ): Discovery

  def scroll(
      sessionId: SessionId,
      aroundMessageId: Long,
      window: Int,
      fullText: Boolean
  ): Option[SessionWindow]

  /** The messages with these ids, whole, in id order; ids not found or in
    * `excludeSession` are left out.
    */
  def get(ids: List[Long], excludeSession: Option[SessionId]): List[FullMessage]

  def browse(
      limit: Int,
      offset: Int,
      excludeSession: Option[SessionId]
  ): List[SessionSummary]

object SessionSearch:
  /** Characters shown on each side of a match. */
  val FragmentRadius: Int = 200

  /** How far a fragment edge may move outwards to reach a word boundary. */
  val BoundarySlack: Int = 40

  val FragmentsPerMessage: Int = 5
  val MatchesPerSession: Int = 5

  /** No-op, used by tests. */
  val empty: SessionSearch = new SessionSearch:
    def discover(
        terms: SearchTerms,
        l: Int,
        o: Int,
        w: Int,
        s: SearchSort,
        ex: Option[SessionId]
    ) = Discovery(0, Nil)
    def scroll(id: SessionId, around: Long, w: Int, full: Boolean) = None
    def get(ids: List[Long], ex: Option[SessionId]) = Nil
    def browse(l: Int, o: Int, ex: Option[SessionId]) = Nil
