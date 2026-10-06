package capybaraclaw.agent.tools

import tacit.agents.llm.agentic.Agent
import tacit.agents.llm.utils.{IsToolArg, desc}

import capybaraclaw.gateway.*

import org.slf4j.LoggerFactory

import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.util.control.NonFatal

object SessionSearchTool:

  case class Args(
      @desc(
        "Terms that must ALL appear (AND). Set any of all_of/any_of to search (discover mode). " +
          "Each term is matched literally — a term may be a single word or a multi-word phrase."
      )
      all_of: Option[List[String]] = None,
      @desc("Terms where at least ONE must appear (OR). Matched literally.")
      any_of: Option[List[String]] = None,
      @desc(
        "Terms that must NOT appear (NOT). Requires at least one all_of/any_of term. Matched literally."
      )
      none_of: Option[List[String]] = None,
      @desc(
        "Prefix-match the positive (all_of/any_of) terms, e.g. 'deploy' also matches 'deployment'. Exclusions stay exact."
      )
      prefix: Option[Boolean] = None,
      @desc(
        "Session id to scroll within (scroll mode). Requires around_message_id."
      )
      session_id: Option[String] = None,
      @desc("Message id to center the scroll window on. Requires session_id.")
      around_message_id: Option[Long] = None,
      @desc(
        "Scroll mode: include each message's whole text. Off by default; check message lengths first."
      )
      full_text: Option[Boolean] = None,
      @desc(
        s"Ids of messages to fetch whole (get mode), at most $MaxGetIds."
      )
      message_ids: Option[List[Long]] = None,
      @desc(
        s"Max results. Discover default 3 (at most $MaxDiscoverLimit), browse default 5 (at most $MaxBrowseLimit)."
      )
      limit: Option[Int] = None,
      @desc(
        "Number of results to skip, for paging (discover/browse). Default 0."
      )
      offset: Option[Int] = None,
      @desc(
        s"Messages on each side of the best match (discover, default 5, at most $MaxDiscoverWindow) " +
          s"or of around_message_id (scroll, default 10, at most $MaxScrollWindow)."
      )
      window: Option[Int] = None,
      @desc(
        "Discover order: 'rank' (best match first, default), 'newest' or 'oldest' (by the session's latest or earliest match)."
      )
      sort: Option[String] = None
  ) derives IsToolArg

  val name: String = "session_search"
  val description: String =
    "Search your own past sessions (all projects) by full text. " +
      "Four modes, inferred from arguments: " +
      "set 'all_of'/'any_of'/'none_of' to find sessions matching terms (discover) — returns one result per " +
      s"session with fragments of about ${SessionSearch.FragmentRadius} characters around each «match» " +
      "and the ids, roles and lengths of the surrounding messages, never whole messages; " +
      "set 'message_ids' to fetch whole messages (get); " +
      "set 'session_id' + 'around_message_id' to page through a session's messages (scroll); " +
      "pass no arguments to list your most recent sessions (browse). " +
      "A fragment is only part of its message: fetch the whole message with 'message_ids' before relying on " +
      "what it says. The current session is always excluded. Terms are matched literally — you do NOT write " +
      "query syntax; combine them via the all_of (AND), any_of (OR) and none_of (NOT) lists."

  def register(
      agent: Agent,
      search: SessionSearch,
      currentSession: SessionId
  ): Unit =
    agent.handle[Args](name, description): (args, _) =>
      run(search, currentSession, args)

  private val logger = LoggerFactory.getLogger(getClass)

  private val timeFmt =
    DateTimeFormatter
      .ofPattern("yyyy-MM-dd HH:mm 'UTC'")
      .withZone(ZoneOffset.UTC)

  private val MaxDiscoverLimit = 10
  private val MaxBrowseLimit = 25
  private val MaxDiscoverWindow = 20
  private val MaxScrollWindow = 50
  private val MaxGetIds = 20

  private def clamp(value: Option[Int], default: Int, max: Int): Int =
    value.getOrElse(default).max(1).min(max)

  private def clampOffset(value: Option[Int]): Int =
    value.getOrElse(0).max(0)

  private def clampWindow(value: Option[Int], default: Int, max: Int): Int =
    value.getOrElse(default).max(0).min(max)

  /** Dispatch to the mode implied by the arguments and render the result JSON. */
  private[tools] def run(
      search: SessionSearch,
      current: SessionId,
      args: Args
  ): String =
    val allOf = args.all_of.getOrElse(Nil)
    val anyOf = args.any_of.getOrElse(Nil)
    val noneOf = args.none_of.getOrElse(Nil)
    val hasPositive = allOf.nonEmpty || anyOf.nonEmpty
    val hasSearch = hasPositive || noneOf.nonEmpty
    val hasScroll =
      args.session_id.isDefined || args.around_message_id.isDefined
    val hasGet = args.message_ids.isDefined
    val json =
      try
        if List(hasSearch, hasScroll, hasGet).count(identity) > 1 then
          err(
            "use one mode at a time: search terms (all_of/any_of/none_of), " +
              "session_id + around_message_id (scroll), or message_ids (get)."
          )
        else if noneOf.nonEmpty && !hasPositive then
          err("none_of requires at least one all_of or any_of term.")
        else if hasSearch then
          args.sort.map(s => s -> SearchSort.parse(s)) match
            case Some((s, None)) =>
              err(s"unknown sort '$s'; use ${SearchSort.names}.")
            case parsed =>
              val sort = parsed.flatMap(_._2).getOrElse(SearchSort.Rank)
              discover(search, current, args, sort)
        else if hasGet then get(search, current, args.message_ids.get)
        else
          (args.session_id, args.around_message_id) match
            case (Some(sid), Some(mid)) =>
              scroll(search, current, sid, mid, args)
            case (Some(_), None) =>
              err("session_id requires around_message_id to scroll.")
            case (None, Some(_)) =>
              err("around_message_id requires session_id to scroll.")
            case (None, None) => browse(search, current, args)
      catch
        case NonFatal(e) =>
          logger.error("session_search failed", e)
          val detail = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
          err(s"search failed: $detail")
    json.render()

  private def discover(
      search: SessionSearch,
      current: SessionId,
      args: Args,
      sort: SearchSort
  ): ujson.Obj =
    val terms = SearchTerms(
      allOf = args.all_of.getOrElse(Nil),
      anyOf = args.any_of.getOrElse(Nil),
      noneOf = args.none_of.getOrElse(Nil),
      prefix = args.prefix.getOrElse(false)
    )
    val offset = clampOffset(args.offset)
    val found = search.discover(
      terms = terms,
      limit = clamp(args.limit, 3, MaxDiscoverLimit),
      offset = offset,
      window = clampWindow(args.window, 5, MaxDiscoverWindow),
      sort = sort,
      excludeSession = Some(current)
    )
    val next = offset + found.hits.size
    val result = ujson.Obj(
      "success" -> true,
      "mode" -> "discover",
      "all_of" -> ujson.Arr.from(terms.allOf),
      "any_of" -> ujson.Arr.from(terms.anyOf),
      "none_of" -> ujson.Arr.from(terms.noneOf),
      "prefix" -> terms.prefix,
      "sort" -> sort.toString.toLowerCase,
      "total_sessions" -> found.totalSessions,
      "offset" -> offset,
      "count" -> found.hits.size,
      "results" -> ujson.Arr.from(found.hits.map(hitJson))
    )
    if found.hits.nonEmpty && next < found.totalSessions then
      result("next_offset") = next
    result

  private def get(
      search: SessionSearch,
      current: SessionId,
      ids: List[Long]
  ): ujson.Obj =
    val distinct = ids.distinct
    if distinct.isEmpty then err("message_ids must not be empty.")
    else if distinct.size > MaxGetIds then
      err(s"at most $MaxGetIds message_ids per call.")
    else
      val found = search.get(distinct, Some(current))
      val foundIds = found.map(_.message.id).toSet
      ujson.Obj(
        "success" -> true,
        "mode" -> "get",
        "count" -> found.size,
        "messages" -> ujson.Arr.from(found.map: m =>
          extend(
            metaJson(m.message),
            "session_id" -> ujson.Str(m.sessionId.toString),
            "text" -> ujson.Str(m.text)
          )),
        "not_found" -> ujson.Arr.from(
          distinct.filterNot(foundIds).map(id => ujson.Num(id.toDouble))
        )
      )

  private def scroll(
      search: SessionSearch,
      current: SessionId,
      sid: String,
      mid: Long,
      args: Args
  ): ujson.Obj =
    val parsed =
      try Some(SessionId(sid))
      catch case _: IllegalArgumentException => None
    val fullText = args.full_text.getOrElse(false)
    parsed match
      case None                => err(s"invalid session id: $sid")
      case Some(_) if mid <= 0 =>
        err("around_message_id must be a positive message id.")
      case Some(sessionId) if sessionId == current =>
        err("cannot scroll the current session; it is already in context.")
      case Some(sessionId) =>
        val window = clampWindow(args.window, 10, MaxScrollWindow)
        search.scroll(sessionId, mid, window, fullText) match
          case None    => err(s"no message $mid found in session $sid")
          case Some(w) =>
            extend(
              sessionJson(w.session),
              "success" -> ujson.True,
              "mode" -> ujson.Str("scroll"),
              "around_message_id" -> ujson.Num(w.aroundMessageId.toDouble),
              "full_text" -> ujson.Bool(fullText),
              "messages" -> ujson.Arr.from(w.messages.map: m =>
                val entry = metaJson(m.message)
                entry("anchor") = m.message.id == w.aroundMessageId
                m.text.foreach(t => entry("text") = t)
                entry)
            )

  private def browse(
      search: SessionSearch,
      current: SessionId,
      args: Args
  ): ujson.Obj =
    val sessions = search.browse(
      limit = clamp(args.limit, 5, MaxBrowseLimit),
      offset = clampOffset(args.offset),
      excludeSession = Some(current)
    )
    ujson.Obj(
      "success" -> true,
      "mode" -> "browse",
      "count" -> sessions.size,
      "results" -> ujson.Arr.from(sessions.map(summaryJson))
    )

  private def hitJson(h: SessionHit): ujson.Obj =
    val matchedIds = h.matches.map(_.message.id).toSet
    extend(
      sessionJson(h.session),
      "matched_messages" -> ujson.Num(h.matchedMessages),
      "matches" -> ujson.Arr.from(h.matches.map: m =>
        extend(
          metaJson(m.message),
          "match_count" -> ujson.Num(m.matchCount),
          "fragments" -> ujson.Arr.from(m.fragments),
          "more_fragments" -> ujson.Num(m.moreFragments)
        )),
      "context" -> ujson.Arr.from(h.context.map: m =>
        extend(metaJson(m), "matched" -> ujson.Bool(matchedIds(m.id))))
    )

  private def sessionJson(s: SessionInfo): ujson.Obj =
    ujson.Obj(
      "session_id" -> s.sessionId.toString,
      "title" -> s.title,
      "workdir" -> s.workdir,
      "started_at" -> timeFmt.format(s.createdAt),
      "last_active" -> timeFmt.format(s.lastActivity)
    )

  private def summaryJson(s: SessionSummary): ujson.Obj =
    ujson.Obj(
      "session_id" -> s.sessionId.toString,
      "title" -> s.title,
      "workdir" -> s.workdir,
      "started_at" -> timeFmt.format(s.createdAt),
      "last_active" -> timeFmt.format(s.lastActivity),
      "message_count" -> s.messageCount
    )

  private def metaJson(m: MessageMeta): ujson.Obj =
    ujson.Obj(
      "id" -> m.id.toDouble,
      "role" -> m.role,
      "when" -> timeFmt.format(m.createdAt),
      "length" -> m.length
    )

  private def extend(
      obj: ujson.Obj,
      fields: (String, ujson.Value)*
  ): ujson.Obj =
    fields.foreach((k, v) => obj(k) = v)
    obj

  private def err(message: String): ujson.Obj =
    ujson.Obj("success" -> false, "error" -> message)
