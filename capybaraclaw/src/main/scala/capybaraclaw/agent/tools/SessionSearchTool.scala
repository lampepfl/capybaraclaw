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
        "Terms that must ALL appear (AND). Set any of allOf/anyOf to search (discover mode). " +
          "Each term is matched literally — a term may be a single word or a multi-word phrase."
      )
      allOf: Option[List[String]] = None,
      @desc("Terms where at least ONE must appear (OR). Matched literally.")
      anyOf: Option[List[String]] = None,
      @desc(
        "Terms that must NOT appear (NOT). Requires at least one allOf/anyOf term. Matched literally."
      )
      noneOf: Option[List[String]] = None,
      @desc(
        "Prefix-match the positive (allOf/anyOf) terms, e.g. 'deploy' also matches 'deployment'. Exclusions stay exact."
      )
      prefix: Option[Boolean] = None,
      @desc("Session UUID to scroll within. Requires around_message_id.")
      sessionId: Option[String] = None,
      @desc("Message id to center the scroll window on. Requires sessionId.")
      aroundMessageId: Option[Int] = None,
      @desc("Max results (discover/browse). Defaults: discover 3, browse 5.")
      limit: Option[Int] = None,
      @desc(
        "Number of results to skip, for paging (discover/browse). Default 0."
      )
      offset: Option[Int] = None,
      @desc(
        "Context messages on each side of a match. Discover default 5, scroll default 10."
      )
      window: Option[Int] = None,
      @desc("Sort for discover: 'rank' (default), 'newest', or 'oldest'.")
      sort: Option[String] = None
  ) derives IsToolArg

  val name: String = "session_search"
  val description: String =
    "Search your own past sessions (all projects) by full text. " +
      "Three modes, inferred from arguments: " +
      "set 'allOf'/'anyOf'/'noneOf' to find sessions matching terms (discover) — returns one hit per " +
      "session with a highlighted snippet and surrounding messages; " +
      "set 'sessionId' + 'aroundMessageId' to page through a session's messages (scroll); " +
      "pass no arguments to list your most recent sessions (browse). " +
      "The current session is always excluded. Terms are matched literally — you do NOT write query " +
      "syntax; combine them via the allOf (AND), anyOf (OR) and noneOf (NOT) lists."

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

  private val MaxLimit = 25
  private val MaxWindow = 50

  private def clampLimit(value: Option[Int], default: Int): Int =
    value.getOrElse(default).max(1).min(MaxLimit)

  private def clampOffset(value: Option[Int]): Int =
    value.getOrElse(0).max(0)

  private def clampWindow(value: Option[Int], default: Int): Int =
    value.getOrElse(default).max(0).min(MaxWindow)

  /** Dispatch to the mode implied by the arguments and render the result JSON. */
  private[tools] def run(
      search: SessionSearch,
      current: SessionId,
      args: Args
  ): String =
    val allOf = args.allOf.getOrElse(Nil)
    val anyOf = args.anyOf.getOrElse(Nil)
    val noneOf = args.noneOf.getOrElse(Nil)
    val hasPositive = allOf.nonEmpty || anyOf.nonEmpty
    val hasSearch = hasPositive || noneOf.nonEmpty
    val hasScrollArgs =
      args.sessionId.isDefined || args.aroundMessageId.isDefined
    val json =
      try
        if hasSearch && hasScrollArgs then
          err(
            "provide either search terms (discover) or 'sessionId'+'around_message_id' (scroll), not both."
          )
        else if noneOf.nonEmpty && !hasPositive then
          err("noneOf requires at least one allOf or anyOf term.")
        else if hasSearch then discover(search, current, args)
        else
          (args.sessionId, args.aroundMessageId) match
            case (Some(sid), Some(mid)) =>
              scroll(search, current, sid, mid, args)
            case (Some(_), None) =>
              err("sessionId requires around_message_id to scroll.")
            case _ => browse(search, current, args)
      catch
        case NonFatal(e) =>
          logger.error("session_search failed", e)
          val detail = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
          err(s"search failed: $detail")
    json.render()

  private def discover(
      search: SessionSearch,
      current: SessionId,
      args: Args
  ): ujson.Obj =
    val terms = SearchTerms(
      allOf = args.allOf.getOrElse(Nil),
      anyOf = args.anyOf.getOrElse(Nil),
      noneOf = args.noneOf.getOrElse(Nil),
      prefix = args.prefix.getOrElse(false)
    )
    val hits = search.discover(
      terms = terms,
      limit = clampLimit(args.limit, 3),
      offset = clampOffset(args.offset),
      window = clampWindow(args.window, 5),
      sort = args.sort.map(SearchSort.fromString).getOrElse(SearchSort.Rank),
      excludeSession = Some(current)
    )
    ujson.Obj(
      "success" -> true,
      "mode" -> "discover",
      "all_of" -> ujson.Arr.from(terms.allOf),
      "any_of" -> ujson.Arr.from(terms.anyOf),
      "none_of" -> ujson.Arr.from(terms.noneOf),
      "prefix" -> terms.prefix,
      "count" -> hits.size,
      "results" -> ujson.Arr.from(hits.map(hitJson))
    )

  private def scroll(
      search: SessionSearch,
      current: SessionId,
      sid: String,
      mid: Int,
      args: Args
  ): ujson.Obj =
    val parsed =
      try Some(SessionId(sid))
      catch case _: IllegalArgumentException => None
    parsed match
      case None                => err(s"invalid session id: $sid")
      case Some(_) if mid <= 0 =>
        err("around_message_id must be a positive message id.")
      case Some(sessionId) if sessionId == current =>
        err("cannot scroll the current session; it is already in context.")
      case Some(sessionId) =>
        search.scroll(sessionId, mid.toLong, clampWindow(args.window, 10)) match
          case None    => err(s"no message $mid found in session $sid")
          case Some(w) =>
            ujson.Obj(
              "success" -> true,
              "mode" -> "scroll",
              "session_id" -> w.sessionId.toString,
              "title" -> w.title,
              "workdir" -> w.workdir,
              "started_at" -> timeFmt.format(w.sessionCreatedAt),
              "last_active" -> timeFmt.format(w.lastActivity),
              "last_active_epoch_ms" -> w.lastActivity.toEpochMilli.toDouble,
              "around_message_id" -> w.aroundMessageId.toDouble,
              "messages" -> ujson.Arr.from(w.window.map(entryJson))
            )

  private def browse(
      search: SessionSearch,
      current: SessionId,
      args: Args
  ): ujson.Obj =
    val sessions = search.browse(
      limit = clampLimit(args.limit, 5),
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
    ujson.Obj(
      "session_id" -> h.sessionId.toString,
      "title" -> h.title,
      "workdir" -> h.workdir,
      "started_at" -> timeFmt.format(h.sessionCreatedAt),
      "last_active" -> timeFmt.format(h.lastActivity),
      "last_active_epoch_ms" -> h.lastActivity.toEpochMilli.toDouble,
      "match_message_id" -> h.matchMessageId.toDouble,
      "matched_role" -> h.matchedRole,
      "snippet" -> h.snippet,
      "window" -> ujson.Arr.from(h.window.map(entryJson)),
      "bookend_start" -> ujson.Arr.from(h.bookendStart.map(entryJson)),
      "bookend_end" -> ujson.Arr.from(h.bookendEnd.map(entryJson))
    )

  private def summaryJson(s: SessionSummary): ujson.Obj =
    ujson.Obj(
      "session_id" -> s.sessionId.toString,
      "title" -> s.title,
      "workdir" -> s.workdir,
      "started_at" -> timeFmt.format(s.createdAt),
      "last_active" -> timeFmt.format(s.lastActivity),
      "last_active_epoch_ms" -> s.lastActivity.toEpochMilli.toDouble,
      "message_count" -> s.messageCount
    )

  private def entryJson(e: MessageEntry): ujson.Obj =
    ujson.Obj(
      "id" -> e.id.toDouble,
      "role" -> e.role,
      "text" -> e.text,
      "when_epoch_ms" -> e.createdAt.toEpochMilli.toDouble,
      "anchor" -> e.anchor
    )

  private def err(message: String): ujson.Obj =
    ujson.Obj("success" -> false, "error" -> message)
