package capybaraclaw.gateway.sqlite

import capybaraclaw.gateway.*
import java.sql.{Connection, ResultSet}
import java.time.Instant

private[sqlite] object SessionSearchQueries:
  import SessionSearch.*

  /** Rank by the best match; newest and oldest by the session's latest and
    * earliest match.
    */
  private def orderClause(sort: SearchSort): String = sort match
    case SearchSort.Rank   => "r.rank ASC, r.msg_id ASC"
    case SearchSort.Newest => "r.newest_id DESC"
    case SearchSort.Oldest => "r.oldest_id ASC"

  def discover(
      conn: Connection,
      matchExpr: String,
      limit: Int,
      offset: Int,
      window: Int,
      sort: SearchSort,
      excludeSession: Option[SessionId]
  ): Discovery =
    val excluded = excludeSession.map(_.toString).getOrElse("")
    val countSql =
      """SELECT COUNT(DISTINCT m.session_id)
        |FROM messages_fts
        |JOIN messages m ON m.id = messages_fts.rowid
        |WHERE messages_fts MATCH ? AND m.session_id != ?""".stripMargin
    val total = SqliteJdbc.withStatement(conn, countSql): stmt =>
      stmt.setString(1, matchExpr)
      stmt.setString(2, excluded)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        if rs.next() then rs.getInt(1) else 0
    val sql =
      s"""WITH matched AS (
         |  SELECT m.id AS msg_id, m.session_id, bm25(messages_fts) AS rank
         |  FROM messages_fts
         |  JOIN messages m ON m.id = messages_fts.rowid
         |  WHERE messages_fts MATCH ? AND m.session_id != ?
         |),
         |ranked AS (
         |  SELECT msg_id, session_id, rank,
         |         ROW_NUMBER() OVER (
         |           PARTITION BY session_id ORDER BY rank ASC, msg_id ASC
         |         ) AS rn,
         |         COUNT(*) OVER (PARTITION BY session_id) AS matched_count,
         |         MAX(msg_id) OVER (PARTITION BY session_id) AS newest_id,
         |         MIN(msg_id) OVER (PARTITION BY session_id) AS oldest_id
         |  FROM matched
         |)
         |SELECT r.msg_id, r.session_id, r.matched_count,
         |       s.workdir, s.created_at, s.last_activity,
         |       (SELECT text FROM messages fm
         |        WHERE fm.session_id = r.session_id AND fm.role = 'user'
         |        ORDER BY id ASC LIMIT 1) AS title
         |FROM ranked r
         |JOIN sessions s ON s.id = r.session_id
         |WHERE r.rn = 1
         |ORDER BY ${orderClause(sort)}
         |LIMIT ? OFFSET ?""".stripMargin
    val anchors = SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, matchExpr)
      stmt.setString(2, excluded)
      stmt.setInt(3, limit)
      stmt.setInt(4, offset)
      SqliteJdbc.withResultSet(stmt.executeQuery())(readAnchors)
    val hits = anchors.map: a =>
      SessionHit(
        session = a.session,
        matchedMessages = a.matchedCount,
        matches = matchesIn(conn, matchExpr, a.session.sessionId),
        context = metaAround(conn, a.session.sessionId, a.bestMessageId, window)
      )
    Discovery(total, hits)

  def scroll(
      conn: Connection,
      sessionId: SessionId,
      aroundMessageId: Long,
      window: Int,
      fullText: Boolean
  ): Option[SessionWindow] =
    // Require both the session and the anchor message to exist, so a caller
    // cannot page an arbitrary id that yields a window with no real anchor.
    sessionInfo(conn, sessionId)
      .filter(_ => messageExists(conn, sessionId, aroundMessageId))
      .map: info =>
        val messages =
          around(conn, sessionId, aroundMessageId, window, withText = true)
            .map: (meta, text) =>
              WindowMessage(meta, Option.when(fullText)(text))
        SessionWindow(info, aroundMessageId, messages)

  def get(
      conn: Connection,
      ids: List[Long],
      excludeSession: Option[SessionId]
  ): List[FullMessage] =
    if ids.isEmpty then Nil
    else
      val sql =
        s"""SELECT id, session_id, role, created_at, text FROM messages
           |WHERE id IN (${ids.map(_ => "?").mkString(", ")})
           |  AND session_id != ?
           |ORDER BY id ASC""".stripMargin
      SqliteJdbc.withStatement(conn, sql): stmt =>
        ids.zipWithIndex.foreach((id, i) => stmt.setLong(i + 1, id))
        stmt.setString(
          ids.size + 1,
          excludeSession.map(_.toString).getOrElse("")
        )
        SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
          rows(rs): rs =>
            val text = rs.getString("text")
            FullMessage(
              SessionId(rs.getString("session_id")),
              meta(rs, text),
              text
            )

  def browse(
      conn: Connection,
      limit: Int,
      offset: Int,
      excludeSession: Option[SessionId]
  ): List[SessionSummary] =
    val sql =
      """SELECT s.id, s.workdir, s.created_at, s.last_activity,
        |       (SELECT COUNT(*) FROM messages m WHERE m.session_id = s.id) AS msg_count,
        |       (SELECT text FROM messages m
        |        WHERE m.session_id = s.id AND m.role = 'user'
        |        ORDER BY id ASC LIMIT 1) AS title
        |FROM sessions s
        |WHERE s.id != ?
        |ORDER BY s.last_activity DESC
        |LIMIT ? OFFSET ?""".stripMargin
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, excludeSession.map(_.toString).getOrElse(""))
      stmt.setInt(2, limit)
      stmt.setInt(3, offset)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        rows(rs): rs =>
          SessionSummary(
            sessionId = SessionId(rs.getString("id")),
            workdir = rs.getString("workdir"),
            title = deriveTitle(Option(rs.getString("title"))),
            createdAt = Instant.ofEpochMilli(rs.getLong("created_at")),
            lastActivity = Instant.ofEpochMilli(rs.getLong("last_activity")),
            messageCount = rs.getInt("msg_count")
          )

  /** The best-ranked matched messages of a session, in id order. */
  private def matchesIn(
      conn: Connection,
      matchExpr: String,
      sessionId: SessionId
  ): List[MessageMatch] =
    val sql =
      """SELECT m.id, m.role, m.created_at, m.text,
        |       highlight(messages_fts, 0, ?, ?) AS highlighted
        |FROM messages_fts
        |JOIN messages m ON m.id = messages_fts.rowid
        |WHERE messages_fts MATCH ? AND m.session_id = ?
        |ORDER BY bm25(messages_fts) ASC, m.id ASC
        |LIMIT ?""".stripMargin
    SqliteJdbc
      .withStatement(conn, sql): stmt =>
        stmt.setString(1, Fragments.Open.toString)
        stmt.setString(2, Fragments.Close.toString)
        stmt.setString(3, matchExpr)
        stmt.setString(4, sessionId)
        stmt.setInt(5, MatchesPerSession)
        SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
          rows(rs): rs =>
            val text = rs.getString("text")
            val ranges =
              Fragments.ranges(text, rs.getString("highlighted")).getOrElse(Nil)
            val fragments =
              Fragments.render(text, ranges, FragmentRadius, BoundarySlack)
            MessageMatch(
              message = meta(rs, text),
              matchCount = ranges.size,
              fragments = fragments.take(FragmentsPerMessage),
              moreFragments = math.max(0, fragments.size - FragmentsPerMessage)
            )
      .sortBy(_.message.id)

  private def metaAround(
      conn: Connection,
      sessionId: SessionId,
      anchorId: Long,
      window: Int
  ): List[MessageMeta] =
    around(conn, sessionId, anchorId, window, withText = false).map(_._1)

  /** Up to `window` messages on each side of `anchorId`, and the anchor. */
  private def around(
      conn: Connection,
      sessionId: SessionId,
      anchorId: Long,
      window: Int,
      withText: Boolean
  ): List[(MessageMeta, String)] =
    val text = if withText then "text" else "'' AS text"
    def select(sql: String, limit: Int) =
      SqliteJdbc.withStatement(conn, sql): stmt =>
        stmt.setString(1, sessionId)
        stmt.setLong(2, anchorId)
        stmt.setInt(3, limit)
        SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
          rows(rs): rs =>
            val t = rs.getString("text")
            val m = MessageMeta(
              rs.getLong("id"),
              rs.getString("role"),
              Instant.ofEpochMilli(rs.getLong("created_at")),
              rs.getInt("len")
            )
            (m, t)
    val columns = s"id, role, created_at, length(text) AS len, $text"
    val before = select(
      s"SELECT $columns FROM messages WHERE session_id = ? AND id <= ? ORDER BY id DESC LIMIT ?",
      window + 1
    ).reverse
    val after = select(
      s"SELECT $columns FROM messages WHERE session_id = ? AND id > ? ORDER BY id ASC LIMIT ?",
      window
    )
    before ++ after

  private def sessionInfo(
      conn: Connection,
      sessionId: SessionId
  ): Option[SessionInfo] =
    val sql =
      """SELECT s.workdir, s.created_at, s.last_activity,
        |       (SELECT text FROM messages m
        |        WHERE m.session_id = s.id AND m.role = 'user'
        |        ORDER BY id ASC LIMIT 1) AS title
        |FROM sessions s WHERE s.id = ?""".stripMargin
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, sessionId)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        Option.when(rs.next())(info(rs, sessionId))

  private def messageExists(
      conn: Connection,
      sessionId: SessionId,
      messageId: Long
  ): Boolean =
    val sql = "SELECT 1 FROM messages WHERE session_id = ? AND id = ? LIMIT 1"
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, sessionId)
      stmt.setLong(2, messageId)
      SqliteJdbc.withResultSet(stmt.executeQuery())(_.next())

  private final case class AnchorRow(
      session: SessionInfo,
      bestMessageId: Long,
      matchedCount: Int
  )

  private def readAnchors(rs: ResultSet): List[AnchorRow] =
    rows(rs): rs =>
      AnchorRow(
        session = info(rs, SessionId(rs.getString("session_id"))),
        bestMessageId = rs.getLong("msg_id"),
        matchedCount = rs.getInt("matched_count")
      )

  /** Reads `workdir`, `created_at`, `last_activity` and `title`. */
  private def info(rs: ResultSet, sessionId: SessionId): SessionInfo =
    SessionInfo(
      sessionId = sessionId,
      workdir = rs.getString("workdir"),
      title = deriveTitle(Option(rs.getString("title"))),
      createdAt = Instant.ofEpochMilli(rs.getLong("created_at")),
      lastActivity = Instant.ofEpochMilli(rs.getLong("last_activity"))
    )

  private def meta(rs: ResultSet, text: String): MessageMeta =
    MessageMeta(
      rs.getLong("id"),
      rs.getString("role"),
      Instant.ofEpochMilli(rs.getLong("created_at")),
      text.codePointCount(0, text.length)
    )

  private def rows[A](rs: ResultSet)(read: ResultSet => A): List[A] =
    Iterator
      .continually(rs.next())
      .takeWhile(identity)
      .map(_ => read(rs))
      .toList

  // TODO: replace this derivation with an LLM-generated title
  def deriveTitle(firstUserText: Option[String]): String =
    firstUserText
      .map(_.linesIterator.nextOption().getOrElse(""))
      .map(stripUserTag)
      .map(_.take(80).trim)
      .filter(_.nonEmpty)
      .getOrElse("(untitled session)")

  private val userTagPrefix = """^\[[^\]]*\]\s+""".r
  private def stripUserTag(line: String): String =
    userTagPrefix.replaceFirstIn(line, "")
