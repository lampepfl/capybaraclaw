package capybaraclaw.gateway.sqlite

import capybaraclaw.gateway.*
import java.sql.{Connection, ResultSet}
import java.time.Instant

private[sqlite] object SessionSearchQueries:

  private val BookendSize = 3

  private def orderClause(sort: SearchSort): String = sort match
    case SearchSort.Rank   => "r.rank ASC, r.msg_id ASC"
    case SearchSort.Newest => "r.msg_id DESC"
    case SearchSort.Oldest => "r.msg_id ASC"

  def discover(
      conn: Connection,
      matchExpr: String,
      limit: Int,
      offset: Int,
      window: Int,
      sort: SearchSort,
      excludeSession: Option[SessionId]
  ): List[SessionHit] =
    if matchExpr.isBlank then Nil
    else
      val sql =
        s"""WITH matched AS (
           |  SELECT m.id AS msg_id, m.session_id, m.role,
           |         bm25(messages_fts) AS rank,
           |         snippet(messages_fts, 0, '«', '»', '…', 12) AS snippet
           |  FROM messages_fts
           |  JOIN messages m ON m.id = messages_fts.rowid
           |  WHERE messages_fts MATCH ?
           |    AND m.session_id != ?
           |),
           |ranked AS (
           |  SELECT msg_id, session_id, role, rank, snippet,
           |         ROW_NUMBER() OVER (
           |           PARTITION BY session_id
           |           ORDER BY rank ASC, msg_id ASC
           |         ) AS rn
           |  FROM matched
           |)
           |SELECT r.msg_id, r.session_id, r.role, r.snippet,
           |       s.workdir, s.created_at AS s_created, s.last_activity,
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
        stmt.setString(2, excludeSession.getOrElse(""))
        stmt.setInt(3, limit)
        stmt.setInt(4, offset)
        SqliteJdbc.withResultSet(stmt.executeQuery())(readAnchors)
      anchors.map: a =>
        val win = windowAround(conn, a.sessionId, a.matchMessageId, window)
        val windowIds = win.map(_.id).toSet
        val notInWindow = (e: MessageEntry) => !windowIds.contains(e.id)
        SessionHit(
          sessionId = a.sessionId,
          workdir = a.workdir,
          title = deriveTitle(a.title),
          sessionCreatedAt = Instant.ofEpochMilli(a.sessionCreated),
          lastActivity = Instant.ofEpochMilli(a.lastActivity),
          matchMessageId = a.matchMessageId,
          matchedRole = a.role,
          snippet = a.snippet,
          window = win,
          bookendStart =
            bookend(conn, a.sessionId, ascending = true).filter(notInWindow),
          bookendEnd =
            bookend(conn, a.sessionId, ascending = false).filter(notInWindow)
        )

  def scroll(
      conn: Connection,
      sessionId: SessionId,
      aroundMessageId: Long,
      window: Int
  ): Option[SessionWindow] =
    // Require both the session and the anchor message to exist, so a caller
    // cannot page an arbitrary id that yields a window with no real anchor.
    sessionRow(conn, sessionId)
      .filter(_ => messageExists(conn, sessionId, aroundMessageId))
      .map: s =>
        val win = windowAround(conn, sessionId, aroundMessageId, window)
        SessionWindow(
          sessionId = sessionId,
          workdir = s.workdir,
          title = deriveTitle(firstUserText(conn, sessionId)),
          sessionCreatedAt = Instant.ofEpochMilli(s.created),
          lastActivity = Instant.ofEpochMilli(s.lastActivity),
          aroundMessageId = aroundMessageId,
          window = win
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
      stmt.setString(1, excludeSession.getOrElse(""))
      stmt.setInt(2, limit)
      stmt.setInt(3, offset)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        Iterator
          .continually(rs.next())
          .takeWhile(identity)
          .map: _ =>
            SessionSummary(
              sessionId = SessionId(rs.getString("id")),
              workdir = rs.getString("workdir"),
              title = deriveTitle(Option(rs.getString("title"))),
              createdAt = Instant.ofEpochMilli(rs.getLong("created_at")),
              lastActivity = Instant.ofEpochMilli(rs.getLong("last_activity")),
              messageCount = rs.getInt("msg_count")
            )
          .toList

  private final case class SessionRow(
      workdir: String,
      created: Long,
      lastActivity: Long
  )

  private def sessionRow(
      conn: Connection,
      sessionId: SessionId
  ): Option[SessionRow] =
    val sql =
      "SELECT workdir, created_at, last_activity FROM sessions WHERE id = ?"
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, sessionId)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        if rs.next() then
          Some(
            SessionRow(
              rs.getString("workdir"),
              rs.getLong("created_at"),
              rs.getLong("last_activity")
            )
          )
        else None

  private def firstUserText(
      conn: Connection,
      sessionId: SessionId
  ): Option[String] =
    val sql =
      "SELECT text FROM messages WHERE session_id = ? AND role = 'user' ORDER BY id ASC LIMIT 1"
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, sessionId)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        if rs.next() then Option(rs.getString("text")) else None

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
      matchMessageId: Long,
      sessionId: SessionId,
      role: String,
      snippet: String,
      workdir: String,
      sessionCreated: Long,
      lastActivity: Long,
      title: Option[String]
  )

  private def readAnchors(rs: ResultSet): List[AnchorRow] =
    Iterator
      .continually(rs.next())
      .takeWhile(identity)
      .map: _ =>
        AnchorRow(
          matchMessageId = rs.getLong("msg_id"),
          sessionId = SessionId(rs.getString("session_id")),
          role = rs.getString("role"),
          snippet = rs.getString("snippet"),
          workdir = rs.getString("workdir"),
          sessionCreated = rs.getLong("s_created"),
          lastActivity = rs.getLong("last_activity"),
          title = Option(rs.getString("title"))
        )
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

  def windowAround(
      conn: Connection,
      sessionId: SessionId,
      anchorId: Long,
      window: Int
  ): List[MessageEntry] =
    val before = selectMessages(
      conn,
      "SELECT id, role, text, created_at FROM messages WHERE session_id = ? AND id <= ? ORDER BY id DESC LIMIT ?",
      sessionId,
      Some(anchorId),
      window + 1
    ).reverse
    val after = selectMessages(
      conn,
      "SELECT id, role, text, created_at FROM messages WHERE session_id = ? AND id > ? ORDER BY id ASC LIMIT ?",
      sessionId,
      Some(anchorId),
      window
    )
    (before ++ after).map(e => e.copy(anchor = e.id == anchorId))

  private def bookend(
      conn: Connection,
      sessionId: SessionId,
      ascending: Boolean
  ): List[MessageEntry] =
    val dir = if ascending then "ASC" else "DESC"
    val rows = selectMessages(
      conn,
      s"SELECT id, role, text, created_at FROM messages WHERE session_id = ? ORDER BY id $dir LIMIT ?",
      sessionId,
      None,
      BookendSize
    )
    if ascending then rows else rows.reverse

  private def selectMessages(
      conn: Connection,
      sql: String,
      sessionId: SessionId,
      boundaryId: Option[Long],
      limit: Int
  ): List[MessageEntry] =
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, sessionId)
      boundaryId match
        case Some(id) =>
          stmt.setLong(2, id)
          stmt.setInt(3, limit)
        case None =>
          stmt.setInt(2, limit)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        Iterator
          .continually(rs.next())
          .takeWhile(identity)
          .map: _ =>
            MessageEntry(
              id = rs.getLong("id"),
              role = rs.getString("role"),
              text = rs.getString("text"),
              createdAt = Instant.ofEpochMilli(rs.getLong("created_at"))
            )
          .toList
