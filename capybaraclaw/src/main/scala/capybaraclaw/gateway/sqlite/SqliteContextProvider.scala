package capybaraclaw.gateway.sqlite

import capybaraclaw.gateway.{
  ContextProvider,
  SessionHandle,
  SessionId,
  SessionMetadata
}
import org.flywaydb.core.Flyway
import org.sqlite.{SQLiteErrorCode, SQLiteException}
import tacit.agents.llm.endpoint.{Content, Message, Role}

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.sql.{Connection, DriverManager}
import java.time.Instant
import scala.util.Using

/** SQLite-backed transcript store keyed by UUID `SessionId`.
  *
  * `~/.claw/state.db` is the single, global source of truth for messages and
  * session metadata. Schema is versioned by Flyway; see `db/migration/V*.sql`
  * resources.
  *
  * Concurrency model: one persistent writer connection + a small pool of
  * read-only connections.
  */
class SqliteContextProvider(
    baseDir: Path = SqliteContextProvider.defaultBaseDir,
    nowMillis: () => Long = () => Instant.now.toEpochMilli
) extends ContextProvider
    with AutoCloseable:

  private val dbFile = baseDir.resolve("state.db").toFile
  private val writeLock = Object()

  runMigrations()
  private val writer: Connection = openWriter()
  private val readers: SqliteReaderPool =
    try SqliteReaderPool(dbFile, size = 2)
    catch
      case e: Throwable =>
        bestEffort:
          writer.close()
        throw e

  def createSession(workdir: String): SessionId =
    writeLock.synchronized:
      val sessionId = SessionId.random()
      val now = nowMillis()
      inTransaction:
        insertSession(writer, sessionId, workdir, now)
        sessionId

  def verifyAndTouchSession(
      id: SessionId,
      expectedWorkdir: String
  ): Option[SessionMetadata] =
    writeLock.synchronized:
      inTransaction:
        selectSession(writer, id).map: metadata =>
          if metadata.workdir == expectedWorkdir then
            updateLastActivity(writer, id, nowMillis())
          metadata

  def resolveOrCreateHandle(
      workdir: String,
      handle: SessionHandle
  ): SessionId =
    writeLock.synchronized:
      selectHandle(writer, workdir, handle) match
        case Some(sessionId) =>
          inTransaction:
            updateLastActivity(writer, sessionId, nowMillis())
          sessionId
        case None =>
          try
            inTransaction:
              val sessionId = SessionId.random()
              val now = nowMillis()
              insertSession(writer, sessionId, workdir, now)
              insertHandle(writer, sessionId, workdir, handle)
              sessionId
          catch
            case e: SQLiteException if isHandleConflict(e) =>
              selectHandle(writer, workdir, handle) match
                case Some(sessionId) =>
                  inTransaction:
                    updateLastActivity(writer, sessionId, nowMillis())
                  sessionId
                case None => throw e

  def findSession(id: SessionId): Option[SessionMetadata] =
    readers.withReader: reader =>
      selectSession(reader, id)

  def listSessions(): List[SessionMetadata] =
    readers.withReader: reader =>
      selectAllSessions(reader)

  def load(sessionId: SessionId): List[Message] =
    readers.withReader: reader =>
      selectMessages(reader, sessionId)

  def append(sessionId: SessionId, msg: Message): Unit =
    persistableText(msg).foreach: (role, text) =>
      writeLock.synchronized:
        inTransaction:
          insertMessage(writer, sessionId, role, text)

  def close(): Unit =
    writeLock.synchronized:
      readers.close()
      if !writer.isClosed then writer.close()

  private def runMigrations(): Unit =
    Files.createDirectories(dbFile.toPath.getParent)
    val _ = Flyway
      .configure()
      .dataSource(s"jdbc:sqlite:${dbFile.getPath}", "", "")
      .locations("classpath:db/migration")
      .load()
      .migrate()

  private def openWriter(): Connection =
    val c = DriverManager.getConnection(s"jdbc:sqlite:${dbFile.getPath}")
    try
      SqliteJdbc.execute(c, "PRAGMA journal_mode = WAL")
      SqliteJdbc.execute(c, "PRAGMA busy_timeout = 5000")
      SqliteJdbc.execute(c, "PRAGMA foreign_keys = ON")
      SqliteJdbc.execute(c, "PRAGMA journal_size_limit = 67108864") /* 64 MiB */
      c
    catch
      case e: Throwable =>
        bestEffort:
          c.close()
        throw e

  private def selectMessages(
      conn: Connection,
      sessionId: SessionId
  ): List[Message] =
    val sql =
      """SELECT role, text
        |FROM messages
        |WHERE session_id = ?
        |ORDER BY id ASC""".stripMargin
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, sessionId)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        Iterator
          .continually(rs.next())
          .takeWhile(identity)
          .flatMap: _ =>
            rs.getString("role") match
              case "user"      => Some(Message.user(rs.getString("text")))
              case "assistant" => Some(Message.assistant(rs.getString("text")))
              case _           => None
          .toList

  private def insertMessage(
      conn: Connection,
      sessionId: SessionId,
      role: String,
      text: String
  ): Unit =
    val sql =
      "INSERT INTO messages(session_id, role, text) VALUES (?, ?, ?)"
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, sessionId)
      stmt.setString(2, role)
      stmt.setString(3, text)
      stmt.executeUpdate()
      ()

  private def insertSession(
      conn: Connection,
      sessionId: SessionId,
      workdir: String,
      nowEpochMillis: Long
  ): Unit =
    val sql =
      """INSERT INTO sessions(
        |  id, workdir, created_at, last_activity
        |) VALUES (?, ?, ?, ?)""".stripMargin
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, sessionId)
      stmt.setString(2, workdir)
      stmt.setLong(3, nowEpochMillis)
      stmt.setLong(4, nowEpochMillis)
      stmt.executeUpdate()
      ()

  private def insertHandle(
      conn: Connection,
      sessionId: SessionId,
      workdir: String,
      handle: SessionHandle
  ): Unit =
    val sql =
      """INSERT INTO session_handles(session_id, workdir, kind, value)
        |VALUES (?, ?, ?, ?)""".stripMargin
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, sessionId)
      stmt.setString(2, workdir)
      stmt.setString(3, handle.kind)
      stmt.setString(4, handle.value)
      stmt.executeUpdate()
      ()

  private def updateLastActivity(
      conn: Connection,
      sessionId: SessionId,
      nowEpochMillis: Long
  ): Int =
    val sql = "UPDATE sessions SET last_activity = ? WHERE id = ?"
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setLong(1, nowEpochMillis)
      stmt.setString(2, sessionId)
      stmt.executeUpdate()

  private def selectHandle(
      conn: Connection,
      workdir: String,
      handle: SessionHandle
  ): Option[SessionId] =
    val sql =
      """SELECT session_id
        |FROM session_handles
        |WHERE workdir = ? AND kind = ? AND value = ?""".stripMargin
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, workdir)
      stmt.setString(2, handle.kind)
      stmt.setString(3, handle.value)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        if rs.next() then
          val raw = rs.getString("session_id")
          Some(
            parseSessionId(
              raw,
              s"session_handles.session_id for workdir='$workdir', kind='${handle.kind}', value='${handle.value}'"
            )
          )
        else None

  private def selectSession(
      conn: Connection,
      sessionId: SessionId
  ): Option[SessionMetadata] =
    val sql =
      """SELECT id, workdir, created_at, last_activity
        |FROM sessions
        |WHERE id = ?""".stripMargin
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, sessionId)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        if rs.next() then Some(sessionMetadataFromRow(rs)) else None

  private def selectAllSessions(conn: Connection): List[SessionMetadata] =
    val sql =
      """SELECT id, workdir, created_at, last_activity
        |FROM sessions
        |ORDER BY last_activity DESC""".stripMargin
    SqliteJdbc.withStatement(conn, sql): stmt =>
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        Iterator
          .continually(rs.next())
          .takeWhile(identity)
          .map(_ => sessionMetadataFromRow(rs))
          .toList

  private def sessionMetadataFromRow(rs: java.sql.ResultSet): SessionMetadata =
    val rawId = rs.getString("id")
    SessionMetadata(
      parseSessionId(rawId, s"sessions.id (looked up by '$rawId')"),
      rs.getString("workdir"),
      Instant.ofEpochMilli(rs.getLong("created_at")),
      Instant.ofEpochMilli(rs.getLong("last_activity"))
    )

  private def parseSessionId(raw: String, context: String): SessionId =
    try SessionId(raw)
    catch
      case e: IllegalArgumentException =>
        throw IllegalStateException(
          s"corrupt session id in $context: '$raw' is not a valid UUID",
          e
        )

  private def isHandleConflict(e: SQLiteException): Boolean =
    e.getResultCode == SQLiteErrorCode.SQLITE_CONSTRAINT_UNIQUE

  private def persistableText(msg: Message): Option[(String, String)] =
    val role = msg.role match
      case Role.User      => Some("user")
      case Role.Assistant => Some("assistant")
      case Role.System    => None
    val text = msg.content.collect { case Content.Text(t) => t }.mkString
    role.filter(_ => text.nonEmpty).map(_ -> text)

  private def inTransaction[A](body: => A): A =
    Using.resource(WriteTransaction(writer)): tx =>
      val result = body
      tx.commit()
      result
  private final class WriteTransaction(conn: Connection) extends AutoCloseable:
    private val previousAutoCommit = conn.getAutoCommit
    conn.setAutoCommit(false)

    def commit(): Unit =
      conn.commit()
      conn.setAutoCommit(previousAutoCommit)

    override def close(): Unit =
      if !conn.getAutoCommit then
        bestEffort:
          conn.rollback()
        bestEffort:
          conn.setAutoCommit(previousAutoCommit)

object SqliteContextProvider:
  /** `~/.claw` */
  def defaultBaseDir: Path =
    Paths.get(System.getProperty("user.home"), ".claw")
