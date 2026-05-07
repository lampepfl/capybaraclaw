package capybaraclaw.gateway.sqlite

import capybaraclaw.gateway.{ContextKey, ContextProvider}
import org.flywaydb.core.Flyway
import tacit.agents.llm.endpoint.{Content, Message, Role}

import java.io.File
import java.nio.file.Files
import java.sql.{Connection, DriverManager}
import scala.util.Using

/** SQLite-backed session store.
  *
  * `.claw/state.db` is the single source of truth. Schema is versioned by
  * Flyway (`flyway_schema_history`); see `db/migration/V*.sql` resources.
  *
  * Concurrency model: one persistent writer connection + a small pool of
  * read-only connections.
  */
class SqliteContextProvider(baseDir: File)
    extends ContextProvider
    with AutoCloseable:

  private val dbFile = File(File(baseDir, ".claw"), "state.db")
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

  def load(key: ContextKey): List[Message] =
    readers.withReader: reader =>
      findSession(reader, key) match
        case None            => Nil
        case Some(sessionId) => selectMessages(reader, sessionId)

  def append(key: ContextKey, msg: Message): Unit =
    persistableText(msg).foreach: (role, text) =>
      writeLock.synchronized:
        inTransaction:
          val sessionId = findOrCreateSession(writer, key)
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

  private def findSession(conn: Connection, key: ContextKey): Option[Long] =
    SqliteJdbc.withStatement(
      conn,
      "SELECT id FROM sessions WHERE port = ? AND thread = ?"
    ): stmt =>
      stmt.setString(1, key.port)
      stmt.setString(2, key.thread)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        if rs.next() then Some(rs.getLong("id")) else None

  private def findOrCreateSession(conn: Connection, key: ContextKey): Long =
    val sql =
      """INSERT INTO sessions(port, thread) VALUES (?, ?)
        |ON CONFLICT(port, thread) DO UPDATE SET port = sessions.port
        |RETURNING id""".stripMargin
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setString(1, key.port)
      stmt.setString(2, key.thread)
      SqliteJdbc.withResultSet(stmt.executeQuery()): rs =>
        if rs.next() then rs.getLong("id")
        else throw IllegalStateException("upsert returned no session id")

  private def selectMessages(
      conn: Connection,
      sessionId: Long
  ): List[Message] =
    val sql =
      """SELECT role, text
        |FROM messages
        |WHERE session_id = ?
        |ORDER BY id ASC""".stripMargin
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setLong(1, sessionId)
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
      sessionId: Long,
      role: String,
      text: String
  ): Unit =
    val sql =
      "INSERT INTO messages(session_id, role, text) VALUES (?, ?, ?)"
    SqliteJdbc.withStatement(conn, sql): stmt =>
      stmt.setLong(1, sessionId)
      stmt.setString(2, role)
      stmt.setString(3, text)
      stmt.executeUpdate()
      ()

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
