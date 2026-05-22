package capybaraclaw.gateway

import capybaraclaw.gateway.sqlite.SqliteContextProvider
import capybaraclaw.gateway.port.slack.SlackPort
import tacit.agents.llm.endpoint.{Content, Message, Role}

import java.nio.file.{Files, Path}
import java.sql.{Connection, DriverManager, ResultSet}
import java.time.Instant
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

class SqliteContextProviderSuite extends munit.FunSuite:

  private val WD = "/tmp/test"
  private val OtherWD = "/tmp/other"
  private def handle(value: String): SessionHandle =
    SessionHandle(SlackPort.Id, value)

  test("createSession inserts metadata"):
    withProvider(): (provider, dbPath) =>
      val sessionId = provider.createSession(WD)

      withConnection(dbPath): conn =>
        val rows = queryRowsPrepared(
          conn,
          """SELECT id, workdir, created_at, last_activity
            |FROM sessions WHERE id = ?""".stripMargin,
          List(sessionId)
        )
        assertEquals(rows.size, 1)
        assertEquals(rows.head(0), sessionId)
        assertEquals(rows.head(1), WD)
        assertEquals(rows.head(2), rows.head(3))
        assertEquals(queryInt(conn, "SELECT COUNT(*) FROM session_handles"), 0)

  test("append then load by UUID preserves message order"):
    withProvider(): (provider, _) =>
      val sessionId = provider.createSession(WD)
      provider.append(sessionId, Message.user("first"))
      provider.append(sessionId, Message.assistant("second"))
      provider.append(sessionId, Message.user("third"))

      assertEquals(
        provider.load(sessionId).map(m => m.role -> m.text),
        List(
          Role.User -> "first",
          Role.Assistant -> "second",
          Role.User -> "third"
        )
      )

  test("keeps separate histories for different sessionIds"):
    withProvider(): (provider, _) =>
      val a = provider.createSession(WD)
      val b = provider.createSession(WD)
      val c = provider.createSession(WD)
      provider.append(a, Message.user("alpha message"))
      provider.append(b, Message.user("beta message"))
      provider.append(c, Message.assistant("gamma message"))

      assertEquals(provider.load(a).map(_.text), List("alpha message"))
      assertEquals(provider.load(b).map(_.text), List("beta message"))
      assertEquals(provider.load(c).map(_.text), List("gamma message"))

  test(
    "verifyAndTouchSession returns pre-bump metadata and bumps last_activity"
  ):
    withProvider(sequentialClock()): (provider, dbPath) =>
      val sessionId = provider.createSession(WD)
      val lastActivityBefore = withConnection(dbPath): conn =>
        queryRowsPrepared(
          conn,
          "SELECT last_activity FROM sessions WHERE id = ?",
          List(sessionId)
        ).head.head.toLong

      val observed = provider.verifyAndTouchSession(sessionId, WD)
      assertEquals(
        observed.map(_.lastActivity.toEpochMilli),
        Some(lastActivityBefore),
        "returned metadata reflects pre-bump state"
      )

      withConnection(dbPath): conn =>
        val lastActivityAfter = queryRowsPrepared(
          conn,
          "SELECT last_activity FROM sessions WHERE id = ?",
          List(sessionId)
        ).head.head.toLong
        assertNotEquals(
          lastActivityAfter,
          lastActivityBefore,
          "DB last_activity advanced"
        )

  test("verifyAndTouchSession returns None for an unknown session UUID"):
    withProvider(): (provider, _) =>
      assertEquals(
        provider.verifyAndTouchSession(SessionId.random(), WD),
        None
      )

  test(
    "verifyAndTouchSession leaves last_activity untouched on workdir mismatch"
  ):
    withProvider(sequentialClock()): (provider, dbPath) =>
      val sessionId = provider.createSession(WD)
      val createdAt = withConnection(dbPath): conn =>
        queryRowsPrepared(
          conn,
          "SELECT created_at FROM sessions WHERE id = ?",
          List(sessionId)
        ).head.head

      val observed = provider.verifyAndTouchSession(sessionId, OtherWD)
      assertEquals(observed.map(_.workdir), Some(WD))

      withConnection(dbPath): conn =>
        val rows = queryRowsPrepared(
          conn,
          "SELECT created_at, last_activity FROM sessions WHERE id = ?",
          List(sessionId)
        )
        assertEquals(
          rows.head(0),
          rows.head(1),
          "last_activity equals created_at — no touch happened"
        )
        assertEquals(rows.head(0), createdAt, "created_at preserved")

  test("resolveOrCreateHandle bumps last_activity on an existing handle hit"):
    withProvider(sequentialClock()): (provider, dbPath) =>
      val sessionId = provider.resolveOrCreateHandle(WD, handle("C1"))
      val createdAt = withConnection(dbPath): conn =>
        queryRowsPrepared(
          conn,
          "SELECT created_at FROM sessions WHERE id = ?",
          List(sessionId)
        ).head.head

      val again = provider.resolveOrCreateHandle(WD, handle("C1"))
      assertEquals(again, sessionId)

      withConnection(dbPath): conn =>
        val rows = queryRowsPrepared(
          conn,
          "SELECT created_at, last_activity FROM sessions WHERE id = ?",
          List(sessionId)
        )
        assertEquals(rows.head(0), createdAt, "created_at preserved")
        assertNotEquals(
          rows.head(1),
          createdAt,
          "last_activity advanced past created_at"
        )

  test("resolveOrCreateHandle tolerates concurrent first writers"):
    val dir = Files.createTempDirectory("claw-concurrent-first-handle")
    val providerA = SqliteContextProvider(dir)
    try
      val providerB = SqliteContextProvider(dir)
      try
        val start = CountDownLatch(1)
        val results = ConcurrentLinkedQueue[SessionId]()
        val errors = ConcurrentLinkedQueue[Throwable]()
        val sharedHandle = handle("first-writer-race")
        val workers: List[Thread] = List(providerA, providerB).map: provider =>
          Thread(
            new Runnable:
              def run(): Unit =
                start.await()
                try
                  results.offer(
                    provider.resolveOrCreateHandle(WD, sharedHandle)
                  )
                  ()
                catch
                  case e: Throwable =>
                    errors.offer(e)
                    ()
          )

        workers.foreach(_.start())
        start.countDown()
        workers.foreach(_.join())

        assertEquals(
          errors.size(),
          0,
          errors.iterator().asScala.toList.map(_.toString).mkString("\n")
        )
        assertEquals(results.iterator().asScala.toList.distinct.size, 1)
      finally providerB.close()
    finally providerA.close()

  test("resolveOrCreateHandle returns existing UUID or creates a new UUID"):
    withProvider(): (provider, _) =>
      val first = provider.resolveOrCreateHandle(WD, handle("C1"))
      val second = provider.resolveOrCreateHandle(WD, handle("C1"))
      val third = provider.resolveOrCreateHandle(WD, handle("C2"))

      assertEquals(second, first)
      assertNotEquals(third, first)

  test("same Slack local id in different workdirs creates two UUIDs"):
    withProvider(): (provider, dbPath) =>
      val a = provider.resolveOrCreateHandle(WD, handle("shared"))
      val b = provider.resolveOrCreateHandle(OtherWD, handle("shared"))
      provider.append(a, Message.user("from a"))
      provider.append(b, Message.user("from b"))

      assertEquals(provider.load(a).map(_.text), List("from a"))
      assertEquals(provider.load(b).map(_.text), List("from b"))

      withConnection(dbPath): conn =>
        assertEquals(queryInt(conn, "SELECT COUNT(*) FROM sessions"), 2)

  test("persists only user and assistant text messages"):
    withProvider(): (provider, _) =>
      val sessionId = provider.createSession(WD)
      val skipped = List(
        Message.system("system prompt"),
        Message(Role.Assistant, List(Content.Thinking("private thought"))),
        Message(Role.Assistant, List(Content.ToolUse("id", "tool", "{}"))),
        Message.toolResult("id", "tool output"),
        Message(Role.User, List(Content.Text("")))
      )

      skipped.foreach(provider.append(sessionId, _))
      provider.append(sessionId, Message.user("visible user"))
      provider.append(sessionId, Message.assistant("visible assistant"))

      assertEquals(
        provider.load(sessionId).map(m => m.role -> m.text),
        List(
          Role.User -> "visible user",
          Role.Assistant -> "visible assistant"
        )
      )

  test("messages FTS matches persisted text"):
    withProvider(): (provider, dbPath) =>
      val sessionId = provider.createSession(WD)
      provider.append(
        sessionId,
        Message.user("sqlite can search capybara transcripts")
      )
      provider.append(sessionId, Message.assistant("ordinary response"))

      withConnection(dbPath): conn =>
        val matches = queryRowsPrepared(
          conn,
          """SELECT m.text
            |FROM messages_fts
            |JOIN messages m ON m.id = messages_fts.rowid
            |WHERE messages_fts MATCH ?
            |ORDER BY m.id ASC""".stripMargin,
          List("capybara")
        )
        assertEquals(
          matches,
          List(List("sqlite can search capybara transcripts"))
        )

  test("operations after close fail fast instead of hanging"):
    val dir = Files.createTempDirectory("claw-after-close")
    val provider = SqliteContextProvider(dir)
    provider.close()
    intercept[IllegalStateException]:
      provider.load(SessionId.random())
    intercept[Exception]:
      provider.append(SessionId.random(), Message.user("hi"))

  test("close is idempotent"):
    val dir = Files.createTempDirectory("claw-close-twice")
    val provider = SqliteContextProvider(dir)
    provider.close()
    provider.close() // must not throw

  private def withProvider(
      nowMillis: () => Long = () => Instant.now.toEpochMilli
  )(body: (SqliteContextProvider, Path) => Unit): Unit =
    val dir = Files.createTempDirectory("claw-sqlite-provider")
    val dbPath = dir.resolve("state.db")
    val provider = SqliteContextProvider(dir, nowMillis)
    try body(provider, dbPath)
    finally provider.close()

  private def sequentialClock(): () => Long =
    val ticks = AtomicLong(0L)
    () => ticks.incrementAndGet()

  private def withConnection[A](dbPath: Path)(body: Connection => A): A =
    val conn = DriverManager.getConnection(s"jdbc:sqlite:${dbPath.toString}")
    try body(conn)
    finally conn.close()

  private def queryInt(conn: Connection, sql: String): Int =
    queryRows(conn, sql).head.head.toInt

  private def queryRows(conn: Connection, sql: String): List[List[String]] =
    val stmt = conn.prepareStatement(sql)
    try collectRows(stmt.executeQuery())
    finally stmt.close()

  private def queryRowsPrepared(
      conn: Connection,
      sql: String,
      values: List[String]
  ): List[List[String]] =
    val stmt = conn.prepareStatement(sql)
    try
      values.zipWithIndex.foreach: (value, index) =>
        stmt.setString(index + 1, value)
      collectRows(stmt.executeQuery())
    finally stmt.close()

  private def collectRows(rs: ResultSet): List[List[String]] =
    try
      val columnCount = rs.getMetaData.getColumnCount
      Iterator
        .continually(rs.next())
        .takeWhile(identity)
        .map: _ =>
          (1 to columnCount).map(index => rs.getString(index)).toList
        .toList
    finally rs.close()
