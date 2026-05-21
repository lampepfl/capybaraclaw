package capybaraclaw.gateway

import capybaraclaw.gateway.sqlite.SqliteContextProvider
import capybaraclaw.gateway.port.slack.SlackPort
import tacit.agents.llm.endpoint.{Content, Message, Role}

import java.nio.file.{Files, Path}
import java.sql.{Connection, DriverManager, ResultSet}
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}
import scala.jdk.CollectionConverters.*

class SqliteContextProviderSuite extends munit.FunSuite:

  private val WD = "/tmp/test"
  private val OtherWD = "/tmp/other"
  private def handle(value: String): SessionHandle =
    SessionHandle(SlackPort.Id, value)

  test("load on a fresh session UUID returns empty without creating a session"):
    withProvider(): (provider, dbPath) =>
      assertEquals(provider.load(SessionId.random()), Nil)

      withConnection(dbPath): conn =>
        assertEquals(queryInt(conn, "SELECT COUNT(*) FROM sessions"), 0)

  test("createSession inserts metadata"):
    withProvider(): (provider, dbPath) =>
      val sessionId = provider.createSession(WD)

      assert(provider.resumeSession(sessionId).isDefined)
      withConnection(dbPath): conn =>
        val rows = queryRowsPrepared(
          conn,
          """SELECT id, workdir, created_at, last_activity
            |FROM sessions WHERE id = ?""".stripMargin,
          List(sessionId.value)
        )
        assertEquals(rows.size, 1)
        assertEquals(rows.head(0), sessionId.value)
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
    "touchSession preserves created_at and bumps last_activity"
  ):
    withProvider(): (provider, dbPath) =>
      val sessionId = provider.createSession(WD)
      Thread.sleep(10)
      provider.touchSession(sessionId)

      withConnection(dbPath): conn =>
        val rows = queryRowsPrepared(
          conn,
          """SELECT workdir, created_at, last_activity
            |FROM sessions WHERE id = ?""".stripMargin,
          List(sessionId.value)
        )
        assertEquals(rows.size, 1)
        val row = rows.head
        assertEquals(row(0), WD, "workdir preserved")
        assertNotEquals(
          row(1),
          row(2),
          "last_activity advanced past created_at"
        )

  test(
    "touchSession fails when the session UUID does not exist"
  ):
    withProvider(): (provider, _) =>
      val missing = SessionId.random()

      val error = intercept[IllegalArgumentException]:
        provider.touchSession(missing)

      assert(
        error.getMessage.contains(missing.value),
        s"error should mention the missing session id, got: ${error.getMessage}"
      )

  test(
    "verifyAndTouchSession returns pre-bump metadata and bumps last_activity"
  ):
    withProvider(): (provider, dbPath) =>
      val sessionId = provider.createSession(WD)
      val before = provider.resumeSession(sessionId).get
      Thread.sleep(10)

      val observed = provider.verifyAndTouchSession(sessionId)
      assertEquals(observed.map(_.lastActivity), Some(before.lastActivity))

      withConnection(dbPath): conn =>
        val rows = queryRowsPrepared(
          conn,
          "SELECT created_at, last_activity FROM sessions WHERE id = ?",
          List(sessionId.value)
        )
        assertNotEquals(
          rows.head(0),
          rows.head(1),
          "last_activity advanced past created_at"
        )

  test("verifyAndTouchSession returns None for an unknown session UUID"):
    withProvider(): (provider, _) =>
      assertEquals(provider.verifyAndTouchSession(SessionId.random()), None)

  test("resolveOrCreateHandle bumps last_activity on an existing handle hit"):
    withProvider(): (provider, dbPath) =>
      val sessionId = provider.resolveOrCreateHandle(WD, handle("C1"))
      val createdAt = withConnection(dbPath): conn =>
        queryRowsPrepared(
          conn,
          "SELECT created_at FROM sessions WHERE id = ?",
          List(sessionId.value)
        ).head.head
      Thread.sleep(10)

      val again = provider.resolveOrCreateHandle(WD, handle("C1"))
      assertEquals(again, sessionId)

      withConnection(dbPath): conn =>
        val rows = queryRowsPrepared(
          conn,
          "SELECT created_at, last_activity FROM sessions WHERE id = ?",
          List(sessionId.value)
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
      assert(provider.resumeSession(first).exists(_.workdir == WD))

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

  test("UNIQUE constraint rejects a duplicate handle in the same workdir"):
    withProvider(): (provider, dbPath) =>
      val first = provider.resolveOrCreateHandle(WD, handle("shared"))
      val second = provider.createSession(WD)
      withConnection(dbPath): conn =>
        val sql =
          """INSERT INTO session_handles(session_id, workdir, kind, value)
            |VALUES (?, ?, ?, ?)""".stripMargin
        val stmt = conn.prepareStatement(sql)
        try
          stmt.setString(1, second.value)
          stmt.setString(2, WD)
          stmt.setString(3, "slack")
          stmt.setString(4, "shared")
          intercept[java.sql.SQLException]:
            stmt.executeUpdate()
        finally stmt.close()

      assertEquals(provider.resolveOrCreateHandle(WD, handle("shared")), first)

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

  test(
    "resolveOrCreateHandle surfaces row context when the stored session_id is not a UUID"
  ):
    withProvider(): (provider, dbPath) =>
      withConnection(dbPath): conn =>
        val insertSession = conn.prepareStatement(
          """INSERT INTO sessions(id, workdir, created_at, last_activity)
            |VALUES (?, ?, ?, ?)""".stripMargin
        )
        try
          insertSession.setString(1, "not-a-uuid")
          insertSession.setString(2, WD)
          insertSession.setLong(3, 0L)
          insertSession.setLong(4, 0L)
          insertSession.executeUpdate()
        finally insertSession.close()

        val insertHandle = conn.prepareStatement(
          """INSERT INTO session_handles(session_id, workdir, kind, value)
            |VALUES (?, ?, ?, ?)""".stripMargin
        )
        try
          insertHandle.setString(1, "not-a-uuid")
          insertHandle.setString(2, WD)
          insertHandle.setString(3, SlackPort.Id)
          insertHandle.setString(4, "corrupt")
          insertHandle.executeUpdate()
        finally insertHandle.close()

      val error = intercept[IllegalStateException]:
        provider.resolveOrCreateHandle(WD, handle("corrupt"))

      assert(
        error.getMessage.contains("session_handles.session_id"),
        s"error should name the offending column, got: ${error.getMessage}"
      )
      assert(
        error.getMessage.contains("not-a-uuid"),
        s"error should include the offending value, got: ${error.getMessage}"
      )
      assert(
        error.getMessage.contains("corrupt"),
        s"error should include the handle value, got: ${error.getMessage}"
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

  test(
    "concurrent providers in same baseDir do not deadlock or violate uniqueness"
  ):
    val dir = Files.createTempDirectory("claw-multi-instance")
    val providerA = SqliteContextProvider(dir)
    try
      val providerB = SqliteContextProvider(dir)
      try
        val sessionId =
          providerA.resolveOrCreateHandle(WD, handle("shared"))

        val threadA = new Thread(() =>
          (1 to 5).foreach: i =>
            providerA.touchSession(sessionId)
            providerA.append(sessionId, Message.user(s"a-$i"))
        )
        val threadB = new Thread(() =>
          (1 to 5).foreach: i =>
            providerB.touchSession(sessionId)
            providerB.append(sessionId, Message.user(s"b-$i"))
        )

        threadA.start()
        threadB.start()
        threadA.join()
        threadB.join()

        val combined = providerA.load(sessionId).map(_.text).toSet
        assertEquals(
          combined,
          (1 to 5).flatMap(i => List(s"a-$i", s"b-$i")).toSet
        )
      finally providerB.close()
    finally providerA.close()

  private def withProvider(
  )(body: (SqliteContextProvider, Path) => Unit): Unit =
    val dir = Files.createTempDirectory("claw-sqlite-provider")
    val dbPath = dir.resolve("state.db")
    val provider = SqliteContextProvider(dir)
    try body(provider, dbPath)
    finally provider.close()

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
