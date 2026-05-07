package capybaraclaw.gateway

import capybaraclaw.gateway.sqlite.SqliteContextProvider
import tacit.agents.llm.endpoint.{Content, Message, Role}

import java.nio.file.{Files, Path}
import java.sql.{Connection, DriverManager, ResultSet}

class SqliteContextProviderSuite extends munit.FunSuite:

  test("load on a fresh key returns empty without creating a session"):
    withProvider(): (provider, dbPath) =>
      val key = ContextKey("cli", "default")
      assertEquals(provider.load(key), Nil)

      withConnection(dbPath): conn =>
        assertEquals(queryInt(conn, "SELECT COUNT(*) FROM sessions"), 0)

  test("append then load preserves message order"):
    withProvider(): (provider, _) =>
      val key = ContextKey("cli", "default")
      provider.append(key, Message.user("first"))
      provider.append(key, Message.assistant("second"))
      provider.append(key, Message.user("third"))

      assertEquals(
        provider.load(key).map(m => m.role -> m.text),
        List(
          Role.User -> "first",
          Role.Assistant -> "second",
          Role.User -> "third"
        )
      )

  test("keeps separate histories for different port and thread pairs"):
    withProvider(): (provider, _) =>
      val cli = ContextKey("cli", "same-thread")
      val slack = ContextKey("slack", "same-thread")
      val otherThread = ContextKey("cli", "other-thread")

      provider.append(cli, Message.user("cli message"))
      provider.append(slack, Message.user("slack message"))
      provider.append(otherThread, Message.assistant("other message"))

      assertEquals(provider.load(cli).map(_.text), List("cli message"))
      assertEquals(provider.load(slack).map(_.text), List("slack message"))
      assertEquals(
        provider.load(otherThread).map(_.text),
        List("other message")
      )

  test("persists only user and assistant text messages"):
    withProvider(): (provider, _) =>
      val key = ContextKey("cli", "default")
      val skipped = List(
        Message.system("system prompt"),
        Message(Role.Assistant, List(Content.Thinking("private thought"))),
        Message(Role.Assistant, List(Content.ToolUse("id", "tool", "{}"))),
        Message.toolResult("id", "tool output"),
        Message(Role.User, List(Content.Text("")))
      )

      skipped.foreach(provider.append(key, _))
      provider.append(key, Message.user("visible user"))
      provider.append(key, Message.assistant("visible assistant"))

      assertEquals(
        provider.load(key).map(m => m.role -> m.text),
        List(
          Role.User -> "visible user",
          Role.Assistant -> "visible assistant"
        )
      )

  test("messages FTS matches persisted text"):
    withProvider(): (provider, dbPath) =>
      val key = ContextKey("cli", "default")
      provider.append(
        key,
        Message.user("sqlite can search capybara transcripts")
      )
      provider.append(key, Message.assistant("ordinary response"))

      withConnection(dbPath): conn =>
        val matches = queryPreparedRows(
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
    val provider = SqliteContextProvider(dir.toFile)
    provider.close()
    intercept[IllegalStateException]:
      provider.load(ContextKey("cli", "default"))
    intercept[Exception]:
      provider.append(ContextKey("cli", "default"), Message.user("hi"))

  test("close is idempotent"):
    val dir = Files.createTempDirectory("claw-close-twice")
    val provider = SqliteContextProvider(dir.toFile)
    provider.close()
    provider.close() // must not throw

  test(
    "concurrent providers in one workdir do not deadlock or violate uniqueness"
  ):
    val dir = Files.createTempDirectory("claw-multi-instance")
    val providerA = SqliteContextProvider(dir.toFile)
    try
      val providerB = SqliteContextProvider(dir.toFile)
      try
        val key = ContextKey("cli", "shared")

        val threadA = new Thread(() =>
          (1 to 5).foreach: i =>
            providerA.append(key, Message.user(s"a-$i"))
        )
        val threadB = new Thread(() =>
          (1 to 5).foreach: i =>
            providerB.append(key, Message.user(s"b-$i"))
        )

        threadA.start()
        threadB.start()
        threadA.join()
        threadB.join()

        val combined = providerA.load(key).map(_.text).toSet
        assertEquals(
          combined,
          (1 to 5).flatMap(i => List(s"a-$i", s"b-$i")).toSet
        )
      finally providerB.close()
    finally providerA.close()

  private def withProvider(
  )(body: (SqliteContextProvider, Path) => Unit): Unit =
    val dir = Files.createTempDirectory("claw-sqlite-provider")
    val dbPath = dir.resolve(".claw").resolve("state.db")
    val provider = SqliteContextProvider(dir.toFile)
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

  private def queryPreparedRows(
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
