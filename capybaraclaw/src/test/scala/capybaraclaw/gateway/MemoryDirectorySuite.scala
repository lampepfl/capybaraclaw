package capybaraclaw.gateway

import capybaraclaw.agent.{MemoryAccess, MemoryFile}
import capybaraclaw.agent.tools.MemoryTool
import MemoryTool.Args

class MemoryDirectorySuite extends munit.FunSuite:
  private val slack = PortId("slack")
  private val cli = PortId("cli")

  private def linked(root: os.Path): MemoryDirectory =
    val ids = os.temp(
      """{"people":{"lukasz":{"ids":["cli:lbialy","slack:U1"]}}}"""
    )
    MemoryDirectory(root.toIO, () => Identities.load(ids.toIO))

  private def tool(access: MemoryAccess, args: Args): ujson.Value =
    ujson.read(MemoryTool.run(access, args))

  private def add(access: MemoryAccess, target: String, text: String) =
    val result = tool(access, Args("add", target, content = Some(text)))
    assert(result("success").bool, result.render())

  private def contents(access: MemoryAccess): Map[String, String] =
    access.snapshot().sections.map(s => s.file.target -> s.content).toMap

  test("ids from outside become single, distinct path segments"):
    assertEquals(MemoryDirectory.segment("slack:U1"), "slack%3AU1")
    assertEquals(MemoryDirectory.segment(".."), "%2E%2E")
    assertEquals(MemoryDirectory.segment("../etc"), "%2E%2E%2Fetc")
    assertEquals(MemoryDirectory.segment("a b"), "a%20b")
    assertEquals(MemoryDirectory.segment("ł"), "%C5%82")
    assertNotEquals(
      MemoryDirectory.segment("a:b"),
      MemoryDirectory.segment("a%3Ab")
    )

  test("a hostile CLI user name stays inside persons/"):
    val root = os.temp.dir()
    val access = MemoryDirectory(root.toIO, () => Identities.empty)
      .access(cli, UserId("../../escape"), Conversation.Direct)
    add(access, "private", "note")
    assertEquals(
      os.list(root / "persons").map(_.last),
      Seq("cli%3A%2E%2E%2F%2E%2E%2Fescape")
    )
    assert(!os.exists(root / os.up / "escape"))

  test("private memory follows a linked person across ports"):
    val dir = linked(os.temp.dir())
    add(
      dir.access(cli, UserId("lbialy"), Conversation.Direct),
      "user",
      "likes tea"
    )
    val viaSlack = dir.access(slack, UserId("U1"), Conversation.Direct)
    assertEquals(contents(viaSlack)("user"), "likes tea")
    assert(viaSlack.conversation.contains("lukasz"), viaSlack.conversation)

  test("private memory is per person"):
    val dir = linked(os.temp.dir())
    add(
      dir.access(slack, UserId("U1"), Conversation.Direct),
      "private",
      "secret"
    )
    val other = dir.access(slack, UserId("U2"), Conversation.Direct)
    assertEquals(contents(other)("private"), "")

  test("a shared conversation never sees private memory"):
    val dir = linked(os.temp.dir())
    add(
      dir.access(slack, UserId("U1"), Conversation.Direct),
      "private",
      "secret"
    )
    add(dir.access(slack, UserId("U1"), Conversation.Direct), "user", "profile")
    val shared = dir.access(slack, UserId("U1"), Conversation.Group("C1"))
    assertEquals(contents(shared).keySet, Set("channel", "public"))
    assert(!shared.snapshot().sections.exists(_.content.contains("secret")))
    val result = tool(shared, Args("read", "private"))
    assert(!result("success").bool, result.render())
    assert(
      result("error").str.contains("Use: channel, public"),
      result.render()
    )

  test("channel memory is shared by everyone in that channel only"):
    val dir = linked(os.temp.dir())
    add(
      dir.access(slack, UserId("U1"), Conversation.Group("C1")),
      "channel",
      "deploys on friday"
    )
    assertEquals(
      contents(dir.access(slack, UserId("U2"), Conversation.Group("C1")))(
        "channel"
      ),
      "deploys on friday"
    )
    assertEquals(
      contents(dir.access(slack, UserId("U2"), Conversation.Group("C2")))(
        "channel"
      ),
      ""
    )

  test(
    "public memory is read everywhere and written by nobody through the tool"
  ):
    val root = os.temp.dir()
    os.write(
      root / "public" / "MEMORY.md",
      "curated fact",
      createFolders = true
    )
    val dir = linked(root)
    List(
      dir.access(cli, UserId("lbialy"), Conversation.Direct),
      dir.access(slack, UserId("U2"), Conversation.Group("C1"))
    ).foreach: access =>
      assertEquals(contents(access)("public"), "curated fact")
      List(
        Args("add", "public", content = Some("injected")),
        Args(
          "replace",
          "public",
          old_text = Some("curated"),
          content = Some("x")
        ),
        Args("remove", "public", old_text = Some("curated")),
        Args("reconcile", "public", content = Some(""))
      ).foreach: args =>
        val result = tool(access, args)
        assert(!result("success").bool, result.render())
        assertEquals(result("error").str, MemoryDirectory.PublicReadOnly)
      assert(tool(access, Args("read", "public"))("success").bool)
    assertEquals(os.read(root / "public" / "MEMORY.md"), "curated fact")

  test("the tool description lists only this conversation's targets"):
    val shared = linked(os.temp.dir())
      .access(slack, UserId("U1"), Conversation.Group("C1"))
    val description = MemoryTool.description(shared)
    assert(description.contains("'channel'"), description)
    assert(
      description.contains(
        "'public' (visible in every conversation, including shared channels, read-only)"
      ),
      description
    )
    assert(!description.contains("'private'"), description)

  test("legacy memory moves into the owner's private memory"):
    val root = os.temp.dir()
    os.write(root / "MEMORY.md", "old notes")
    os.write(root / "USER.md", "old profile")
    os.write(root / "MEMORY.md.bak.abc", "old backup")
    os.write(root / "MEMORY.md.lock", "")
    val dir = linked(root)
    dir.migrateLegacy(dir.person(cli, UserId("lbialy")))
    val access = dir.access(slack, UserId("U1"), Conversation.Direct)
    assertEquals(contents(access)("private"), "old notes")
    assertEquals(contents(access)("user"), "old profile")
    assert(os.exists(root / "persons" / "lukasz" / "MEMORY.md.bak.abc"))
    assert(!os.exists(root / "MEMORY.md"))
    assert(os.exists(root / "MEMORY.md.lock"))

  test("migration leaves files alone when the target already exists"):
    val root = os.temp.dir()
    os.write(root / "MEMORY.md", "old notes")
    os.write(
      root / "persons" / "lukasz" / "MEMORY.md",
      "new notes",
      createFolders = true
    )
    val dir = linked(root)
    dir.migrateLegacy(dir.person(cli, UserId("lbialy")))
    assertEquals(os.read(root / "MEMORY.md"), "old notes")
    assertEquals(
      os.read(root / "persons" / "lukasz" / "MEMORY.md"),
      "new notes"
    )
