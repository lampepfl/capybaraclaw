package capybaraclaw.gateway

import capybaraclaw.agent.ConfigError

import Permission.*

class IdentitiesSuite extends munit.FunSuite:
  private def load(json: String): Identities =
    val file = os.temp(json, prefix = "identities-", suffix = ".json")
    Identities.load(file.toIO)

  private def rejected(json: String, fragment: String): Unit =
    val ex = intercept[ConfigError](load(json))
    assert(ex.getMessage.contains(fragment), ex.getMessage)

  private val slack = PortId("slack")

  /** One role allowing `rules`, held by `slack:U1`. */
  private def holder(rules: String): (Identities, Person) =
    val ids = load(
      s"""{"roles": {"r": {"may": [$rules]}}, "people": {"p": {"ids": ["slack:U1"], "roles": ["r"]}}}"""
    )
    (ids, ids.person(slack, UserId("U1")))

  private def allows(rules: String, permission: Permission): Boolean =
    val (ids, p) = holder(rules)
    ids.disallowed(p, permission).isEmpty

  test("a missing file links nobody and allows nothing"):
    val ids = Identities.load(java.io.File("/nonexistent/identities.json"))
    val p = ids.person(slack, UserId("U1"))
    assertEquals(p, Person("slack:U1", Set.empty))
    assertEquals(
      ids.disallowed(p, Commands(Set("git"))),
      Some(Commands(Set("git")))
    )

  test("linked ids on different ports resolve to the same person"):
    val ids = load(
      """{"roles": {"admin": {"may": ["*"]}},
         "people": {"lukasz": {"ids": ["cli:lbialy", "slack:U1"], "roles": ["admin"]}}}"""
    )
    val expected = Person("lukasz", Set("admin"))
    assertEquals(ids.person(PortId("cli"), UserId("lbialy")), expected)
    assertEquals(ids.person(slack, UserId("U1")), expected)
    assertEquals(ids.person(slack, UserId("U2")), Person("slack:U2", Set.empty))

  test("default roles go to people not listed and listed without roles"):
    val ids = load(
      """{"roles": {"guest": {"may": []}, "dev": {"may": []}},
         "default_roles": ["guest"],
         "people": {"ann": {"ids": ["slack:U3"]}, "bo": {"ids": ["slack:U4"], "roles": ["dev"]}}}"""
    )
    assertEquals(ids.person(slack, UserId("U9")).roles, Set("guest"))
    assertEquals(ids.person(slack, UserId("U3")).roles, Set("guest"))
    assertEquals(ids.person(slack, UserId("U4")).roles, Set("dev"))

  test("the operator is allowed everything, on their own port and user only"):
    val ids = load(
      """{"people": {"lukasz": {"ids": ["cli:lbialy", "slack:U1"], "roles": []}}}"""
    ).withOperator("cli:lbialy")
    val cli = ids.person(PortId("cli"), UserId("lbialy"))
    assert(cli.operator)
    assertEquals(ids.disallowed(cli, Commands(Set("rm"))), None)
    val onSlack = ids.person(slack, UserId("U1"))
    assert(!onSlack.operator)
    assertEquals(
      ids.disallowed(onSlack, Commands(Set("rm"))),
      Some(Commands(Set("rm")))
    )

  test("a pattern covers everything below it"):
    assert(allows("\"*\"", Plugin("any", "thing", Set.empty)))
    assert(allows("\"files\"", Files("/x", FileAccess.ReadWrite)))
    assert(allows("\"plugin\"", Plugin("a", "b", Set("c"))))
    assert(allows("\"plugin:*\"", Plugin("a", "b", Set.empty)))
    assert(allows("\"plugin:demo\"", Plugin("demo", "read payroll", Set.empty)))
    assert(allows("\"plugin:demo/*\"", Plugin("demo", "read", Set.empty)))
    assert(
      allows(
        "\"plugin:demo/read payroll\"",
        Plugin("demo", "read payroll", Set("q3"))
      )
    )
    assert(
      !allows(
        "\"plugin:demo/read payroll\"",
        Plugin("demo", "edit payroll", Set("q3"))
      )
    )
    assert(!allows("\"plugin:demo\"", Plugin("other", "read", Set.empty)))
    assert(!allows("\"plugin:*\"", Commands(Set("git"))))

  test("wider access covers narrower, never the other way"):
    assert(allows("\"files:write\"", Files("/x", FileAccess.Read)))
    assert(!allows("\"files:read\"", Files("/x", FileAccess.ReadWrite)))
    assert(allows("\"network:send\"", Hosts(Set("a.com"), NetworkAccess.Fetch)))
    assert(
      !allows("\"network:fetch\"", Hosts(Set("a.com"), NetworkAccess.Send))
    )

  test("items limit a rule to those commands, hosts and plugin items"):
    val (ids, p) = holder(
      """{"permission": "exec", "items": ["git"]},
         {"permission": "network", "items": ["API.github.com"]},
         {"permission": "plugin:demo/read", "items": ["q3"]}"""
    )
    assertEquals(
      ids.disallowed(p, Commands(Set("git", "rm"))),
      Some(Commands(Set("rm")))
    )
    assertEquals(
      ids.disallowed(p, Hosts(Set("api.GitHub.com"), NetworkAccess.Send)),
      None,
      "host names are compared case-insensitively"
    )
    assertEquals(
      ids.disallowed(p, Plugin("demo", "read", Set("q3", "q4"))),
      Some(Plugin("demo", "read", Set("q4")))
    )
    assertEquals(
      ids.disallowed(p, Plugin("demo", "read", Set.empty)),
      Some(Plugin("demo", "read", Set.empty)),
      "a rule with items does not cover the permission as a whole"
    )

  test("items may be allowed by different rules and roles"):
    val ids = load(
      """{"roles": {
           "a": {"may": [{"permission": "exec", "items": ["git"]}]},
           "b": {"may": [{"permission": "exec", "items": ["sbt"]}]}},
         "people": {"p": {"ids": ["slack:U1"], "roles": ["a", "b"]}}}"""
    )
    assertEquals(
      ids.disallowed(
        ids.person(slack, UserId("U1")),
        Commands(Set("git", "sbt"))
      ),
      None
    )

  test("directory items cover everything below them, after resolving them"):
    val dir = os.temp.dir()
    val link = os.temp.dir() / "link"
    os.symlink(link, dir)
    val canonical = dir.toNIO.toRealPath().toString
    val (ids, p) =
      holder(s"""{"permission": "files:read", "items": ["$link"]}""")
    assertEquals(
      ids.disallowed(p, Files(s"$canonical/sub", FileAccess.Read)),
      None
    )
    assertEquals(
      ids.disallowed(p, Files(s"${canonical}2", FileAccess.Read)),
      Some(Files(s"${canonical}2", FileAccess.Read)),
      "a shared name prefix is not a subpath"
    )

  test("a home-relative directory item"):
    val home = java.io.File(System.getProperty("user.home")).getCanonicalPath
    val (ids, p) = holder("""{"permission": "files", "items": ["~/data"]}""")
    assertEquals(
      ids.disallowed(p, Files(s"$home/data/x", FileAccess.Read)),
      None
    )

  test("a person cannot be named like the id of someone not listed"):
    // Otherwise the unlisted slack:U999 would share this person's id, and
    // with it their grants and private memory.
    rejected(
      """{"people": {"slack:U999": {"ids": ["slack:U111"]}}}""",
      "person 'slack:U999': a person name must not contain ':'"
    )

  test("plugin items need the plugin they belong to"):
    rejected(
      """{"roles":{"r":{"may":[{"permission":"plugin","items":["q3"]}]}}}""",
      "'items' of a plugin permission needs the plugin"
    )
    rejected(
      """{"roles":{"r":{"may":[{"permission":"plugin:*/read","items":["q3"]}]}}}""",
      "'items' of a plugin permission needs the plugin"
    )
    assert(
      allows(
        """{"permission": "plugin:demo/*", "items": ["q3"]}""",
        Plugin("demo", "x", Set("q3"))
      )
    )

  test("an id listed for two people is rejected"):
    rejected(
      """{"people":{"a":{"ids":["slack:U1"]},"b":{"ids":["slack:U1"]}}}""",
      "id 'slack:U1' is listed for several people: a, b"
    )

  test("unknown keys suggest the closest known one"):
    rejected(
      """{"people":{"a":{"id":["slack:U1"]}}}""",
      "unknown key 'id', did you mean 'ids'?"
    )
    rejected(
      """{"peeple":{}}""",
      "unknown key 'peeple', did you mean 'people'?"
    )
    rejected(
      """{"people":{"a":{"ids":["slack:U1"],"role":"admin"}}}""",
      "unknown key 'role', did you mean 'roles'?"
    )

  List(
    """{"people":{"a":{"ids":["U1"]}}}""" -> "<port>:<user>",
    """{"people":{"a":{"ids":[":U1"]}}}""" -> "<port>:<user>",
    """{"people":{"a":{"ids":["slack:"]}}}""" -> "<port>:<user>",
    """{"people":{"a":{"ids":[]}}}""" -> "'ids' must be a non-empty array",
    """{"people":{"a":{"ids":["slack:U1"],"roles":["root"]}}}""" -> "unknown role 'root'",
    """{"default_roles":["root"]}""" -> "'default_roles': unknown role 'root'",
    """{"people":[]}""" -> "'people' must be an object",
    """{"roles":{"r":{}}}""" -> "'may' must be an array",
    """{"roles":{"r":{"may":["shell"]}}}""" -> "must start with files, exec, network, plugin or be *",
    """{"roles":{"r":{"may":["plugin:de*"]}}}""" -> "may only use * for a whole part",
    """{"roles":{"r":{"may":["files:execute"]}}}""" -> "must be files, files:read or files:write",
    """{"roles":{"r":{"may":["network:post"]}}}""" -> "must be network, network:fetch or network:send",
    """{"roles":{"r":{"may":["exec:git"]}}}""" -> "name the commands in 'items'",
    """{"roles":{"r":{"may":["plugin:"]}}}""" -> "has an empty part",
    """{"roles":{"r":{"may":[{"items":["git"]}]}}}""" -> "a rule needs a 'permission'",
    """{"roles":{"r":{"may":[{"permission":"exec","items":[]}]}}}""" -> "'items' of exec must be a non-empty array",
    """{"roles":{"r":{"may":[{"permission":"*","items":["x"]}]}}}""" -> "'items' needs a kind of permission",
    """{"roles":{"r":{"may":[{"permission":"files","items":["data"]}]}}}""" -> "must be absolute or start with ~/",
    """{"roles":{"r":{"may":[42]}}}""" -> "a rule must be a permission or an object",
    """[]""" -> "not a valid JSON object"
  ).foreach: (json, fragment) =>
    test(s"rejects $json"):
      rejected(json, fragment)

  // --- reloading ---

  private val adminJson =
    """{"roles": {"admin": {"may": ["*"]}}, "people": {"p": {"ids": ["slack:U1"], "roles": ["admin"]}}}"""

  test("a changed file applies without a restart"):
    val file = os.temp(adminJson, suffix = ".json")
    val source = IdentitiesFile(file.toIO)
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set("admin")
    )
    os.write.over(
      file,
      adminJson.replace("\"roles\": [\"admin\"]", "\"roles\": []")
    )
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set.empty[String]
    )

  test("a change that does not load keeps the previous identities"):
    val file = os.temp(adminJson, suffix = ".json")
    val source = IdentitiesFile(file.toIO)
    os.write.over(file, adminJson.replace("[\"admin\"]", "[\"astronaut\"]"))
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set("admin")
    )
    os.write.over(file, "{ not json")
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set("admin")
    )
    os.write.over(file, """{}""")
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set.empty[String]
    )

  test("a deleted file links nobody, except the operator stays allowed"):
    val file = os.temp(adminJson, suffix = ".json")
    val source = IdentitiesFile(file.toIO, operator = Some("cli:op"))
    os.remove(file)
    val ids = source.current()
    assertEquals(ids.person(slack, UserId("U1")).roles, Set.empty[String])
    assert(ids.person(PortId("cli"), UserId("op")).operator)

  test("a file that cannot be read keeps the previous identities"):
    assume(System.getProperty("user.name") != "root", "root reads anything")
    val file = os.temp(adminJson, suffix = ".json")
    val source = IdentitiesFile(file.toIO)
    os.write.over(file, adminJson.replace("[\"admin\"]", "[]"))
    os.perms.set(file, "---------")
    try
      assertEquals(
        source.current().person(slack, UserId("U1")).roles,
        Set("admin")
      )
    finally os.perms.set(file, "rw-------")
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set.empty[String],
      "read again once readable"
    )

  test("a change of the same size is noticed by its modification time"):
    val file = os.temp(adminJson, suffix = ".json")
    val source = IdentitiesFile(file.toIO)
    val before = os.mtime(file)
    val renamed = adminJson.replace("\"admin\"", "\"xdmin\"")
    os.write.over(file, renamed)
    os.mtime.set(file, before + 2000)
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set("xdmin")
    )

  test("a same-size edit within the same timestamp is noticed by content"):
    val file = os.temp(adminJson, suffix = ".json")
    val source = IdentitiesFile(file.toIO)
    val before = os.mtime(file)
    os.write.over(file, adminJson.replace("\"admin\"", "\"xdmin\""))
    os.mtime.set(file, before) // as a coarse file system would record it
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set("xdmin")
    )

  test("a change that is not valid UTF-8 keeps the previous identities"):
    val file = os.temp(adminJson, suffix = ".json")
    val source = IdentitiesFile(file.toIO)
    os.write.over(file, Array[Byte](0x7b, 0xff.toByte, 0x7d))
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set("admin")
    )
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set("admin")
    )
    os.write.over(file, "{}")
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set.empty[String]
    )

  test("a directory that is not a valid path is a config error"):
    val json =
      """{"roles":{"r":{"may":[{"permission":"files","items":["/a\u0000b"]}]}}}"""
    rejected(json, "is not a valid path")
    val file = os.temp(adminJson, suffix = ".json")
    val source = IdentitiesFile(file.toIO)
    os.write.over(file, json)
    assertEquals(
      source.current().person(slack, UserId("U1")).roles,
      Set("admin"),
      "a reload keeps the previous identities"
    )

  test("invalid UTF-8 at startup is a config error"):
    val file = os.temp(Array[Byte](0x7b, 0xff.toByte, 0x7d), suffix = ".json")
    val ex = intercept[ConfigError](IdentitiesFile(file.toIO))
    assert(ex.getMessage.contains("is not valid UTF-8"), ex.getMessage)

  test("a file that does not load at startup is an error"):
    val file = os.temp("{ not json", suffix = ".json")
    intercept[ConfigError](IdentitiesFile(file.toIO))
