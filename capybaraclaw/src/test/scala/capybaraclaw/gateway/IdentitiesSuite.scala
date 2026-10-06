package capybaraclaw.gateway

import capybaraclaw.agent.ConfigError

class IdentitiesSuite extends munit.FunSuite:
  private def load(json: String): Identities =
    val file = os.temp(json, prefix = "identities-", suffix = ".json")
    Identities.load(file.toIO)

  private def rejected(json: String, fragment: String): Unit =
    val ex = intercept[ConfigError](load(json))
    assert(ex.getMessage.contains(fragment), ex.getMessage)

  test("a missing file links nobody"):
    val ids = Identities.load(java.io.File("/nonexistent/identities.json"))
    assertEquals(
      ids.person(PortId("slack"), UserId("U1")),
      Person("slack:U1", Role.Member)
    )

  test("linked ids on different ports resolve to the same person"):
    val ids = load(
      """{"people":{"lukasz":{"ids":["cli:lbialy","slack:U1"],"role":"admin"}}}"""
    )
    val expected = Person("lukasz", Role.Admin)
    assertEquals(ids.person(PortId("cli"), UserId("lbialy")), expected)
    assertEquals(ids.person(PortId("slack"), UserId("U1")), expected)
    assertEquals(
      ids.person(PortId("slack"), UserId("U2")),
      Person("slack:U2", Role.Member)
    )

  test("role defaults to member"):
    val ids = load("""{"people":{"ann":{"ids":["slack:U3"]}}}""")
    assertEquals(ids.person(PortId("slack"), UserId("U3")).role, Role.Member)

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

  List(
    """{"people":{"a":{"ids":["U1"]}}}""" -> "<port>:<user>",
    """{"people":{"a":{"ids":[":U1"]}}}""" -> "<port>:<user>",
    """{"people":{"a":{"ids":["slack:"]}}}""" -> "<port>:<user>",
    """{"people":{"a":{"ids":[]}}}""" -> "'ids' must be a non-empty array",
    """{"people":{"a":{"ids":["slack:U1"],"role":"root"}}}""" -> "'role' must be",
    """{"people":[]}""" -> "'people' must be an object",
    """[]""" -> "not a valid JSON object"
  ).foreach: (json, fragment) =>
    test(s"rejects $json"):
      rejected(json, fragment)
