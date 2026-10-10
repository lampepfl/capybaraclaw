package capybaraclaw.gateway

import capybaraclaw.gateway.port.slack.SlackPort

class ApprovalBrokerSuite extends munit.FunSuite:
  private val sessionId = SessionId.random()
  private val origin =
    Origin(SlackPort.Id, UserId("U_alice"), SessionRef.Direct(sessionId))

  private def request(
      root: String,
      kind: String = "filesystem",
      resolved: Option[String] = None
  ): String =
    ujson.write(
      ujson.Obj(
        "kind" -> kind,
        "root" -> root,
        "resolved" -> resolved.getOrElse(root)
      )
    )

  private def askingBroker(): (ApprovalBroker, FakePort) =
    val broker = TestIdentities.broker()
    val port = FakePort(SlackPort.Id, supportsApprovals = true)
    broker.beginTurn(sessionId, port, origin)
    (broker, port)

  private def answer(json: String): (Boolean, Option[String]) =
    val obj = ujson.read(json).obj
    (obj("allow").bool, obj.get("message").map(_.str))

  test("asks the user on a port that supports approvals and denies meanwhile"):
    val (broker, port) = askingBroker()
    val (allowed, message) = answer(broker.oracle(sessionId)(request("/data")))
    assert(!allowed)
    assert(message.exists(_.contains("permission request #1")), message)
    assertEquals(
      Option(port.approvalRequests.poll()),
      Some(
        ApprovalRequest(
          1,
          sessionId,
          "slack:U_alice",
          Permission.Files("/data", Permission.FileAccess.ReadWrite)
        )
      )
    )

  test("denies without asking on a port that cannot ask"):
    val broker = TestIdentities.broker()
    val port = FakePort(SlackPort.Id)
    broker.beginTurn(sessionId, port, origin)
    val (allowed, message) = answer(broker.oracle(sessionId)(request("/data")))
    assert(!allowed)
    assert(message.exists(_.contains("cannot ask the user")), message)
    assert(port.approvalRequests.isEmpty)
    assertEquals(
      broker.resolve(sessionId, None, ApprovalDecision.Approve, origin),
      Left("You have no pending permission requests in this session.")
    )

  test("denies outside a turn, when there is nobody to ask"):
    val broker = TestIdentities.broker()
    val (allowed, _) = answer(broker.oracle(sessionId)(request("/data")))
    assert(!allowed)

  test("allows a root below an approved one"):
    val (broker, _) = askingBroker()
    val _ = broker.oracle(sessionId)(request("/data"))
    assertEquals(
      broker
        .resolve(sessionId, Some(1), ApprovalDecision.Approve, origin)
        .map(_.request.permission),
      Right(Permission.Files("/data", Permission.FileAccess.ReadWrite))
    )
    assertEquals(
      answer(broker.oracle(sessionId)(request("/data/q3"))),
      (true, None)
    )

  test("denies requests it does not understand"):
    val (broker, port) = askingBroker()
    assert(
      !answer(broker.oracle(sessionId)(request("example.com", "telepathy")))._1
    )
    assert(!answer(broker.oracle(sessionId)("not json"))._1)
    assert(port.approvalRequests.isEmpty)

  test("a repeated request reuses the pending one and does not prompt again"):
    val (broker, port) = askingBroker()
    val first = answer(broker.oracle(sessionId)(request("/data")))
    val second = answer(broker.oracle(sessionId)(request("/data")))
    assertEquals(first, second)
    assertEquals(port.approvalRequests.size, 1)

  test("a relative root is denied without asking"):
    val (broker, port) = askingBroker()
    val (allowed, message) =
      answer(
        broker.oracle(sessionId)(request("src", resolved = Some("/srv/src")))
      )
    assert(!allowed)
    assert(message.exists(_.contains("absolute path")), message)
    assert(port.approvalRequests.isEmpty)

  List(
    "terminal escapes" -> "/data\u001b[2K\rfine",
    "a bidi override" -> "/data/\u202Etxt.exe",
    "a zero-width space" -> "/da\u200Bta",
    "a line separator" -> "/data\u2028fine"
  ).foreach: (what, path) =>
    test(s"a path with $what is denied without asking"):
      val (broker, port) = askingBroker()
      val (allowed, message) = answer(broker.oracle(sessionId)(request(path)))
      assert(!allowed)
      assert(message.exists(_.contains("non-printable characters")), message)
      assert(port.approvalRequests.isEmpty)

  test("a grant in one session does not let another session through"):
    val (broker, port) = askingBroker()
    val other = SessionId.random()
    broker.beginTurn(other, port, origin)
    val _ = broker.oracle(sessionId)(request("/data"))
    val _ = broker.resolve(sessionId, Some(1), ApprovalDecision.Approve, origin)
    assertEquals(answer(broker.oracle(sessionId)(request("/data")))._1, true)
    assertEquals(answer(broker.oracle(other)(request("/data")))._1, false)

  test("the denial message quotes the path"):
    val (broker, _) = askingBroker()
    val (_, message) = answer(broker.oracle(sessionId)(request("/my data")))
    assert(
      message.exists(
        _.contains("Read and write access to files under \"/my data\" needs")
      ),
      message
    )

  private def itemsRequest(kind: String, items: String*): String =
    ujson.write(
      ujson.Obj("kind" -> kind, "items" -> ujson.Arr(items.map(ujson.Str(_))*))
    )

  test("asks only for the commands that are not granted yet"):
    val (broker, port) = askingBroker()
    val _ = broker.oracle(sessionId)(itemsRequest("exec", "git"))
    val _ = broker.resolve(sessionId, Some(1), ApprovalDecision.Approve, origin)
    val (allowed, message) =
      answer(broker.oracle(sessionId)(itemsRequest("exec", "git", "sbt")))
    assert(!allowed)
    assert(
      message.exists(_.contains("Running \"sbt\" with any arguments needs")),
      message
    )
    assertEquals(
      port.approvalRequests.toArray.toList
        .map(_.asInstanceOf[ApprovalRequest].permission),
      List(Permission.Commands(Set("git")), Permission.Commands(Set("sbt")))
    )
    assertEquals(
      answer(broker.oracle(sessionId)(itemsRequest("exec", "git")))._1,
      true
    )

  test("asks for network hosts"):
    val (broker, port) = askingBroker()
    val (allowed, _) =
      answer(
        broker.oracle(sessionId)(itemsRequest("network", "api.github.com"))
      )
    assert(!allowed)
    assertEquals(
      Option(port.approvalRequests.poll()).map(_.permission),
      Some(
        Permission.Hosts(Set("api.github.com"), Permission.NetworkAccess.Send)
      )
    )

  test("a command with non-printable characters is denied without asking"):
    val (broker, port) = askingBroker()
    val (allowed, _) =
      answer(broker.oracle(sessionId)(itemsRequest("exec", "git\u202E")))
    assert(!allowed)
    assert(port.approvalRequests.isEmpty)

  List(
    "a wildcard host" -> itemsRequest("network", "api.github.com*"),
    "a whole-network host" -> itemsRequest("network", "*"),
    "a wildcard command" -> itemsRequest("exec", "s*"),
    "an empty command" -> itemsRequest("exec", " ")
  ).foreach: (what, json) =>
    test(s"$what is denied without asking"):
      val (broker, port) = askingBroker()
      assert(!answer(broker.oracle(sessionId)(json))._1)
      assert(port.approvalRequests.isEmpty)

  test("the access level and reason reach the user"):
    val (broker, port) = askingBroker()
    val json = ujson.write(
      ujson.Obj(
        "kind" -> "filesystem",
        "root" -> "/data",
        "resolved" -> "/data",
        "access" -> "read",
        "reason" -> "to summarize Q3"
      )
    )
    val (_, message) = answer(broker.oracle(sessionId)(json))
    assert(message.exists(_.startsWith("Read access to files under")), message)
    val request = port.approvalRequests.poll().nn
    assertEquals(
      request.permission,
      Permission.Files("/data", Permission.FileAccess.Read)
    )
    assertEquals(request.reason, "to summarize Q3")

  test("fetch-only network requests keep their access level"):
    val (broker, port) = askingBroker()
    val json = ujson.write(
      ujson.Obj(
        "kind" -> "network",
        "items" -> ujson.Arr("a.com"),
        "access" -> "fetch"
      )
    )
    val _ = broker.oracle(sessionId)(json)
    assertEquals(
      port.approvalRequests.poll().nn.permission,
      Permission.Hosts(Set("a.com"), Permission.NetworkAccess.Fetch)
    )

  test("an overlong or non-printable reason is denied without asking"):
    val (broker, port) = askingBroker()
    def withReason(reason: String) = ujson.write(
      ujson.Obj(
        "kind" -> "exec",
        "items" -> ujson.Arr("git"),
        "reason" -> reason
      )
    )
    assert(!answer(broker.oracle(sessionId)(withReason("x" * 201)))._1)
    assert(!answer(broker.oracle(sessionId)(withReason("ok\u001b[2Kfine")))._1)
    assert(port.approvalRequests.isEmpty)

  test("a request without an access level asks for the widest access"):
    val (broker, port) = askingBroker()
    val _ = broker.oracle(sessionId)(request("/data"))
    val _ = broker.oracle(sessionId)(itemsRequest("network", "a.com"))
    assertEquals(
      port.approvalRequests.toArray.toList
        .map(_.asInstanceOf[ApprovalRequest].permission),
      List(
        Permission.Files("/data", Permission.FileAccess.ReadWrite),
        Permission.Hosts(Set("a.com"), Permission.NetworkAccess.Send)
      )
    )

  private def pluginRequest(
      plugin: String,
      permission: String,
      items: String*
  ): String =
    ujson.write(
      ujson.Obj(
        "kind" -> "plugin",
        "plugin" -> plugin,
        "permission" -> permission,
        "items" -> ujson.Arr(items.map(ujson.Str(_))*),
        "reason" -> "to review"
      )
    )

  test("a plugin's request is asked with its plugin, permission and items"):
    val (broker, port) = askingBroker()
    val (allowed, message) = answer(
      broker.oracle(sessionId)(pluginRequest("demo", "read payroll", "q3.xlsx"))
    )
    assert(!allowed)
    assert(
      message.exists(
        _.contains(
          "Plugin \"demo\" permission \"read payroll\" for \"q3.xlsx\""
        )
      ),
      message
    )
    val request = port.approvalRequests.poll().nn
    assertEquals(
      request.permission,
      Permission.Plugin("demo", "read payroll", Set("q3.xlsx"))
    )
    assertEquals(request.reason, "to review")

  test("a plugin request with a blank plugin id is denied without asking"):
    val (broker, port) = askingBroker()
    assert(!answer(broker.oracle(sessionId)(pluginRequest(" ", "read")))._1)
    assert(port.approvalRequests.isEmpty)

  test("a plugin request with malformed items is denied without asking"):
    val (broker, port) = askingBroker()
    val json = ujson.write(
      ujson.Obj(
        "kind" -> "plugin",
        "plugin" -> "demo",
        "permission" -> "read",
        "items" -> "q3.xlsx"
      )
    )
    assert(!answer(broker.oracle(sessionId)(json))._1)
    assert(port.approvalRequests.isEmpty)

  test("a plugin request without items asks for the permission as a whole"):
    val (broker, port) = askingBroker()
    val json = ujson.write(
      ujson.Obj("kind" -> "plugin", "plugin" -> "demo", "permission" -> "read")
    )
    assert(!answer(broker.oracle(sessionId)(json))._1)
    assertEquals(
      port.approvalRequests.poll().nn.permission,
      Permission.Plugin("demo", "read", Set.empty)
    )

  test("a request the port fails to show is dropped and asked again"):
    val broker = TestIdentities.broker()
    val failures = java.util.concurrent.atomic.AtomicInteger(1)
    val port = new FakePort(SlackPort.Id, supportsApprovals = true):
      override def requestApproval(
          sessionId: SessionId,
          origin: Origin,
          request: ApprovalRequest
      ): Unit =
        if failures.getAndDecrement() > 0 then
          throw RuntimeException("slack is down")
        super.requestApproval(sessionId, origin, request)
    broker.beginTurn(sessionId, port, origin)
    val (allowed, message) = answer(broker.oracle(sessionId)(request("/data")))
    assert(!allowed)
    assert(message.exists(_.contains("could not reach the user")), message)
    assertEquals(
      broker.resolve(sessionId, None, ApprovalDecision.Approve, origin),
      Left("You have no pending permission requests in this session.")
    )
    val (retried, _) = answer(broker.oracle(sessionId)(request("/data")))
    assert(!retried)
    assertEquals(
      Option(port.approvalRequests.poll()).map(_.permission),
      Some(Permission.Files("/data", Permission.FileAccess.ReadWrite))
    )

  test("a request naming too much is denied without asking"):
    val (broker, port) = askingBroker()
    val many = (1 to 300).map(i => s"host-$i.example.com")
    assert(!answer(broker.oracle(sessionId)(itemsRequest("network", many*)))._1)
    assert(port.approvalRequests.isEmpty)

  // --- roles ---

  private val bobOrigin =
    Origin(SlackPort.Id, UserId("U_bob"), SessionRef.Direct(sessionId))

  private def identities(json: String): Identities =
    Identities.parse(json, "identities.json")

  /** alice may approve files and git; bob only fetching; others nothing. */
  private val teamJson =
    """{
      "roles": {
        "ops": {"may": ["files", {"permission": "exec", "items": ["git"]}]},
        "dev": {"may": ["network:fetch"]}
      },
      "people": {
        "alice": {"ids": ["slack:U_alice"], "roles": ["ops"]},
        "bob": {"ids": ["slack:U_bob"], "roles": ["dev"]}
      }
    }"""

  private def teamBroker(
      json: String = teamJson
  ): (
      ApprovalBroker,
      FakePort,
      java.util.concurrent.atomic.AtomicReference[Identities]
  ) =
    val ids = java.util.concurrent.atomic.AtomicReference(identities(json))
    val broker = ApprovalBroker(() => ids.get())
    val port = FakePort(SlackPort.Id, supportsApprovals = true)
    broker.beginTurn(sessionId, port, origin)
    (broker, port, ids)

  test(
    "a permission the requester's roles do not allow is denied without asking"
  ):
    val (broker, port, _) = teamBroker()
    val (allowed, message) =
      answer(broker.oracle(sessionId)(itemsRequest("network", "a.com")))
    assert(!allowed)
    assert(
      message.exists(
        _.contains(
          "Denied sending data to \"a.com\": the roles of alice do not allow it"
        )
      ),
      message
    )
    assert(port.approvalRequests.isEmpty)

  test("the denial names the part of a request the roles do not allow"):
    val (broker, port, _) = teamBroker()
    val (allowed, message) =
      answer(broker.oracle(sessionId)(itemsRequest("exec", "git", "rm")))
    assert(!allowed)
    assert(
      message.exists(_.contains("Denied running \"rm\" with any arguments")),
      message
    )
    assert(port.approvalRequests.isEmpty)

  test("someone not listed gets the default roles"):
    val (broker, port, _) = teamBroker(
      """{"roles": {"guest": {"may": ["network:fetch"]}}, "default_roles": ["guest"]}"""
    )
    val fetch = ujson.write(
      ujson.Obj(
        "kind" -> "network",
        "items" -> ujson.Arr("a.com"),
        "access" -> "fetch"
      )
    )
    assert(!answer(broker.oracle(sessionId)(fetch))._1)
    assertEquals(port.approvalRequests.size, 1)
    assert(!answer(broker.oracle(sessionId)(request("/data")))._1)
    assertEquals(port.approvalRequests.size, 1, "files are not asked for")

  test("with no roles at all, nobody but the operator is asked"):
    val broker = ApprovalBroker(() => Identities.empty.withOperator("cli:op"))
    val port = FakePort(PortId("cli"), supportsApprovals = true)
    broker.beginTurn(sessionId, port, origin)
    assert(!answer(broker.oracle(sessionId)(request("/data")))._1)
    assert(port.approvalRequests.isEmpty)
    val operator =
      Origin(PortId("cli"), UserId("op"), SessionRef.Direct(sessionId))
    broker.beginTurn(sessionId, port, operator)
    assert(!answer(broker.oracle(sessionId)(request("/data")))._1)
    assertEquals(port.approvalRequests.size, 1)
    assert(
      broker
        .resolve(sessionId, Some(1), ApprovalDecision.Approve, operator)
        .isRight
    )
    assertEquals(answer(broker.oracle(sessionId)(request("/data")))._1, true)

  test("a grant covers its requester only, not others in the same session"):
    val (broker, port, _) = teamBroker(
      """{"roles": {"all": {"may": ["*"]}}, "default_roles": ["all"]}"""
    )
    val _ = broker.oracle(sessionId)(request("/data"))
    val _ = broker.resolve(sessionId, Some(1), ApprovalDecision.Approve, origin)
    assertEquals(answer(broker.oracle(sessionId)(request("/data")))._1, true)
    broker.beginTurn(sessionId, port, bobOrigin)
    val (allowed, message) = answer(broker.oracle(sessionId)(request("/data")))
    assert(!allowed)
    assert(message.exists(_.contains("permission request #2")), message)
    assertEquals(port.approvalRequests.poll().nn.requester, "slack:U_alice")
    assertEquals(port.approvalRequests.poll().nn.requester, "slack:U_bob")

  test("only the requester can answer a request"):
    val (broker, _, _) = teamBroker()
    val _ = broker.oracle(sessionId)(request("/data"))
    assertEquals(
      broker.resolve(sessionId, Some(1), ApprovalDecision.Approve, bobOrigin),
      Left("Permission request #1 is not yours to answer.")
    )
    assertEquals(
      broker.resolve(sessionId, None, ApprovalDecision.Approve, bobOrigin),
      Left("You have no pending permission requests in this session.")
    )
    assert(
      broker
        .resolve(sessionId, Some(1), ApprovalDecision.Approve, origin)
        .isRight
    )

  test("a grant stops counting as soon as its holder loses the role"):
    val (broker, _, ids) = teamBroker()
    val _ = broker.oracle(sessionId)(request("/data"))
    val _ = broker.resolve(sessionId, Some(1), ApprovalDecision.Approve, origin)
    assertEquals(answer(broker.oracle(sessionId)(request("/data/q3")))._1, true)
    ids.set(
      identities(
        teamJson.replace("\"roles\": [\"ops\"]", "\"roles\": [\"dev\"]")
      )
    )
    val (allowed, message) =
      answer(broker.oracle(sessionId)(request("/data/q3")))
    assert(!allowed)
    assert(
      message.exists(_.contains("the roles of alice do not allow it")),
      message
    )

  test("approving after losing the role withdraws the request without a grant"):
    val (broker, _, ids) = teamBroker()
    val _ = broker.oracle(sessionId)(request("/data"))
    ids.set(
      identities(
        teamJson.replace("\"roles\": [\"ops\"]", "\"roles\": [\"dev\"]")
      )
    )
    val resolution =
      broker.resolve(sessionId, Some(1), ApprovalDecision.Approve, origin)
    resolution match
      case Right(ApprovalResolution.Withdrawn(request, reason)) =>
        assertEquals(request.id, 1)
        assert(reason.contains("the roles of alice no longer allow"), reason)
      case other => fail(s"expected a withdrawal, got $other")
    ids.set(identities(teamJson))
    val (allowed, message) = answer(broker.oracle(sessionId)(request("/data")))
    assert(!allowed, "the withdrawn request granted nothing")
    assert(message.exists(_.contains("permission request #2")), message)

  test("denying is always possible, even after losing every role"):
    val (broker, _, ids) = teamBroker()
    val _ = broker.oracle(sessionId)(request("/data"))
    ids.set(
      identities(teamJson.replace("\"roles\": [\"ops\"]", "\"roles\": []"))
    )
    broker.resolve(sessionId, Some(1), ApprovalDecision.Deny, origin) match
      case Right(ApprovalResolution.Answered(_, ApprovalDecision.Deny)) => ()
      case other => fail(s"expected a denial, got $other")

  test("outside a turn nothing beyond the defaults is allowed, granted or not"):
    val (broker, _) = askingBroker()
    val _ = broker.oracle(sessionId)(request("/data"))
    val _ = broker.resolve(sessionId, Some(1), ApprovalDecision.Approve, origin)
    broker.endTurn(sessionId)
    val (allowed, message) = answer(broker.oracle(sessionId)(request("/data")))
    assert(!allowed)
    assert(message.exists(_.contains("outside a turn")), message)
