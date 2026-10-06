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
    val broker = ApprovalBroker()
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
          Permission.Files("/data", Permission.FileAccess.ReadWrite)
        )
      )
    )

  test("denies without asking on a port that cannot ask"):
    val broker = ApprovalBroker()
    val port = FakePort(SlackPort.Id)
    broker.beginTurn(sessionId, port, origin)
    val (allowed, message) = answer(broker.oracle(sessionId)(request("/data")))
    assert(!allowed)
    assert(message.exists(_.contains("cannot ask the user")), message)
    assert(port.approvalRequests.isEmpty)
    assertEquals(
      broker.resolve(sessionId, None, ApprovalDecision.Approve),
      Left("No pending permission requests in this session.")
    )

  test("denies outside a turn, when there is nobody to ask"):
    val broker = ApprovalBroker()
    val (allowed, _) = answer(broker.oracle(sessionId)(request("/data")))
    assert(!allowed)

  test("allows a root below an approved one"):
    val (broker, _) = askingBroker()
    val _ = broker.oracle(sessionId)(request("/data"))
    broker.endTurn(sessionId)
    assertEquals(
      broker
        .resolve(sessionId, Some(1), ApprovalDecision.Approve)
        .map(_.permission),
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
    val (broker, _) = askingBroker()
    val other = SessionId.random()
    val _ = broker.oracle(sessionId)(request("/data"))
    val _ = broker.resolve(sessionId, Some(1), ApprovalDecision.Approve)
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
    val _ = broker.resolve(sessionId, Some(1), ApprovalDecision.Approve)
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
    val broker = ApprovalBroker()
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
      broker.resolve(sessionId, None, ApprovalDecision.Approve),
      Left("No pending permission requests in this session.")
    )
    val (retried, _) = answer(broker.oracle(sessionId)(request("/data")))
    assert(!retried)
    assertEquals(
      Option(port.approvalRequests.poll()).map(_.permission),
      Some(Permission.Files("/data", Permission.FileAccess.ReadWrite))
    )
