package capybaraclaw.gateway

import Permission.*

class ApprovalsSuite extends munit.FunSuite:
  private val s1 = SessionId.random()
  private val s2 = SessionId.random()

  private def approved(
      permission: Permission,
      session: SessionId = s1
  ): Approvals =
    val (pending, request, _) = Approvals.empty.request(session, permission)
    val Right((approvals, _)) =
      pending.resolve(
        session,
        Some(request.id),
        ApprovalDecision.Approve
      ): @unchecked
    approvals

  test(
    "requests get increasing ids, and a repeated permission reuses its request"
  ):
    val (a1, r1, new1) =
      Approvals.empty.request(s1, Files("/data", FileAccess.ReadWrite))
    val (a2, r2, new2) = a1.request(s1, Commands(Set("git")))
    val (a3, r3, new3) = a2.request(s1, Files("/data", FileAccess.ReadWrite))
    assertEquals((r1.id, r2.id), (1, 2))
    assertEquals((new1, new2, new3), (true, true, false))
    assertEquals(r3, r1)
    assertEquals(a3, a2)

  test("an approved root covers everything below it, in that session only"):
    val approvals = approved(Files("/data", FileAccess.ReadWrite))
    assert(approvals.pending.isEmpty)
    assertEquals(
      approvals.ungranted(s1, Files("/data", FileAccess.ReadWrite)),
      None
    )
    assertEquals(
      approvals
        .ungranted(s1, Files("/data/reports/q3.xlsx", FileAccess.ReadWrite)),
      None
    )
    assertEquals(
      approvals.ungranted(s1, Files("/database", FileAccess.ReadWrite)),
      Some(Files("/database", FileAccess.ReadWrite)),
      "a shared name prefix is not a subpath"
    )
    assertEquals(
      approvals.ungranted(s2, Files("/data", FileAccess.ReadWrite)),
      Some(Files("/data", FileAccess.ReadWrite))
    )

  test("only the commands not granted yet are ungranted"):
    val approvals = approved(Commands(Set("git")))
    assertEquals(approvals.ungranted(s1, Commands(Set("git"))), None)
    assertEquals(
      approvals.ungranted(s1, Commands(Set("git", "sbt"))),
      Some(Commands(Set("sbt")))
    )

  test("only the hosts not granted yet are ungranted"):
    val approvals = approved(Hosts(Set("api.github.com"), NetworkAccess.Send))
    assertEquals(
      approvals.ungranted(
        s1,
        Hosts(Set("api.github.com", "evil.org"), NetworkAccess.Send)
      ),
      Some(Hosts(Set("evil.org"), NetworkAccess.Send))
    )
    assertEquals(
      approvals.ungranted(s1, Commands(Set("api.github.com"))),
      Some(Commands(Set("api.github.com"))),
      "a host grant does not grant a command"
    )

  test("denying removes the request without granting"):
    val (pending, request, _) =
      Approvals.empty.request(s1, Files("/data", FileAccess.ReadWrite))
    val Right((denied, _)) =
      pending.resolve(s1, Some(request.id), ApprovalDecision.Deny): @unchecked
    assert(denied.pending.isEmpty)
    assertEquals(
      denied.ungranted(s1, Files("/data", FileAccess.ReadWrite)),
      Some(Files("/data", FileAccess.ReadWrite))
    )

  test("without an id, the session's latest request is answered"):
    val (a1, _, _) =
      Approvals.empty.request(s1, Files("/first", FileAccess.ReadWrite))
    val (a2, latest, _) =
      a1.request(s1, Hosts(Set("example.com"), NetworkAccess.Send))
    val (a3, _, _) = a2.request(s2, Files("/elsewhere", FileAccess.ReadWrite))
    val Right((_, resolved)) =
      a3.resolve(s1, None, ApprovalDecision.Approve): @unchecked
    assertEquals(resolved, latest)

  test("a session cannot answer another session's request"):
    val (pending, request, _) =
      Approvals.empty.request(s1, Files("/data", FileAccess.ReadWrite))
    assertEquals(
      pending.resolve(s2, Some(request.id), ApprovalDecision.Approve),
      Left(s"No pending permission request #${request.id} in this session.")
    )
    assertEquals(
      pending.resolve(s2, None, ApprovalDecision.Approve),
      Left("No pending permission requests in this session.")
    )

  test("descriptions quote what is asked for"):
    assertEquals(
      Files("/my data", FileAccess.Read).describe,
      "read access to files under \"/my data\""
    )
    assertEquals(
      Commands(Set("sbt", "git")).describe,
      "running \"git\", \"sbt\""
    )
    assertEquals(
      Hosts(Set("a.com"), NetworkAccess.Fetch).describe,
      "fetching from \"a.com\" (GET and HEAD only)"
    )

  test(
    "a read-write grant covers reading, a read grant does not cover writing"
  ):
    val readWrite = approved(Files("/data", FileAccess.ReadWrite))
    assertEquals(
      readWrite.ungranted(s1, Files("/data/x", FileAccess.Read)),
      None
    )
    val read = approved(Files("/data", FileAccess.Read))
    assertEquals(read.ungranted(s1, Files("/data/x", FileAccess.Read)), None)
    assertEquals(
      read.ungranted(s1, Files("/data/x", FileAccess.ReadWrite)),
      Some(Files("/data/x", FileAccess.ReadWrite))
    )

  test("a send grant covers fetching, a fetch grant does not cover sending"):
    val send = approved(Hosts(Set("a.com"), NetworkAccess.Send))
    assertEquals(
      send.ungranted(s1, Hosts(Set("a.com"), NetworkAccess.Fetch)),
      None
    )
    val fetch = approved(Hosts(Set("a.com"), NetworkAccess.Fetch))
    assertEquals(
      fetch.ungranted(s1, Hosts(Set("a.com", "b.com"), NetworkAccess.Send)),
      Some(Hosts(Set("a.com", "b.com"), NetworkAccess.Send))
    )

  test("a plugin grant covers its items for that plugin and permission only"):
    val approvals = approved(Plugin("demo", "read payroll", Set("q3.xlsx")))
    assertEquals(
      approvals.ungranted(s1, Plugin("demo", "read payroll", Set("q3.xlsx"))),
      None
    )
    assertEquals(
      approvals.ungranted(
        s1,
        Plugin("demo", "read payroll", Set("q3.xlsx", "q4.xlsx"))
      ),
      Some(Plugin("demo", "read payroll", Set("q4.xlsx")))
    )
    assertEquals(
      approvals.ungranted(s1, Plugin("demo", "read payroll", Set.empty)),
      Some(Plugin("demo", "read payroll", Set.empty)),
      "a grant for some items does not cover the permission as a whole"
    )
    assertEquals(
      approvals.ungranted(s1, Plugin("demo", "edit payroll", Set("q3.xlsx"))),
      Some(Plugin("demo", "edit payroll", Set("q3.xlsx")))
    )
    assertEquals(
      approvals.ungranted(s1, Plugin("other", "read payroll", Set("q3.xlsx"))),
      Some(Plugin("other", "read payroll", Set("q3.xlsx")))
    )

  test("a plugin permission without items needs its own grant"):
    assertEquals(
      Approvals.empty.ungranted(s1, Plugin("demo", "sync", Set.empty)),
      Some(Plugin("demo", "sync", Set.empty))
    )

  test("a plugin grant without items covers only requests without items"):
    val approvals = approved(Plugin("demo", "sync", Set.empty))
    assertEquals(
      approvals.ungranted(s1, Plugin("demo", "sync", Set.empty)),
      None
    )
    assertEquals(
      approvals.ungranted(s1, Plugin("demo", "sync", Set("x"))),
      Some(Plugin("demo", "sync", Set("x")))
    )
