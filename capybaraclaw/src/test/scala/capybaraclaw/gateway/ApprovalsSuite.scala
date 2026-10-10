package capybaraclaw.gateway

import Permission.*

class ApprovalsSuite extends munit.FunSuite:
  private val s1 = SessionId.random()
  private val s2 = SessionId.random()
  private val g1 = Grantee(s1, "alice")
  private val g2 = Grantee(s2, "alice")
  private val bob = Grantee(s1, "bob")

  private def approved(
      permission: Permission,
      grantee: Grantee = g1
  ): Approvals =
    val (pending, request, _) = Approvals.empty.request(grantee, permission)
    pending.resolve(request, ApprovalDecision.Approve)

  test(
    "requests get increasing ids, and a repeated permission reuses its request"
  ):
    val (a1, r1, new1) =
      Approvals.empty.request(g1, Files("/data", FileAccess.ReadWrite))
    val (a2, r2, new2) = a1.request(g1, Commands(Set("git")))
    val (a3, r3, new3) = a2.request(g1, Files("/data", FileAccess.ReadWrite))
    assertEquals((r1.id, r2.id), (1, 2))
    assertEquals((new1, new2, new3), (true, true, false))
    assertEquals(r3, r1)
    assertEquals(a3, a2)

  test("an approved root covers everything below it, for that grantee only"):
    val approvals = approved(Files("/data", FileAccess.ReadWrite))
    assert(approvals.pending.isEmpty)
    assertEquals(
      approvals.ungranted(g1, Files("/data", FileAccess.ReadWrite)),
      None
    )
    assertEquals(
      approvals
        .ungranted(g1, Files("/data/reports/q3.xlsx", FileAccess.ReadWrite)),
      None
    )
    assertEquals(
      approvals.ungranted(g1, Files("/database", FileAccess.ReadWrite)),
      Some(Files("/database", FileAccess.ReadWrite)),
      "a shared name prefix is not a subpath"
    )
    assertEquals(
      approvals.ungranted(g2, Files("/data", FileAccess.ReadWrite)),
      Some(Files("/data", FileAccess.ReadWrite)),
      "the same person in another session"
    )
    assertEquals(
      approvals.ungranted(bob, Files("/data", FileAccess.ReadWrite)),
      Some(Files("/data", FileAccess.ReadWrite)),
      "another person in the same session"
    )

  test("only the commands not granted yet are ungranted"):
    val approvals = approved(Commands(Set("git")))
    assertEquals(approvals.ungranted(g1, Commands(Set("git"))), None)
    assertEquals(
      approvals.ungranted(g1, Commands(Set("git", "sbt"))),
      Some(Commands(Set("sbt")))
    )

  test("only the hosts not granted yet are ungranted"):
    val approvals = approved(Hosts(Set("api.github.com"), NetworkAccess.Send))
    assertEquals(
      approvals.ungranted(
        g1,
        Hosts(Set("api.github.com", "evil.org"), NetworkAccess.Send)
      ),
      Some(Hosts(Set("evil.org"), NetworkAccess.Send))
    )
    assertEquals(
      approvals.ungranted(g1, Commands(Set("api.github.com"))),
      Some(Commands(Set("api.github.com"))),
      "a host grant does not grant a command"
    )

  test("denying removes the request without granting"):
    val (pending, request, _) =
      Approvals.empty.request(g1, Files("/data", FileAccess.ReadWrite))
    val denied = pending.resolve(request, ApprovalDecision.Deny)
    assert(denied.pending.isEmpty)
    assertEquals(
      denied.ungranted(g1, Files("/data", FileAccess.ReadWrite)),
      Some(Files("/data", FileAccess.ReadWrite))
    )

  test("without an id, the grantee's latest request is answerable"):
    val (a1, _, _) =
      Approvals.empty.request(g1, Files("/first", FileAccess.ReadWrite))
    val (a2, latest, _) =
      a1.request(g1, Hosts(Set("example.com"), NetworkAccess.Send))
    val (a3, _, _) = a2.request(g2, Files("/elsewhere", FileAccess.ReadWrite))
    val (a4, _, _) = a3.request(bob, Files("/bobs", FileAccess.ReadWrite))
    assertEquals(a4.answerable(g1, None), Right(latest))
    assertEquals(
      a4.answerable(Grantee(s1, "carol"), None),
      Left("You have no pending permission requests in this session.")
    )

  test("a session cannot answer another session's request"):
    val (pending, request, _) =
      Approvals.empty.request(g1, Files("/data", FileAccess.ReadWrite))
    assertEquals(
      pending.answerable(g2, Some(request.id)),
      Left(s"No pending permission request #${request.id} in this session.")
    )
    assertEquals(
      pending.answerable(g2, None),
      Left("You have no pending permission requests in this session.")
    )

  test("only the requester can answer a request"):
    val (pending, request, _) =
      Approvals.empty.request(g1, Files("/data", FileAccess.ReadWrite))
    assertEquals(
      pending.answerable(bob, Some(request.id)),
      Left(s"Permission request #${request.id} is not yours to answer.")
    )
    assertEquals(pending.answerable(g1, Some(request.id)), Right(request))

  test("the same permission asked by two people is two requests"):
    val (a1, r1, _) = Approvals.empty.request(g1, Commands(Set("git")))
    val (a2, r2, isNew) = a1.request(bob, Commands(Set("git")))
    assert(isNew)
    assertNotEquals(r1.id, r2.id)
    assertEquals((r1.requester, r2.requester), ("alice", "bob"))
    val granted = a2.resolve(r2, ApprovalDecision.Approve)
    assertEquals(granted.ungranted(bob, Commands(Set("git"))), None)
    assertEquals(
      granted.ungranted(g1, Commands(Set("git"))),
      Some(Commands(Set("git")))
    )

  test("descriptions quote what is asked for"):
    assertEquals(
      Files("/my data", FileAccess.Read).describe,
      "read access to files under \"/my data\""
    )
    assertEquals(
      Commands(Set("sbt", "git")).describe,
      "running \"git\", \"sbt\" with any arguments"
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
      readWrite.ungranted(g1, Files("/data/x", FileAccess.Read)),
      None
    )
    val read = approved(Files("/data", FileAccess.Read))
    assertEquals(read.ungranted(g1, Files("/data/x", FileAccess.Read)), None)
    assertEquals(
      read.ungranted(g1, Files("/data/x", FileAccess.ReadWrite)),
      Some(Files("/data/x", FileAccess.ReadWrite))
    )

  test("a send grant covers fetching, a fetch grant does not cover sending"):
    val send = approved(Hosts(Set("a.com"), NetworkAccess.Send))
    assertEquals(
      send.ungranted(g1, Hosts(Set("a.com"), NetworkAccess.Fetch)),
      None
    )
    val fetch = approved(Hosts(Set("a.com"), NetworkAccess.Fetch))
    assertEquals(
      fetch.ungranted(g1, Hosts(Set("a.com", "b.com"), NetworkAccess.Send)),
      Some(Hosts(Set("a.com", "b.com"), NetworkAccess.Send))
    )

  test("a plugin grant covers its items for that plugin and permission only"):
    val approvals = approved(Plugin("demo", "read payroll", Set("q3.xlsx")))
    assertEquals(
      approvals.ungranted(g1, Plugin("demo", "read payroll", Set("q3.xlsx"))),
      None
    )
    assertEquals(
      approvals.ungranted(
        g1,
        Plugin("demo", "read payroll", Set("q3.xlsx", "q4.xlsx"))
      ),
      Some(Plugin("demo", "read payroll", Set("q4.xlsx")))
    )
    assertEquals(
      approvals.ungranted(g1, Plugin("demo", "read payroll", Set.empty)),
      Some(Plugin("demo", "read payroll", Set.empty)),
      "a grant for some items does not cover the permission as a whole"
    )
    assertEquals(
      approvals.ungranted(g1, Plugin("demo", "edit payroll", Set("q3.xlsx"))),
      Some(Plugin("demo", "edit payroll", Set("q3.xlsx")))
    )
    assertEquals(
      approvals.ungranted(g1, Plugin("other", "read payroll", Set("q3.xlsx"))),
      Some(Plugin("other", "read payroll", Set("q3.xlsx")))
    )

  test("a plugin permission without items needs its own grant"):
    assertEquals(
      Approvals.empty.ungranted(g1, Plugin("demo", "sync", Set.empty)),
      Some(Plugin("demo", "sync", Set.empty))
    )

  test("a plugin grant without items covers only requests without items"):
    val approvals = approved(Plugin("demo", "sync", Set.empty))
    assertEquals(
      approvals.ungranted(g1, Plugin("demo", "sync", Set.empty)),
      None
    )
    assertEquals(
      approvals.ungranted(g1, Plugin("demo", "sync", Set("x"))),
      Some(Plugin("demo", "sync", Set("x")))
    )
