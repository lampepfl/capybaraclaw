package capybaraclaw.gateway.port.cli

import capybaraclaw.gateway.SessionId
import munit.FunSuite

class SessionFormattingSuite extends FunSuite:

  /** resumeCommand */

  test("resumeCommand: plain UUID is unquoted"):
    val id = SessionId("11111111-2222-3333-4444-555555555555")
    assertEquals(
      SessionFormatting.resumeCommand(id),
      "claw -r 11111111-2222-3333-4444-555555555555"
    )

  /** turnsLabel */

  test("turnsLabel: singular and plural"):
    assertEquals(SessionFormatting.turnsLabel(1), "1 turn")
    assertEquals(SessionFormatting.turnsLabel(0), "0 turns")
    assertEquals(SessionFormatting.turnsLabel(7), "7 turns")

  /** tildify */

  test("tildify: replaces home prefix with `~`"):
    assertEquals(
      SessionFormatting
        .tildify("/Users/alice/code/project", Some("/Users/alice")),
      "~/code/project"
    )

  test("tildify: matches the home directory itself"):
    assertEquals(
      SessionFormatting.tildify("/Users/alice", Some("/Users/alice")),
      "~"
    )

  test("tildify: leaves non-home paths unchanged"):
    assertEquals(
      SessionFormatting.tildify("/etc/foo", Some("/Users/alice")),
      "/etc/foo"
    )

  test("tildify: does not rewrite paths that only share a prefix"):
    assertEquals(
      SessionFormatting.tildify("/Users/alicia/project", Some("/Users/alice")),
      "/Users/alicia/project"
    )

  test("tildify: leaves path unchanged when homeDir is None"):
    assertEquals(
      SessionFormatting.tildify("/Users/alice/x", None),
      "/Users/alice/x"
    )

  /** formatSessionsList */

  test("formatSessionsList: empty list"):
    assertEquals(
      SessionFormatting.formatSessionsList(
        Nil,
        currentId = None,
        nowMillis = 0L
      ),
      "No sessions recorded."
    )

  test("formatSessionsList: single session marked as current"):
    val id = SessionId("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
    val rendered = SessionFormatting.formatSessionsList(
      entries = List((id, "/tmp/wd", 0L)),
      currentId = Some(id),
      nowMillis = 30_000L,
      homeDir = None
    )
    assertEquals(
      rendered,
      s"""|1 session:
          |
          |/tmp/wd
          |  > $id   (just now)""".stripMargin
    )

  test("formatSessionsList: groups multiple sessions in same workdir"):
    val a = SessionId("aaaaaaaa-1111-1111-1111-111111111111")
    val b = SessionId("bbbbbbbb-2222-2222-2222-222222222222")
    val rendered = SessionFormatting.formatSessionsList(
      entries =
        List((a, "/tmp/shared", 0L), (b, "/tmp/shared", -60L * 60L * 1000L)),
      currentId = None,
      nowMillis = 0L,
      homeDir = None
    )
    assertEquals(
      rendered,
      s"""|2 sessions:
          |
          |/tmp/shared
          |  $a   (just now)
          |  $b   (1h ago)""".stripMargin
    )

  test("formatSessionsList: groups across workdirs, newest group first"):
    val a = SessionId("aaaaaaaa-1111-1111-1111-111111111111")
    val b = SessionId("bbbbbbbb-2222-2222-2222-222222222222")
    val rendered = SessionFormatting.formatSessionsList(
      entries =
        List((a, "/tmp/old", -2L * 60L * 60L * 1000L), (b, "/tmp/new", 0L)),
      currentId = None,
      nowMillis = 0L,
      homeDir = None
    )
    assertEquals(
      rendered,
      s"""|2 sessions:
          |
          |/tmp/new
          |  $b   (just now)
          |
          |/tmp/old
          |  $a   (2h ago)""".stripMargin
    )

  test("formatSessionsList: tildifies paths under home"):
    val id = SessionId("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
    val rendered = SessionFormatting.formatSessionsList(
      entries = List((id, "/Users/alice/code/project", 0L)),
      currentId = None,
      nowMillis = 0L,
      homeDir = Some("/Users/alice")
    )
    assert(
      rendered.contains("~/code/project"),
      s"expected tildified path, got:\n$rendered"
    )
    assert(
      !rendered.contains("/Users/alice/code"),
      s"raw home path should be replaced, got:\n$rendered"
    )

  test("formatSessionsList: leaves non-home paths unchanged"):
    val id = SessionId("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
    val rendered = SessionFormatting.formatSessionsList(
      entries = List((id, "/etc/foo", 0L)),
      currentId = None,
      nowMillis = 0L,
      homeDir = Some("/Users/alice")
    )
    assert(rendered.contains("/etc/foo"), rendered)
