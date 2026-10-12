package capybaraclaw.util

import java.nio.file.Paths

class ClawHomeSuite extends munit.FunSuite:
  private val home = "/home/op"

  test("unset or blank means ~/.claw"):
    assertEquals(ClawHome.resolve(None, home), Paths.get("/home/op/.claw"))
    assertEquals(ClawHome.resolve(Some("  "), home), Paths.get("/home/op/.claw"))

  test("an absolute path is used as it is, normalized"):
    assertEquals(ClawHome.resolve(Some("/srv/claw/../demo"), home), Paths.get("/srv/demo"))

  test("a leading ~ is the user's home"):
    assertEquals(ClawHome.resolve(Some("~/demo-claw"), home), Paths.get("/home/op/demo-claw"))
    assertEquals(ClawHome.resolve(Some("~"), home), Paths.get("/home/op"))

  test("a relative path is taken from the current directory"):
    assertEquals(
      ClawHome.resolve(Some("demo/.claw-home"), home),
      Paths.get("demo/.claw-home").toAbsolutePath.normalize
    )
