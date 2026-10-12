package capybaraclaw.util

import capybaraclaw.agent.ConfigError

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, Paths}

class ClawHomeSuite extends munit.FunSuite:
  private val home = "/home/op"
  private val cwd = Paths.get("/srv/run")

  private def resolve(value: Option[String]) =
    ClawHome.resolve(value, home, cwd)

  private val tmp = FunFixture[Path](
    setup = _ => Files.createTempDirectory("claw-home").toRealPath(),
    teardown = dir =>
      Files
        .walk(dir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete)
  )

  test("unset or blank means ~/.claw"):
    assertEquals(resolve(None), Paths.get("/home/op/.claw"))
    assertEquals(resolve(Some("  ")), Paths.get("/home/op/.claw"))

  test("an absolute path is used as it is, normalized"):
    assertEquals(resolve(Some("/srv/claw/../demo")), Paths.get("/srv/demo"))

  test("~/… is under the user's home"):
    assertEquals(resolve(Some("~/demo-claw")), Paths.get("/home/op/demo-claw"))

  test("a relative path is taken from the current directory"):
    assertEquals(
      resolve(Some("demo/.claw-home")),
      Paths.get("/srv/run/demo/.claw-home")
    )
    assertEquals(resolve(Some("../state")), Paths.get("/srv/state"))

  test("~ alone and ~user are refused"):
    intercept[ConfigError](resolve(Some("~")))
    intercept[ConfigError](resolve(Some("~/")))
    intercept[ConfigError](resolve(Some("~bob/claw")))

  tmp.test("the state directory may not be inside the workdir or contain it"):
    dir =>
      val workDir = Files.createDirectories(dir.resolve("work"))
      intercept[ConfigError](
        ClawHome.requireApart(workDir, workDir.resolve(".claw"))
      )
      intercept[ConfigError](ClawHome.requireApart(workDir, dir))
      intercept[ConfigError](ClawHome.requireApart(workDir, workDir))
      ClawHome.requireApart(workDir, dir.resolve("state"))
      ClawHome.requireApart(workDir, dir.resolve("work-state"))

  tmp.test("a symlink does not hide an overlap"): dir =>
    val workDir = Files.createDirectories(dir.resolve("work"))
    val link = Files.createSymbolicLink(dir.resolve("link"), workDir)
    intercept[ConfigError](
      ClawHome.requireApart(workDir, link.resolve("state"))
    )
    val linkedWork =
      Files.createSymbolicLink(dir.resolve("linked-work"), workDir)
    intercept[ConfigError](
      ClawHome.requireApart(linkedWork, workDir.resolve("state"))
    )

  tmp.test("a new state directory is created owner-only"): dir =>
    val home = dir.resolve("a/b/claw")
    assertEquals(ClawHome.prepare(home, System.getProperty("user.name")), Nil)
    assertEquals(
      PosixFilePermissions.toString(Files.getPosixFilePermissions(home)),
      "rwx------"
    )

  tmp.test(
    "an existing one writable by others, or someone else's, is reported"
  ): dir =>
    val home = Files.createDirectory(dir.resolve("claw"))
    Files.setPosixFilePermissions(
      home,
      PosixFilePermissions.fromString("rwxrwxrwx")
    )
    val warnings = ClawHome.prepare(home, "not-the-owner")
    assert(warnings.exists(_.contains("writable by other users")), warnings)
    assert(warnings.exists(_.contains("owned by")), warnings)
    assertEquals(
      PosixFilePermissions.toString(Files.getPosixFilePermissions(home)),
      "rwxrwxrwx",
      "its permissions are left alone"
    )

  tmp.test("an existing one others can only read is reported too"): dir =>
    val home = Files.createDirectory(dir.resolve("claw"))
    Files.setPosixFilePermissions(
      home,
      PosixFilePermissions.fromString("rwxr-xr-x")
    )
    val warnings = ClawHome.prepare(home, System.getProperty("user.name"))
    assertEquals(warnings.size, 1, warnings)
    assert(warnings.head.contains("readable by other users"), warnings)
    Files.setPosixFilePermissions(
      home,
      PosixFilePermissions.fromString("rwx------")
    )
    assertEquals(ClawHome.prepare(home, System.getProperty("user.name")), Nil)
