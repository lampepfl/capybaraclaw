package capybaraclaw.agent

import java.nio.file.{Files, Path}

import tacit.core.ApiMode

class PluginsSuite extends munit.FunSuite:
  private val workDir = FunFixture[Path](
    setup = _ => Files.createTempDirectory("claw-plugins"),
    teardown = deleteRecursively
  )

  workDir.test("no plugins/ folder loads nothing"): dir =>
    assert(!Plugins.hasDir(dir.toString))
    assertEquals(Plugins.scan(dir.toString), Plugins.Report(Nil, Nil))

  workDir.test("empty plugins/ folder loads nothing"): dir =>
    Files.createDirectory(dir.resolve("plugins"))
    assertEquals(Plugins.load(dir.toString), Nil)

  workDir.test("a valid plugin jar is loaded with its manifest and docs"):
    dir =>
      TestPlugins.write(dir, "demo.jar", apiMode = "replace-core")
      val List(plugin) = Plugins.load(dir.toString): @unchecked
      assertEquals(plugin.manifest.id, "test.demo")
      assertEquals(plugin.manifest.apiMode, ApiMode.ReplaceCore)
      assertEquals(plugin.apiDocs, "demo docs")
      assertEquals(Plugins.describe(plugin), "Demo 1.0 (replace-core)")

  workDir.test("a broken jar is skipped and reported, the others still load"):
    dir =>
      TestPlugins.write(dir, "good.jar", apiMode = "extend-core")
      Files.writeString(dir.resolve("plugins/broken.jar"), "not a jar")
      val report = Plugins.scan(dir.toString)
      assertEquals(report.loaded.map(_.manifest.id), List("test.demo"))
      val List(problem) = report.problems: @unchecked
      assert(problem.contains("broken.jar"), problem)
      assertEquals(
        Plugins.load(dir.toString).map(_.manifest.id),
        List("test.demo")
      )

  workDir.test("a plugin loads after the plugins it requires"): dir =>
    TestPlugins.write(
      dir,
      "a.jar",
      "extend-core",
      id = "a",
      requires = List("b")
    )
    TestPlugins.write(dir, "b.jar", "extend-core", id = "b")
    val report = Plugins.scan(dir.toString)
    assertEquals(report.loaded.map(_.manifest.id), List("b", "a"))
    assertEquals(report.problems, Nil)

  workDir.test("a broken jar also skips what requires it, nothing else"): dir =>
    TestPlugins.write(
      dir,
      "a.jar",
      "extend-core",
      id = "a",
      requires = List("b")
    )
    Files.writeString(dir.resolve("plugins/b.jar"), "not a jar")
    TestPlugins.write(dir, "c.jar", "extend-core", id = "c")
    val report = Plugins.scan(dir.toString)
    assertEquals(report.loaded.map(_.manifest.id), List("c"))
    val List(broken, skipped) = report.problems: @unchecked
    assert(broken.contains("b.jar"), broken)
    assertEquals(skipped, "a (a.jar) skipped: requires b, not loaded")

  workDir.test("skipping a plugin skips its dependents transitively"): dir =>
    TestPlugins.write(
      dir,
      "a.jar",
      "extend-core",
      id = "a",
      requires = List("b")
    )
    TestPlugins.write(
      dir,
      "b.jar",
      "extend-core",
      id = "b",
      requires = List("x")
    )
    TestPlugins.write(dir, "c.jar", "extend-core", id = "c")
    val report = Plugins.scan(dir.toString)
    assertEquals(report.loaded.map(_.manifest.id), List("c"))
    assertEquals(
      report.problems,
      List(
        "b (b.jar) skipped: requires x, not loaded",
        "a (a.jar) skipped: requires b, not loaded"
      )
    )

  workDir.test("a failed check across the set loads no plugins"): dir =>
    TestPlugins.write(dir, "a.jar", apiMode = "extend-core", id = "test.a")
    TestPlugins.write(dir, "b.jar", apiMode = "replace-core", id = "test.b")
    val report = Plugins.scan(dir.toString)
    assertEquals(report.loaded, Nil)
    val List(problem) = report.problems: @unchecked
    assert(problem.contains("Cannot mix"), problem)
    assertEquals(Plugins.load(dir.toString), Nil)

  workDir.test("a plugins file that is not a directory loads nothing"): dir =>
    Files.writeString(dir.resolve("plugins"), "")
    val report = Plugins.scan(dir.toString)
    assertEquals(report.loaded, Nil)
    assert(report.problems.exists(_.contains("not a directory")), report)
    assertEquals(Plugins.load(dir.toString), Nil)

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
