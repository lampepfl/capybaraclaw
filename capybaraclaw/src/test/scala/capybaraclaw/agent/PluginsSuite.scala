package capybaraclaw.agent

import java.nio.file.{Files, Path}

import tacit.core.ApiMode

class PluginsSuite extends munit.FunSuite:
  private val workDir = FunFixture[Path](
    setup = _ => Files.createTempDirectory("claw-plugins"),
    teardown = deleteRecursively
  )

  workDir.test("no plugins/ folder loads nothing"): dir =>
    assertEquals(Plugins.scanDirs(dir.toString), Nil)
    assertEquals(Plugins.load(dir.toString), Nil)

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

  workDir.test("a broken plugin jar raises a ConfigError naming plugins/"):
    dir =>
      Files.createDirectory(dir.resolve("plugins"))
      Files.writeString(dir.resolve("plugins/broken.jar"), "not a jar")
      val ex = intercept[ConfigError](Plugins.load(dir.toString))
      assert(ex.getMessage.startsWith("plugins: "), ex.getMessage)
      assert(ex.getMessage.contains("broken.jar"), ex.getMessage)

  workDir.test("a plugins file that is not a directory raises a ConfigError"):
    dir =>
      Files.writeString(dir.resolve("plugins"), "")
      val ex = intercept[ConfigError](Plugins.load(dir.toString))
      assert(ex.getMessage.contains("not a directory"), ex.getMessage)

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
