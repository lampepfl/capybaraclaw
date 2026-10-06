package capybaraclaw.agent

import java.nio.file.{Files, Path}

class ReplEnvironmentSuite extends munit.FunSuite:
  private val workDir = FunFixture[Path](
    setup = _ => Files.createTempDirectory("claw-repl-env"),
    teardown = deleteRecursively
  )

  workDir.test("coreInterface is the real tacit-library API source"): dir =>
    val env = ReplEnvironment(dir.toString, Nil)
    assert(env.coreInterface.contains("def requestFileSystem"))

  workDir.test("a loaded plugin's preamble runs in the REPL"): dir =>
    TestPlugins.write(
      dir,
      "marker.jar",
      apiMode = "extend-core",
      preamble = "@scala.caps.assumeSafe val pluginMarker = 42"
    )
    val env = ReplEnvironment(dir.toString, Nil)
    val result = env.repl.execute("pluginMarker")
    assert(result.output.contains("42"), result.output)

  workDir.test("a broken plugin jar does not stop the session"): dir =>
    TestPlugins.write(
      dir,
      "marker.jar",
      apiMode = "extend-core",
      preamble = "@scala.caps.assumeSafe val pluginMarker = 42"
    )
    Files.writeString(dir.resolve("plugins/broken.jar"), "not a jar")
    val env = ReplEnvironment(dir.toString, Nil)
    assertEquals(env.loadedPlugins.map(_.manifest.id), List("test.demo"))
    val result = env.repl.execute("pluginMarker")
    assert(result.output.contains("42"), result.output)

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
