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

  test("the agent cannot change plugins/, claw.json or CLAW.md"):
    // Under the test JVM's working directory, which tacit allows as a root
    // until a later change bounds file access to the workdir itself.
    val target = Files.createDirectories(Path.of("target"))
    val dir = Files.createTempDirectory(target, "claw-read-only").toRealPath()
    try
      Files.writeString(dir.resolve("CLAW.md"), "be nice")
      val env = ReplEnvironment(dir.toString, Nil)
      def run(code: String): String =
        val r = env.repl.execute(s"""requestFileSystem("$dir") { $code }""")
        r.output + r.error.getOrElse("")
      assert(run(s"""access("$dir/CLAW.md").read()""").contains("be nice"))
      for path <- List("plugins/x.jar", "PLUGINS/x.jar", "claw.json", "CLAW.md")
      do
        val plain = run(s"""access("$dir/$path").write("x")""")
        assert(plain.contains("read-only"), s"$path: $plain")
        val classified =
          run(s"""writeClassified("$dir/$path", classify("x"))""")
        assert(classified.contains("read-only"), s"$path: $classified")
      run(s"""access("$dir/notes.txt").write("x")""")
      assertEquals(Files.readString(dir.resolve("CLAW.md")), "be nice")
      assert(Files.exists(dir.resolve("notes.txt")))
      assert(!Files.exists(dir.resolve("plugins")))
      assert(!Files.exists(dir.resolve("claw.json")))
    finally deleteRecursively(dir)

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
