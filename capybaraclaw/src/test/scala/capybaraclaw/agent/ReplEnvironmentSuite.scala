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

  workDir.test("file access is bounded to the working directory"): dir =>
    val outside = Files.createTempDirectory("claw-outside").toRealPath()
    try
      val env = ReplEnvironment(dir.toString, Nil)
      val inside = env.repl.execute(existsCode(dir.toRealPath()))
      assert(inside.output.contains("true"), inside.output)
      val denied = env.repl.execute(existsCode(outside))
      assert(
        denied.output.contains("not within any allowed root"),
        denied.output
      )
    finally deleteRecursively(outside)

  workDir.test("requests outside the working directory ask the oracle"): dir =>
    val outside = Files.createTempDirectory("claw-outside").toRealPath()
    try
      val asked = java.util.concurrent.ConcurrentLinkedQueue[String]()
      val oracle: String => String = request =>
        asked.add(request)
        ujson.write(
          ujson.Obj("allow" -> false, "message" -> "waiting for approval #1")
        )
      val env = ReplEnvironment(dir.toString, Nil, Some(oracle))
      val result = env.repl.execute(existsCode(outside))
      assert(result.output.contains("waiting for approval #1"), result.output)
      assertEquals(asked.size, 1)
      assertEquals(
        ujson.read(asked.peek()).obj("resolved").str,
        outside.toString
      )
    finally deleteRecursively(outside)

  workDir.test("after the user approves, the same request succeeds"): dir =>
    import capybaraclaw.gateway.*
    import capybaraclaw.gateway.port.slack.SlackPort
    val outside = Files.createTempDirectory("claw-outside").toRealPath()
    try
      val sessionId = SessionId.random()
      val broker = TestIdentities.broker()
      val origin =
        Origin(SlackPort.Id, UserId("U_alice"), SessionRef.Direct(sessionId))
      broker.beginTurn(
        sessionId,
        FakePort(SlackPort.Id, supportsApprovals = true),
        origin
      )
      val env =
        ReplEnvironment(dir.toString, Nil, Some(broker.oracle(sessionId)))
      val denied = env.repl.execute(existsCode(outside))
      assert(denied.output.contains("permission request #1"), denied.output)
      assert(
        broker
          .resolve(sessionId, Some(1), ApprovalDecision.Approve, origin)
          .isRight
      )
      val granted = env.repl.execute(existsCode(outside))
      assert(granted.output.contains("true"), granted.output)
    finally deleteRecursively(outside)

  workDir.test("commands are blocked by default and run once approved"): dir =>
    import capybaraclaw.gateway.*
    import capybaraclaw.gateway.port.slack.SlackPort
    val sessionId = SessionId.random()
    val broker = TestIdentities.broker()
    val origin =
      Origin(SlackPort.Id, UserId("U_alice"), SessionRef.Direct(sessionId))
    broker.beginTurn(
      sessionId,
      FakePort(SlackPort.Id, supportsApprovals = true),
      origin
    )
    val env = ReplEnvironment(dir.toString, Nil, Some(broker.oracle(sessionId)))
    val code =
      """requestExecPermission(Set("echo")) { execOutput("echo", List("ran")) }"""
    val denied = env.repl.execute(code)
    assert(denied.output.contains("permission request #1"), denied.output)
    assert(
      broker
        .resolve(sessionId, Some(1), ApprovalDecision.Approve, origin)
        .isRight
    )
    val ran = env.repl.execute(code)
    assert(ran.output.contains("ran"), ran.output)

  workDir.test("relative paths resolve against the working directory"): dir =>
    // The test JVM runs elsewhere, so this fails if tacit resolves against
    // the JVM's working directory.
    val env = ReplEnvironment(dir.toString, Nil)
    val result = env.repl.execute(
      """requestFileSystem(".") { access("notes.txt").write("x") }"""
    )
    assertEquals(
      Files.readString(dir.resolve("notes.txt")),
      "x",
      result.output + result.error.getOrElse("")
    )

  workDir.test("a runaway loop times out and does not block other sessions"):
    dir =>
      val stuck = ReplEnvironment(dir.toString, Nil, executionTimeoutMs = 500)
      val other = ReplEnvironment(dir.toString, Nil, executionTimeoutMs = 500)
      // Ignores interrupts; bounded so the thread ends after the suite.
      val loop =
        """def spin(n: Long): Long =
          |  var acc = 0L
          |  var i = 0L
          |  while i < n do
          |    acc = acc * 31 + i
          |    i += 1
          |  acc
          |spin(20000000000L)""".stripMargin
      val timedOut = stuck.repl.execute(loop)
      assert(!timedOut.success)
      assert(
        timedOut.error.exists(_.contains("timed out")),
        timedOut.error.getOrElse(timedOut.output)
      )
      val started = System.nanoTime
      val result = other.repl.execute("1 + 1")
      assert(result.output.contains("2"), result.output)
      assert((System.nanoTime - started) / 1000000 < 5000)

  private def existsCode(path: Path): String =
    s"""requestFileSystem("$path") { access("$path").exists }"""

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
