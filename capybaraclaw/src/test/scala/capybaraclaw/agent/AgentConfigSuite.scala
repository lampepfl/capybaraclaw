package capybaraclaw.agent

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import tacit.agents.llm.endpoint.{EffortLevel, ThinkingMode}

class AgentConfigSuite extends munit.FunSuite:
  private val workDir = FunFixture[Path](
    setup = _ => Files.createTempDirectory("claw-config"),
    teardown = deleteRecursively
  )

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.deleteIfExists(_))
      finally stream.close()

  private def writeConfig(dir: Path, json: String): Unit =
    Files.writeString(dir.resolve("claw.json"), json, StandardCharsets.UTF_8)

  workDir.test("missing claw.json yields defaults"): dir =>
    val c = AgentConfig.load(dir.toString)
    assertEquals(c.provider, Provider.OpenRouter)
    assertEquals(c.model, "minimax/minimax-m2.7")
    assertEquals(c.maxTokens, 16000)
    assertEquals(c.classifiedPaths, Nil)
    assertEquals(c.executionTimeoutMs, AgentConfig.DefaultExecutionTimeoutMs)
    assertEquals(
      c.toLLMConfig("").thinking,
      Some(ThinkingMode.Effort(EffortLevel.Medium))
    )

  workDir.test("valid claw.json is parsed"): dir =>
    writeConfig(
      dir,
      """{"provider":"anthropic","model":"claude","max_tokens":42,"classified_paths":["a/","b"],"execution_timeout_ms":5000}"""
    )
    val c = AgentConfig.load(dir.toString)
    assertEquals(c.provider, Provider.Anthropic)
    assertEquals(c.model, "claude")
    assertEquals(c.maxTokens, 42)
    assertEquals(c.classifiedPaths, List("a/", "b"))
    assertEquals(c.executionTimeoutMs, 5000)
    assertEquals(c.toLLMConfig("").thinking, Some(ThinkingMode.Budget(2048)))

  workDir.test("ollama runs without thinking"): dir =>
    writeConfig(dir, """{"provider":"ollama"}""")
    assertEquals(AgentConfig.load(dir.toString).toLLMConfig("").thinking, None)

  workDir.test("present-but-partial config keeps defaults for missing fields"):
    dir =>
      writeConfig(dir, """{"model":"only-model"}""")
      val c = AgentConfig.load(dir.toString)
      assertEquals(c.model, "only-model")
      assertEquals(c.provider, Provider.OpenRouter)
      assertEquals(c.maxTokens, 16000)

  workDir.test("malformed JSON raises a ConfigError naming claw.json"): dir =>
    writeConfig(dir, "{ not json")
    val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
    assert(ex.getMessage.contains("claw.json"), ex.getMessage)

  workDir.test("a non-object top level raises a ConfigError"): dir =>
    writeConfig(dir, "[1, 2, 3]")
    val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
    assert(ex.getMessage.contains("claw.json"), ex.getMessage)

  workDir.test("wrong-typed max_tokens names the field and expected type"):
    dir =>
      writeConfig(dir, """{"max_tokens":"lots"}""")
      val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
      assert(ex.getMessage.contains("max_tokens"), ex.getMessage)
      assert(ex.getMessage.contains("positive integer"), ex.getMessage)

  workDir.test("wrong-typed classified_paths names the field"): dir =>
    writeConfig(dir, """{"classified_paths":"not-an-array"}""")
    val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
    assert(ex.getMessage.contains("classified_paths"), ex.getMessage)

  workDir.test("wrong-typed provider names the field"): dir =>
    writeConfig(dir, """{"provider":42}""")
    val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
    assert(ex.getMessage.contains("'provider' must be a string"), ex.getMessage)

  workDir.test("unknown provider is rejected at load time"): dir =>
    writeConfig(dir, """{"provider":"antropic"}""")
    val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
    assert(ex.getMessage.contains("'provider' must be one of"), ex.getMessage)

  workDir.test("null model names the field"): dir =>
    writeConfig(dir, """{"model":null}""")
    val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
    assert(
      ex.getMessage.contains("'model' must be a non-empty string"),
      ex.getMessage
    )

  List("1.5", "0", "-3", "1e20").foreach: n =>
    workDir.test(s"max_tokens $n is rejected as not a positive integer"): dir =>
      writeConfig(dir, s"""{"max_tokens":$n}""")
      val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
      assert(ex.getMessage.contains("positive integer"), ex.getMessage)

  List("1.5", "0", "-3", "\"soon\"").foreach: n =>
    workDir.test(s"execution_timeout_ms $n is rejected"): dir =>
      writeConfig(dir, s"""{"execution_timeout_ms":$n}""")
      val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
      assert(ex.getMessage.contains("execution_timeout_ms"), ex.getMessage)
      assert(ex.getMessage.contains("positive integer"), ex.getMessage)

  workDir.test("errors name the full path of claw.json"): dir =>
    writeConfig(dir, """{"model":1}""")
    val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
    assert(
      ex.getMessage.contains(dir.resolve("claw.json").toString),
      ex.getMessage
    )

  workDir.test("unreadable claw.json (a directory) raises a ConfigError"):
    dir =>
      Files.createDirectory(dir.resolve("claw.json"))
      val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
      assert(ex.getMessage.contains("cannot read"), ex.getMessage)

  workDir.test("invalid UTF-8 raises a ConfigError"): dir =>
    Files.write(dir.resolve("claw.json"), Array[Byte](0x7b, 0xff.toByte, 0x7d))
    val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
    assert(ex.getMessage.contains("cannot read"), ex.getMessage)

  List("\"\"", "\"   \"").foreach: m =>
    workDir.test(s"blank model $m is rejected"): dir =>
      writeConfig(dir, s"""{"model":$m}""")
      val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
      assert(
        ex.getMessage.contains("'model' must be a non-empty string"),
        ex.getMessage
      )

  workDir.test("blank classified_paths entry is rejected"): dir =>
    writeConfig(dir, """{"classified_paths":["keys", ""]}""")
    val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
    assert(ex.getMessage.contains("classified_paths"), ex.getMessage)

  workDir.test("unknown key close to a known one suggests it"): dir =>
    writeConfig(dir, """{"classifed_paths":["keys"]}""")
    val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
    assert(
      ex.getMessage.contains(
        "unknown key 'classifed_paths', did you mean 'classified_paths'?"
      ),
      ex.getMessage
    )

  workDir.test("unknown key far from any known one lists the known keys"):
    dir =>
      writeConfig(dir, """{"thinking":"high"}""")
      val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
      assert(ex.getMessage.contains("unknown key 'thinking'"), ex.getMessage)
      assert(
        ex.getMessage.contains(
          "known keys: provider, model, max_tokens, classified_paths"
        ),
        ex.getMessage
      )
