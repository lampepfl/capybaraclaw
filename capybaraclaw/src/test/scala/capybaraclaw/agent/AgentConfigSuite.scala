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
    assertEquals(c.provider, "openrouter")
    assertEquals(c.model, "minimax/minimax-m2.7")
    assertEquals(c.maxTokens, 16000)
    assertEquals(c.classifiedPaths, Nil)
    assertEquals(c.thinking, Some(ThinkingMode.Effort(EffortLevel.Medium)))

  workDir.test("valid claw.json is parsed"): dir =>
    writeConfig(
      dir,
      """{"provider":"anthropic","model":"claude","max_tokens":42,"classified_paths":["a/","b"]}"""
    )
    val c = AgentConfig.load(dir.toString)
    assertEquals(c.provider, "anthropic")
    assertEquals(c.model, "claude")
    assertEquals(c.maxTokens, 42)
    assertEquals(c.classifiedPaths, List("a/", "b"))

  workDir.test("present-but-partial config keeps defaults for missing fields"):
    dir =>
      writeConfig(dir, """{"model":"only-model"}""")
      val c = AgentConfig.load(dir.toString)
      assertEquals(c.model, "only-model")
      assertEquals(c.provider, "openrouter")
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
      assert(ex.getMessage.contains("number"), ex.getMessage)

  workDir.test("wrong-typed classified_paths names the field"): dir =>
    writeConfig(dir, """{"classified_paths":"not-an-array"}""")
    val ex = intercept[ConfigError](AgentConfig.load(dir.toString))
    assert(ex.getMessage.contains("classified_paths"), ex.getMessage)
