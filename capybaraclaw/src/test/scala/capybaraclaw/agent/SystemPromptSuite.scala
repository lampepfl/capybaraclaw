package capybaraclaw.agent

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class SystemPromptSuite extends munit.FunSuite:
  private val workDir = FunFixture[Path](
    setup = _ => Files.createTempDirectory("claw-system-prompt"),
    teardown = SystemPromptSuite.deleteRecursively
  )

  test("renderResource substitutes placeholders and strips the trailing newline"):
    val rendered = SystemPrompt.renderResource(
      "prompts/render-basic.md",
      Map("a" -> "1", "b" -> "2")
    )
    assertEquals(rendered, "A: 1\nB: 2")

  test("renderResource inserts values literally without re-expanding them"):
    val rendered = SystemPrompt.renderResource(
      "prompts/render-escaping.md",
      Map("v" -> "$HOME and {{a}} and \\ stay literal")
    )
    assertEquals(rendered, "Value: $HOME and {{a}} and \\ stay literal")

  test("renderResource reports the missing placeholder and template"):
    val ex = intercept[IllegalStateException]:
      SystemPrompt.renderResource("prompts/bad-template.md", Map.empty)
    assertEquals(
      ex.getMessage,
      "No value for placeholder {{missing}} in template: prompts/bad-template.md"
    )

  workDir.test("build renders every section when config and CLAW.md are present"):
    dir =>
      Files.writeString(
        dir.resolve("CLAW.md"),
        "Be concise.",
        StandardCharsets.UTF_8
      )
      val config = AgentConfig(
        workDir = dir.toString,
        provider = "openrouter",
        model = "test/model",
        classifiedPaths = List("secret/")
      )

      val expected =
        SystemPromptSuite.systemSection(config) +
          "\n\n" +
          """<classified_paths>
            |The following paths should be classified:
            |- secret/
            |</classified_paths>""".stripMargin +
          "\n\n" +
          """<project_instructions>
            |Be concise.
            |</project_instructions>""".stripMargin

      assertEquals(SystemPrompt.build(config), expected)

  workDir.test("build emits only the system section without config or CLAW.md"):
    dir =>
      val config = AgentConfig(workDir = dir.toString)
      assertEquals(SystemPrompt.build(config), SystemPromptSuite.systemSection(config))

object SystemPromptSuite:
  private def systemSection(config: AgentConfig): String =
    SystemPrompt.renderResource(
      "prompts/system.md",
      Map(
        "work_dir" -> config.workDir,
        "config" -> config.toString,
        "interface_source" -> interfaceSource
      )
    )

  private def interfaceSource: String =
    try
      val source = scala.io.Source.fromResource("Interface.scala")
      try source.mkString
      finally source.close()
    catch
      case _: java.io.FileNotFoundException =>
        "(Interface.scala not found on classpath)"

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
