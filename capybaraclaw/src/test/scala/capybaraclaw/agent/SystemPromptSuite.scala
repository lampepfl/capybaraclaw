package capybaraclaw.agent

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class SystemPromptSuite extends munit.FunSuite:
  private val workDir = FunFixture[Path](
    setup = _ => Files.createTempDirectory("claw-system-prompt"),
    teardown = SystemPromptSuite.deleteRecursively
  )

  test(
    "renderResource substitutes placeholders and strips the trailing newline"
  ):
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

  workDir.test(
    "build renders every section when config, CLAW.md and memory are present"
  ): dir =>
    Files.writeString(
      dir.resolve("CLAW.md"),
      "Be concise.",
      StandardCharsets.UTF_8
    )
    val config = AgentConfig(
      workDir = dir.toString,
      provider = Provider.OpenRouter,
      model = "test/model",
      classifiedPaths = List("secret/"),
      memorySnapshot = SystemPromptSuite.snapshot
    )

    val expected =
      SystemPromptSuite.systemSection(config) +
        "\n\n" +
        """<classified_paths>
          |The following paths are classified. The REPL API only exposes their contents as `Classified[T]`, which cannot be unwrapped or printed. Write results derived from them only to classified paths, with the classified write function if show_interface lists one; if it lists none, do not write them at all.
          |- secret/
          |</classified_paths>""".stripMargin +
        "\n\n" +
        """<project_instructions>
          |Be concise.
          |</project_instructions>""".stripMargin +
        "\n\n"

    val built = SystemPrompt.build(config, "the API")
    assert(built.startsWith(expected), built)
    val boundary = SystemPromptSuite.boundaryOf(built)
    assertEquals(
      built.stripPrefix(expected),
      SystemPrompt.renderMemory(config.memorySnapshot, boundary)
    )

  workDir.test("build emits only the system section without memory"): dir =>
    val config = AgentConfig(workDir = dir.toString)
    assertEquals(
      SystemPrompt.build(config, "the API"),
      SystemPromptSuite.systemSection(config)
    )

  test("renderMemory fences every section and marks read-only ones"):
    val rendered = SystemPrompt.renderMemory(SystemPromptSuite.snapshot, "b0")
    assert(rendered.contains("This is a test conversation."), rendered)
    assert(
      rendered.contains(
        """<memory target="private" visible="test notes" access="read-write" usage="0%" chars="13/2200" boundary="b0">
          |memory marker
          |</memory boundary="b0">""".stripMargin
      ),
      rendered
    )
    assert(
      rendered.contains(
        """<memory target="public" visible="everyone" access="read-only" usage="0%" chars="0/2200" boundary="b0">
          |(empty)
          |</memory boundary="b0">""".stripMargin
      ),
      rendered
    )

  workDir.test("every prompt gets a fresh memory boundary"): dir =>
    val config = AgentConfig(
      workDir = dir.toString,
      memorySnapshot = SystemPromptSuite.snapshot
    )
    val first =
      SystemPromptSuite.boundaryOf(SystemPrompt.build(config, "the API"))
    val second =
      SystemPromptSuite.boundaryOf(SystemPrompt.build(config, "the API"))
    assertNotEquals(first, second)
    assert(first.matches("[0-9a-f]{16}"), first)

object SystemPromptSuite:
  private def systemSection(config: AgentConfig): String =
    SystemPrompt.renderResource(
      "prompts/system.md",
      Map("work_dir" -> config.workDir, "api_reference" -> "the API")
    )

  private val snapshot: MemorySnapshot = MemorySnapshot(
    "a test conversation",
    List(
      MemorySection(MemoryFile.User, "test profile", true, "user marker"),
      MemorySection(MemoryFile.Private, "test notes", true, "memory marker"),
      MemorySection(MemoryFile.Public, "everyone", false, "")
    )
  )

  private def boundaryOf(prompt: String): String =
    raw"""boundary="([0-9a-f]+)"""".r
      .findFirstMatchIn(prompt)
      .map(_.group(1).nn)
      .getOrElse(throw AssertionError(s"no boundary in: $prompt"))

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
