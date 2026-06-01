package capybaraclaw.agent

import tacit.core.{ApiMode, LoadedPlugin, PluginManifest}

import java.nio.file.{Files, Path}

class ClawAgentSuite extends munit.FunSuite:

  test(
    "show_interface with replace-core plugin exposes plugin metadata and docs, hides core"
  ):
    val plugin = LoadedPlugin(
      jarPath = "/fake/compreview.jar",
      manifest = PluginManifest(
        schemaVersion = 1,
        id = "safemode.compreview",
        name = "Safemode Compensation Review",
        version = "0.1.0",
        apiMode = ApiMode.ReplaceCore,
        domain = Some("Compensation Review"),
        description = Some("xlsx-backed compensation review analytics.")
      ),
      preamble = "import safemode.compreview.*",
      apiDocs = "Use `writePrivateAnswer(Classified[String])`."
    )
    val config = AgentConfig(workDir = "/tmp")
    val reference = ClawAgent.composedInterfaceReference(config, List(plugin))

    assert(reference.contains("Safemode Compensation Review 0.1.0"))
    assert(reference.contains("Compensation Review"))
    assert(reference.contains("Mode: replace-core"))
    assert(reference.contains("writePrivateAnswer"))
    assert(!reference.contains("def writeClassified"))

  test("show_interface without plugins shows core Interface.scala only"):
    val config = AgentConfig(workDir = "/tmp")
    val reference = ClawAgent.composedInterfaceReference(config, Nil)
    // Either the real Interface.scala block or the explicit fallback message —
    // both confirm the no-plugin branch was taken.
    assert(
      reference.contains("```scala") ||
        reference.contains("Interface.scala not found")
    )
    assert(!reference.contains("# Loaded plugins"))

  test("loads private LLM config from separate workdir file"):
    val dir = Files.createTempDirectory("claw-private-llm")
    try
      Files.writeString(
        dir.resolve("claw.json"),
        """{
          |  "private_llm_config": "claw.private.json"
          |}
          |""".stripMargin
      )
      Files.writeString(
        dir.resolve("claw.private.json"),
        """{
          |  "provider": "ollama",
          |  "base_url": "http://127.0.0.1:11434",
          |  "model": "llama3.2:latest"
          |}
          |""".stripMargin
      )

      val config = AgentConfig.load(dir.toString)

      assertEquals(
        config.privateLlm,
        Some(
          PrivateLlmConfig(
            provider = "ollama",
            baseUrl = "http://127.0.0.1:11434",
            model = "llama3.2:latest"
          )
        )
      )
    finally deleteRecursively(dir)

  test("pluginsConfigured is true when workdir/plugins/ contains a *.jar"):
    val dir = Files.createTempDirectory("claw-plugins-true")
    try
      val pluginsDir = dir.resolve("plugins")
      Files.createDirectory(pluginsDir)
      Files.writeString(pluginsDir.resolve("dummy.jar"), "")
      val config = AgentConfig.load(dir.toString)
      assert(config.pluginsConfigured)
    finally deleteRecursively(dir)

  test("pluginsConfigured is false when workdir has no plugins folder"):
    val dir = Files.createTempDirectory("claw-plugins-none")
    try
      val config = AgentConfig.load(dir.toString)
      assert(!config.pluginsConfigured)
    finally deleteRecursively(dir)

  test("pluginsConfigured is false when plugins folder exists but is empty"):
    val dir = Files.createTempDirectory("claw-plugins-empty")
    try
      Files.createDirectory(dir.resolve("plugins"))
      val config = AgentConfig.load(dir.toString)
      assert(!config.pluginsConfigured)
    finally deleteRecursively(dir)

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      if Files.isDirectory(path) then
        val children = Files.list(path)
        try children.forEach(p => deleteRecursively(p))
        finally children.close()
      Files.deleteIfExists(path)
