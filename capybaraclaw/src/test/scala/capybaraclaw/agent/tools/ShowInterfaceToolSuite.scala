package capybaraclaw.agent.tools

import tacit.core.{ApiMode, LoadedPlugin, PluginManifest}

class ShowInterfaceToolSuite extends munit.FunSuite:
  private val core = "def writeClassified(x: Classified[String]): Unit"

  private def plugin(mode: ApiMode, name: String = "Comp Review") =
    LoadedPlugin(
      jarPath = s"/fake/$name.jar",
      manifest = PluginManifest(
        schemaVersion = 1,
        id = s"test.$name",
        name = name,
        version = "0.1.0",
        apiMode = mode,
        domain = Some("Compensation"),
        description = Some("xlsx-backed analytics.")
      ),
      preamble = "",
      apiDocs = s"$name docs"
    )

  test("without plugins shows only the core interface"):
    val ref = ShowInterfaceTool.reference(Nil, core)
    assert(ref.contains(s"```scala\n$core\n```"), ref)
    assert(!ref.contains("# Loaded plugins"), ref)

  test("replace-core plugin shows its metadata and docs and hides core"):
    val ref =
      ShowInterfaceTool.reference(List(plugin(ApiMode.ReplaceCore)), core)
    assert(ref.contains("## Comp Review 0.1.0 — Compensation"), ref)
    assert(ref.contains("Mode: replace-core"), ref)
    assert(ref.contains("xlsx-backed analytics."), ref)
    assert(ref.contains("Comp Review docs"), ref)
    assert(!ref.contains(core), ref)

  test("extend-core plugins show core followed by each plugin's docs"):
    val ref = ShowInterfaceTool.reference(
      List(plugin(ApiMode.ExtendCore, "A"), plugin(ApiMode.ExtendCore, "B")),
      core
    )
    val positions = List(core, "A docs", "B docs").map(ref.indexOf)
    assert(positions.forall(_ >= 0), ref)
    assertEquals(positions, positions.sorted)
