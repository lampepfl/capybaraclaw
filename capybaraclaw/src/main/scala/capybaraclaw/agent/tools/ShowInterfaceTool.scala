package capybaraclaw.agent.tools

import tacit.agents.llm.agentic.Agent
import tacit.agents.llm.utils.IsToolArg
import tacit.core.{ApiMode, LoadedPlugin}

import capybaraclaw.agent.Plugins

object ShowInterfaceTool:
  case class Args() derives IsToolArg

  val name: String = "show_interface"
  val description: String =
    "Returns the exact API available in the evaluate_scala REPL for this " +
      "workdir, including any loaded plugins. Call it before your first " +
      "evaluate_scala in a task instead of guessing method names."

  def register(
      agent: Agent,
      plugins: List[LoadedPlugin],
      coreInterface: String
  ): Unit =
    val text = reference(plugins, coreInterface)
    agent.handle[Args](name, description): (_, _) =>
      text

  def reference(plugins: List[LoadedPlugin], coreInterface: String): String =
    val includeCore = plugins.forall(_.manifest.apiMode == ApiMode.ExtendCore)
    val core = Option.when(includeCore)(s"```scala\n$coreInterface\n```")
    val apis =
      core.toList ++ plugins.map(p => s"## ${p.manifest.name}\n\n${p.apiDocs}")
    val loaded = Option.when(plugins.nonEmpty):
      ("# Loaded plugins" :: plugins.map(pluginSummary)).mkString("\n\n")
    (List(preface) ++ loaded ++ List(
      "# Available API\n\n" + apis.mkString("\n\n---\n\n")
    )).mkString("\n\n")

  private def pluginSummary(plugin: LoadedPlugin): String =
    val m = plugin.manifest
    val header = s"## ${m.name} ${m.version}${m.domain.fold("")(d => s" — $d")}"
    (header :: s"Mode: ${Plugins.renderApiMode(m.apiMode)}" :: m.description.toList)
      .mkString("\n")

  private val preface: String =
    """|Only use the API below to interact with the system. Direct use of JDK or
       |Scala standard library side effects (java.io, java.nio, scala.io,
       |sys.process, java.net, ...) is rejected by the REPL's code validator.
       |Everything listed here is already imported in every evaluate_scala call.""".stripMargin
