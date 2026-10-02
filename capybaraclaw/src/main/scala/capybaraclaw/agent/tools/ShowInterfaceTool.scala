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
      "workdir, including any loaded plugins (the same reference as in the " +
      "system prompt). Use it instead of guessing method names."

  def register(agent: Agent, apiReference: String): Unit =
    val text = s"$apiReference\n\n$closing"
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

  // Small models tend to discuss the API after reading it instead of going
  // back to the user's task; this steers them to act.
  private val closing: String =
    """|---
       |
       |Now return to the user's request. Your next step is an evaluate_scala
       |call that works on it with the API above. Do not summarize or comment on
       |the API, and do not write code in chat: it does not run.""".stripMargin
