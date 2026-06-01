package capybaraclaw.agent

import tacit.core.{ApiMode, LoadedPlugin}
import tacit.agents.llm.endpoint.*
import tacit.agents.llm.agentic.{Agent, AgentRun, AgentState, AgentError}
import tacit.agents.llm.utils.IsToolArg
import gears.async.Async
import tacit.agents.utils.Result

import capybaraclaw.agent.tools.{EvalScalaTool, MemoryTool, SessionSearchTool}
import capybaraclaw.gateway.{SessionId, SessionSearch}

case class ShowInterfaceArgs() derives IsToolArg

/** Agent class for Claw. */
class ClawAgent(
    val workDir: String,
    sessionId: SessionId,
    sessionSearch: SessionSearch,
    initialMessages: List[Message] = Nil,
    endpointOverride: Option[Endpoint] = None,
    memoryStore: MemoryStore = MemoryStore.default()
):
  val agentConfig: AgentConfig =
    AgentConfig.load(workDir, memoryStore.snapshot())

  private val replEnv: ReplEnvironment =
    ReplEnvironment(
      workDir,
      agentConfig.classifiedPaths,
      agentConfig.privateLlm
    )

  private given Endpoint = endpointOverride.getOrElse(agentConfig.provider match
    case "anthropic"  => AnthropicEndpoint.createFromEnv()
    case "openai"     => OpenAIEndpoint.createFromEnv()
    case "openrouter" => OpenRouterEndpoint.createFromEnv()
    case "ollama"     => OllamaEndpoint.createFromEnv()
    case other        => throw RuntimeException(s"Unknown provider: $other"))

  private val agent: Agent =
    val a = new Agent:
      type State = AgentState
      def getInitState = new AgentState:
        val llmConfig = agentConfig.toLLMConfig

    EvalScalaTool.register(a, replEnv.repl)
    MemoryTool.register(a, memoryStore)
    SessionSearchTool.register(a, sessionSearch, sessionId)

    a.handle[ShowInterfaceArgs](
      "show_interface",
      "Returns the exact capability-scoped API available in this REPL. Call " +
        "this BEFORE your first evaluate_scala — guessing method names wastes " +
        "turns. The REPL preamble pre-loads available imports, so refer to the " +
        "documented symbols directly."
    ): (_, _) =>
      ClawAgent.composedInterfaceReference(agentConfig, replEnv.loadedPlugins)

    // Seed with any persisted prior transcript so rehydrated conversations continue
    // where they left off.
    a.state.messages = initialMessages

    a

  def ask(
      message: String,
      onToolCall: Option[(String, String, String) => Unit] = None
  ): Result[ChatResponse, AgentError] =
    agent.ask(message, onToolCall)

  def streamAsk(message: String)(using Async.Spawn): AgentRun =
    agent.streamAsk(message)

object ClawAgent:
  /** Compose the `show_interface` output from the API surface actually loaded
    * into the REPL. When any plugin uses `replace-core`, the core tacit
    * `Interface.scala` is intentionally hidden from the model — its symbols
    * are not imported and surfacing them would invite hallucinated calls.
    */
  def composedInterfaceReference(
      agentConfig: AgentConfig,
      plugins: List[LoadedPlugin]
  ): String =
    val sb = StringBuilder()
    sb.append(
      """|IMPORTANT: You must only use the provided interface below to interact
         |with the system. Do not use Java/Scala stdlib APIs (java.io, java.nio,
         |scala.io, sys.process, java.net, etc.) directly — they are blocked by
         |the REPL's code validator. All side effects must go through the
         |capability-scoped API so they are properly sandboxed.
         |
         |The interface is pre-loaded and available in all evaluate_scala calls.
         |
         |""".stripMargin
    )

    if plugins.nonEmpty then
      val includeCore = plugins.forall(_.manifest.apiMode == ApiMode.ExtendCore)
      sb.append("# Loaded plugins\n\n")
      plugins.foreach: p =>
        val m = p.manifest
        val domainPart = m.domain.fold("")(d => s" — $d")
        sb.append(s"## ${m.name} ${m.version}$domainPart\n")
        sb.append(s"Mode: ${renderApiMode(m.apiMode)}\n")
        m.description.foreach(d => sb.append(s"$d\n"))
        sb.append("\n")
      sb.append("# Available API\n\n")
      if includeCore then
        sb.append(coreInterfaceReference).append("\n\n---\n\n")
      sb.append(
        plugins
          .map(p => s"## ${p.manifest.name}\n\n${p.apiDocs}")
          .mkString("\n\n---\n\n")
      )
    else sb.append(coreInterfaceReference)

    // Closing nudge. Small models tend to "reflect on the architecture" after
    // a long doc dump instead of returning to the user's task. The directive
    // below pushes them to immediately call evaluate_scala.
    sb.append(
      """|
         |
         |---
         |
         |**Now return to the user's question.** Your next response MUST be a
         |`evaluate_scala` tool call containing the Scala code that answers it.
         |If that code successfully writes the requested confidential answer,
         |stop calling tools; repeated writes append duplicate output.
         |Do NOT write commentary on the API design, do NOT summarize the
         |interface, do NOT emit free-form code in chat — none of those run.
         |""".stripMargin
    )
    sb.toString

  def renderApiMode(mode: ApiMode): String = mode match
    case ApiMode.ExtendCore  => "extend-core"
    case ApiMode.ReplaceCore => "replace-core"

  private def coreInterfaceReference: String =
    val stream = classOf[ClawAgent].getClassLoader
      .getResourceAsStream("Interface.scala")
    if stream == null then
      "(Interface.scala not found on classpath — this is a build issue)"
    else
      try
        val content = scala.io.Source.fromInputStream(stream).mkString
        "```scala\n" + content + "\n```"
      finally stream.close()
