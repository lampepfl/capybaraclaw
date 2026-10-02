package capybaraclaw.agent

import tacit.agents.llm.endpoint.*
import tacit.agents.llm.agentic.{Agent, AgentRun, AgentState, AgentError}
import gears.async.Async
import tacit.agents.utils.Result

import capybaraclaw.agent.tools.{
  EvalScalaTool,
  MemoryTool,
  SessionSearchTool,
  ShowInterfaceTool
}
import capybaraclaw.gateway.{SessionId, SessionSearch}

/** Agent class for Claw. */
class ClawAgent(
    val workDir: String,
    sessionId: SessionId,
    sessionSearch: SessionSearch,
    initialMessages: List[Message] = Nil,
    endpointOverride: Option[Endpoint] = None,
    memory: MemoryAccess,
    permissionOracle: Option[String => String] = None
):
  val agentConfig: AgentConfig =
    AgentConfig.load(workDir, memory.snapshot())

  private val replEnv: ReplEnvironment =
    ReplEnvironment(workDir, agentConfig.classifiedPaths, permissionOracle)

  private given Endpoint =
    endpointOverride.getOrElse(agentConfig.provider.createEndpoint())

  private val apiReference: String =
    ShowInterfaceTool.reference(replEnv.loadedPlugins, replEnv.coreInterface)

  private val agent: Agent =
    val a = new Agent:
      type State = AgentState
      def getInitState = new AgentState:
        val llmConfig = agentConfig.toLLMConfig(apiReference)

    EvalScalaTool.register(a, replEnv.repl)
    MemoryTool.register(a, memory)
    SessionSearchTool.register(a, sessionSearch, sessionId)
    ShowInterfaceTool.register(a, apiReference)

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
