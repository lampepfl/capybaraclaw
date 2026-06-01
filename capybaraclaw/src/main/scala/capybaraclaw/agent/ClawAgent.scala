package capybaraclaw.agent

import tacit.agents.llm.endpoint.*
import tacit.agents.llm.agentic.{Agent, AgentRun, AgentState, AgentError}
import gears.async.Async
import tacit.agents.utils.Result

import capybaraclaw.agent.tools.EvalScalaTool

/** Agent class for Claw. */
class ClawAgent(
    val workDir: String,
    initialMessages: List[Message] = Nil,
    endpointOverride: Option[Endpoint] = None
):
  val agentConfig: AgentConfig = AgentConfig.load(workDir)

  private val replEnv: ReplEnvironment =
    ReplEnvironment(workDir, agentConfig.classifiedPaths)

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
