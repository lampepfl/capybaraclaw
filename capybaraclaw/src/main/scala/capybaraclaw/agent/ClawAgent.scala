package capybaraclaw.agent

import tacit.core.{Context as TacitContext, Config as TacitConfig}
import tacit.executor.ReplSession
import tacit.agents.llm.endpoint.*
import tacit.agents.llm.agentic.{Agent, AgentRun, AgentState, AgentError}
import gears.async.Async
import tacit.agents.llm.utils.IsToolArg
import tacit.agents.utils.Result
import io.circe.Json
import io.circe.syntax.*

case class EvalScalaArgs(code: String) derives IsToolArg

/** Agent class for Claw. */
class ClawAgent(
  val workDir: String,
  initialMessages: List[Message] = Nil,
  endpointOverride: Option[Endpoint] = None,
):
  val agentConfig: AgentConfig = AgentConfig.load(workDir)

  private val tacitContext: TacitContext = TacitContext(
    TacitConfig(
      libraryConfig = Json.obj(
        "classifiedPaths" -> agentConfig.classifiedPaths.map(p => java.io.File(workDir, p).getCanonicalPath).asJson
      ),
    ),
    recorder = None,
  )
  private val repl: ReplSession = ReplSession.create(using tacitContext)

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

    a.handle[EvalScalaArgs]("evaluate_scala", "Evaluate a Scala expression in a persistent REPL session"): (args, _) =>
      val result = repl.execute(args.code)
      if result.success then
        if result.output.nonEmpty then result.output
        else "(executed successfully, no output)"
      else
        val msg = StringBuilder("Execution failed.\n")
        if result.output.nonEmpty then msg.append(s"Output:\n${result.output}\n")
        result.error.foreach(e => msg.append(s"Error:\n$e\n"))
        msg.toString

    // Seed with any persisted prior transcript so rehydrated conversations continue
    // where they left off.
    a.state.messages = initialMessages

    a

  def ask(
    message: String,
    onToolCall: Option[(String, String, String) => Unit] = None,
  ): Result[ChatResponse, AgentError] =
    agent.ask(message, onToolCall)

  def streamAsk(message: String)(using Async.Spawn): AgentRun =
    agent.streamAsk(message)

  def printStartupInfo(): Unit =
    val clawJsonExists = java.io.File(workDir, "claw.json").exists()
    val clawMdExists = java.io.File(workDir, "CLAW.md").exists()
    println("Capybara Claw")
    println(s"  workdir  : $workDir")
    println(s"  provider : ${agentConfig.provider}")
    println(s"  model    : ${agentConfig.model}")
    println(s"  thinking : ${agentConfig.thinking.getOrElse("off")}")
    println(s"  claw.json: ${if clawJsonExists then "found" else "defaults"}")
    println(s"  CLAW.md  : ${if clawMdExists then "found" else "not found"}")
    if agentConfig.classifiedPaths.nonEmpty then
      println(s"  classify : ${agentConfig.classifiedPaths.mkString(", ")}")
    println()
