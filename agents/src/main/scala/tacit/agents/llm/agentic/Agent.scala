package tacit.agents
package llm
package agentic

import endpoint.{
  Endpoint,
  LLMConfig,
  LLMError,
  Message,
  Content,
  ChatResponse,
  FinishReason,
  ToolSchema,
  StreamEvent
}
import tacit.agents.utils.Result
import llm.utils.{IsToolArg, ToolArgParsingError}
import scala.util.boundary
import scala.util.control.NonFatal
import java.util.concurrent.CancellationException
import scala.annotation.tailrec
import java.util.concurrent.atomic.AtomicBoolean
import gears.async.{
  Async,
  Future,
  SyncChannel,
  ReadableChannel,
  UnboundedChannel,
  Channel,
  ChannelClosedException
}

class AgentError(val description: String):
  override def toString: String = s"AgentError: $description"

enum AgentStreamEvent:
  case Stream(event: StreamEvent)
  case ToolResult(id: String, toolName: String, result: String)
  case MaxTokensExceeded
  case Steered(texts: List[String])
  case Unconsumed(texts: List[String])

enum SteerOutcome:
  case Accepted
  case RejectedRunEnded

class AgentRun private[agentic] (
    val events: ReadableChannel[Result[AgentStreamEvent, AgentError]],
    private val steering: UnboundedChannel[String],
    private val completed: AtomicBoolean
):
  def steer(text: String): SteerOutcome =
    if completed.get then SteerOutcome.RejectedRunEnded
    else
      try
        steering.sendImmediately(text)
        SteerOutcome.Accepted
      catch
        case _: ChannelClosedException =>
          SteerOutcome.RejectedRunEnded

  def isActive: Boolean = !completed.get

trait AgentState:
  val llmConfig: LLMConfig
  var messages: List[Message] = Nil

trait AgentTool[-StateType <: AgentState]:
  type ArgType: IsToolArg

  def name: String
  def description: String

  def handle(arg: ArgType, state: StateType): String

  def toolSchema: ToolSchema =
    ToolSchema(
      name,
      description,
      parameters = summon[IsToolArg[ArgType]].schema
    )

  def parseArgs(input: String): Result[ArgType, ToolArgParsingError] =
    summon[IsToolArg[ArgType]].parse(input)

abstract class Agent:
  type State <: AgentState

  val state: State = getInitState

  private var _tools: List[AgentTool[State]] = Nil
  def tools: List[AgentTool[State]] = _tools

  def getInitState: State

  def addTool(tool: AgentTool[State]): this.type =
    if _tools.exists(_.name == tool.name) then
      throw IllegalArgumentException(
        s"Tool with name '${tool.name}' already exists"
      )
    _tools = _tools :+ tool
    this

  def addTools(newTools: AgentTool[State]*): this.type =
    newTools.foreach(addTool)
    this

  def handle[A: IsToolArg](toolName: String, desc: String)(
      handler: (A, this.State) => String
  ): this.type =
    val tool = new AgentTool[State]:
      type ArgType = A
      def name = toolName
      def description = desc
      def handle(arg: A, state: State): String = handler(arg, state)
    addTool(tool)

  def ask(
      message: String,
      onToolCall: Option[(String, String, String) => Unit] = None
  )(using endpoint: Endpoint): Result[ChatResponse, AgentError] =
    state.messages = state.messages :+ Message.user(message)
    val config = state.llmConfig.copy(tools = tools.map(_.toolSchema))
    Result:
      loop(config, onToolCall)

  @tailrec
  private def loop(
      config: LLMConfig,
      onToolCall: Option[(String, String, String) => Unit]
  )(using
      endpoint: Endpoint,
      label: boundary.Label[Result[ChatResponse, AgentError]]
  ): ChatResponse =
    val response = endpoint.invoke(state.messages, config) match
      case Right(r) => r
      case Left(e)  => boundary.break(Left(AgentError(e.description)))

    state.messages = state.messages :+ response.message

    response.finishReason match
      case FinishReason.ToolUse =>
        val toolUses = response.message.content.collect:
          case tu: Content.ToolUse => tu

        val toolResults = toolUses.map: tu =>
          val result = dispatchTool(tu)
          val resultContent = result.content.collectFirst:
            case Content.ToolResult(_, content, _) => content
          onToolCall.foreach(_(tu.name, tu.input, resultContent.getOrElse("")))
          result

        state.messages = state.messages :++ toolResults
        loop(config, onToolCall)

      case FinishReason.MaxTokens =>
        redactMaxTokensMessage(response)
        response

      case _ => response

  def streamAsk(
      message: String
  )(using endpoint: Endpoint, spawn: Async.Spawn): AgentRun =
    state.messages = state.messages :+ Message.user(message)
    val config = state.llmConfig.copy(tools = tools.map(_.toolSchema))
    val ch = SyncChannel[Result[AgentStreamEvent, AgentError]]()
    val steering = UnboundedChannel[String]()
    val completed = new AtomicBoolean(false)
    Future:
      try streamLoop(config, ch, steering)(using endpoint)
      finally
        // Reject any further steer() before capturing leftovers, so a late
        // send cannot land silently between the final drain and the close.
        completed.set(true)
        val leftover = drainSteering(steering)
        try
          // Best effort: the reader may have stopped reading, or the run is
          // being cancelled.
          if leftover.nonEmpty then
            try ch.send(Right(AgentStreamEvent.Unconsumed(leftover)))
            catch case NonFatal(_) => ()
        finally
          // Closing the events is how the reader learns the run is over, so
          // it happens whatever the send did.
          try steering.close()
          finally ch.close()
    AgentRun(ch.asReadable, steering, completed)

  private def drainSteering(queue: UnboundedChannel[String]): List[String] =
    val buf = scala.collection.mutable.ListBuffer[String]()
    var more = true
    while more do
      queue.readSource.poll() match
        case Some(Right(t)) => buf += t
        case _              => more = false
    buf.toList

  private def streamLoop(
      config: LLMConfig,
      ch: SyncChannel[Result[AgentStreamEvent, AgentError]],
      steering: UnboundedChannel[String]
  )(using endpoint: Endpoint, spawn: Async.Spawn): Unit =
    try
      val streamCh = endpoint.stream(state.messages, config)
      val response = consumeStream(streamCh, ch)

      state.messages = state.messages :+ response.message

      response.finishReason match
        case FinishReason.ToolUse =>
          val toolUses = response.message.content.collect:
            case tu: Content.ToolUse => tu

          for tu <- toolUses do
            val msg = dispatchTool(tu, () => spawn.group.isCancelled)
            state.messages = state.messages :+ msg
            val resultContent = msg.content.collectFirst:
              case Content.ToolResult(_, content, _) => content
            ch.send(
              Right(
                AgentStreamEvent.ToolResult(
                  tu.id,
                  tu.name,
                  resultContent.getOrElse("")
                )
              )
            )

          val steered = drainSteering(steering)
          if steered.nonEmpty then
            state.messages = state.messages ++ steered.map(Message.user)
            ch.send(Right(AgentStreamEvent.Steered(steered)))

          streamLoop(config, ch, steering)

        case FinishReason.MaxTokens =>
          redactMaxTokensMessage(response)
          ch.send(Right(AgentStreamEvent.MaxTokensExceeded))

        case _ =>
          ()
    catch
      // NonFatal, not Exception: an Error from the endpoint (an assertion, a
      // `???`) must still reach the reader as a failure, not a quiet close.
      case NonFatal(e) =>
        try ch.send(Left(AgentError(s"Stream error: ${e.getMessage}")))
        catch case NonFatal(_) => ()

  private def consumeStream(
      streamCh: ReadableChannel[Result[StreamEvent, LLMError]],
      outCh: SyncChannel[Result[AgentStreamEvent, AgentError]]
  )(using Async): ChatResponse =
    var finalResponse: ChatResponse | Null = null
    var reading = true
    while reading do
      streamCh.read() match
        case Right(Right(event)) =>
          outCh.send(Right(AgentStreamEvent.Stream(event)))
          event match
            case StreamEvent.Done(response) =>
              finalResponse = response
              reading = false
            case _ =>
        case Right(Left(llmError)) =>
          outCh.send(Left(AgentError(llmError.description)))
          throw RuntimeException(s"LLM error: ${llmError.description}")
        case Left(_) => // channel closed
          reading = false
    if finalResponse == null then
      outCh.send(Left(AgentError("Stream ended without Done event")))
      throw RuntimeException("Stream ended without Done event")
    finalResponse

  /** Redact a MaxTokens response: remove incomplete tool calls, update message history. */
  private def redactMaxTokensMessage(response: ChatResponse): Unit =
    val cleaned = response.message.content.filter:
      case _: Content.ToolUse => false
      case _                  => true
    // Replace the last message in history with the cleaned version
    state.messages =
      state.messages.init :+ response.message.copy(content = cleaned)

  /** Every tool call gets a result, an error one when the call is invalid or
    * the tool throws: providers reject a history with an unanswered call, and
    * the model can correct itself instead of the run ending.
    *
    * A `CancellationException` only ends the run when `runCancelled` says the
    * run itself is being cancelled; one from the tool's own work (a future it
    * waited on was cancelled) is a tool failure like any other.
    */
  private def dispatchTool(
      toolUse: Content.ToolUse,
      runCancelled: () => Boolean = () => false
  ): Message =
    def error(text: String) = Message.toolResult(toolUse.id, text, true)
    tools.find(_.name == toolUse.name) match
      case None =>
        error(s"Unknown tool: ${toolUse.name}")

      case Some(tool) =>
        tool.parseArgs(toolUse.input) match
          case Left(err) =>
            error(s"Failed to parse args for ${toolUse.name}: ${err.message}")

          case Right(args) =>
            try
              Message.toolResult(
                toolUse.id,
                tool.handle(args.asInstanceOf[tool.ArgType], state)
              )
            catch
              case e: CancellationException if runCancelled() => throw e
              case NonFatal(e)                                =>
                error(s"Tool ${toolUse.name} failed: $e")
