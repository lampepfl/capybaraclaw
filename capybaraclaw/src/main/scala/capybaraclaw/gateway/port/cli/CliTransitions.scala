package capybaraclaw.gateway.port.cli

import capybaraclaw.Throwables
import capybaraclaw.gateway.{
  ApprovalDecision,
  ApprovalReply,
  ApprovalRequest,
  GatewayMessage,
  Inbound,
  Origin
}

/** Pure logic of the CLI port. Runner lives in [[CliPort]]. */
object CliTransitions:
  import CommandMatching.*

  enum CliEvent:
    case UserInput(raw: String)
    case AssistantTextDelta(text: String)
    case AssistantTextComplete(text: String)
    case ErrorText(text: String)
    case ToolCall(toolName: String, args: String)
    case ApprovalRequested(request: ApprovalRequest)
    case ApprovalChosen(request: ApprovalRequest, decision: ApprovalDecision)
    case ApprovalPostponed(request: ApprovalRequest)
    case SpinnerTick(nowMillis: Long)
    case HintTick(buffer: String)
    case InputReadFailed(error: Throwable)
    case TurnFinished
    case InputClosed
    case ShutdownRequested
  export CliEvent.*

  final case class SpinnerState(
      startedAtMillis: Long,
      wordStartIdx: Int,
      frameTick: Int
  )

  final case class State(
      running: Boolean,
      spinner: Option[SpinnerState],
      turnCount: Int,
      turnInFlight: Boolean,
      pendingApprovals: List[ApprovalRequest]
  )

  object State:
    def initial: State =
      State(
        running = true,
        spinner = None,
        turnCount = 0,
        turnInFlight = false,
        pendingApprovals = Nil
      )

  enum Role:
    case User, Assistant, Error, Tool, Permission

  /** What the input reader does next, between turns. */
  enum InputRequest:
    case ReadLine
    case AskApproval(request: ApprovalRequest)

  /** `None` while a turn runs or after quitting. Pending permission requests
    * are asked one by one before the next line is read, unless the terminal
    * cannot show a menu (then `/approve` and `/deny` answer them).
    */
  def nextInput(state: State, menusSupported: Boolean): Option[InputRequest] =
    if !state.running || state.turnInFlight then None
    else
      state.pendingApprovals.headOption
        .filter(_ => menusSupported)
        .map(InputRequest.AskApproval(_))
        .orElse(Some(InputRequest.ReadLine))

  enum CliEffect:
    case Render(role: Role, text: String)
    case RenderAssistantDelta(text: String)
    case RenderAssistantComplete
    case ClearAssistantBuffer
    case RenderSpinner(spinner: SpinnerState, nowMillis: Long)
    case StopSpinner
    case SetEcho(enabled: Boolean)
    case StartSpinnerFiber
    case CancelSpinnerFiber
    case SendOutbound(msg: Inbound)
    case RenderApprovalRequest(request: ApprovalRequest)
    case RenderSessionsList
    case RenderCurrentInfo
    case RenderHintStatus(text: String)

  final case class TransitionContext(
      now: Long,
      newSpinnerWordIdx: Int,
      shouldRenderSpinner: Boolean,
      origin: Origin,
      menusSupported: Boolean = true
  )

  final case class TransitionResult(
      state: State,
      effects: List[CliEffect]
  )

  def transition(
      state: State,
      event: CliEvent,
      ctx: TransitionContext
  ): TransitionResult =
    import CliEffect.*
    event match
      case UserInput(raw) =>
        userInputTransition(state, raw, ctx)

      case AssistantTextDelta(text) =>
        if state.running then
          TransitionResult(state, List(RenderAssistantDelta(text)))
        else TransitionResult(state, Nil)

      case AssistantTextComplete(_) =>
        if state.running then
          TransitionResult(state, List(RenderAssistantComplete))
        else TransitionResult(state, Nil)

      case ErrorText(text) =>
        if state.running then
          TransitionResult(
            state,
            List(RenderAssistantComplete, Render(Role.Error, text))
          )
        else TransitionResult(state, Nil)

      case ToolCall(toolName, args) =>
        // Flushes the text streamed before the call, so it prints above the
        // tool line and the text after it starts a new assistant entry.
        if state.running then
          TransitionResult(
            state,
            List(
              RenderAssistantComplete,
              Render(Role.Tool, formatToolCall(toolName, args))
            )
          )
        else TransitionResult(state, Nil)

      case ApprovalRequested(request) =>
        if !state.running then TransitionResult(state, Nil)
        else
          TransitionResult(
            state.copy(pendingApprovals =
              state.pendingApprovals.filterNot(_.id == request.id) :+ request
            ),
            Option
              .unless(ctx.menusSupported)(RenderApprovalRequest(request))
              .toList
          )

      case ApprovalChosen(request, decision) =>
        startTurn(
          withoutPending(state, request),
          ctx,
          ApprovalReply(ctx.origin, Some(request.id), decision),
          countsAsTurn = false
        )

      case ApprovalPostponed(request) =>
        TransitionResult(
          withoutPending(state, request),
          List(
            Render(
              Role.Permission,
              s"#${request.id} postponed. Answer later with /approve ${request.id} or /deny ${request.id}."
            )
          )
        )

      case TurnFinished =>
        TransitionResult(
          state.copy(spinner = None, turnInFlight = false),
          cancelSpinnerIfActive(state) ++ List(
            SetEcho(true),
            StopSpinner,
            ClearAssistantBuffer
          )
        )

      case SpinnerTick(now) =>
        state.spinner match
          case None    => TransitionResult(state, Nil)
          case Some(s) =>
            val ticked = s.copy(frameTick = s.frameTick + 1)
            val renderEffect =
              if ctx.shouldRenderSpinner then List(RenderSpinner(s, now))
              else Nil
            TransitionResult(state.copy(spinner = Some(ticked)), renderEffect)

      case HintTick(buffer) =>
        if state.turnInFlight then TransitionResult(state, Nil)
        else
          val text = formatHints(topMatches(buffer, HintLimit))
          TransitionResult(state, List(RenderHintStatus(text)))

      case InputReadFailed(error) =>
        if state.running then
          TransitionResult(
            state,
            List(
              Render(
                Role.Error,
                s"Input reader failed: ${Throwables.errorMessage(error)}"
              )
            )
          )
        else TransitionResult(state, Nil)

      case InputClosed | ShutdownRequested =>
        TransitionResult(
          state.copy(spinner = None, running = false),
          cancelSpinnerIfActive(state)
        )

  private def withoutPending(state: State, request: ApprovalRequest): State =
    state.copy(pendingApprovals =
      state.pendingApprovals.filterNot(_.id == request.id)
    )

  private def cancelSpinnerIfActive(state: State): List[CliEffect] =
    state.spinner.map(_ => CliEffect.CancelSpinnerFiber).toList

  private def userInputTransition(
      state: State,
      raw: String,
      ctx: TransitionContext
  ): TransitionResult =
    import CliEffect.*
    val trimmed = raw.trim
    if trimmed.isEmpty then TransitionResult(state, Nil)
    else if CliCommands.isQuit(trimmed) then
      TransitionResult(state.copy(running = false), Nil)
    else if CliCommands.isSessions(trimmed) then
      TransitionResult(state, List(RenderSessionsList))
    else if CliCommands.isCurrent(trimmed) then
      TransitionResult(state, List(RenderCurrentInfo))
    else
      CliCommands.parseApproval(trimmed) match
        case Some(Left(usage)) =>
          TransitionResult(state, List(Render(Role.Error, usage)))
        case Some(Right(command)) =>
          val answered = command.requestId match
            case Some(id) => state.pendingApprovals.filterNot(_.id == id)
            case None     => state.pendingApprovals.dropRight(1)
          startTurn(
            state.copy(pendingApprovals = answered),
            ctx,
            ApprovalReply(ctx.origin, command.requestId, command.decision),
            countsAsTurn = false
          )
        case None if CliCommands.isSlashCommand(trimmed) =>
          TransitionResult(
            state,
            List(Render(Role.Error, unknownCommandText(trimmed)))
          )
        case None =>
          startTurn(
            state,
            ctx,
            GatewayMessage(ctx.origin, raw),
            countsAsTurn = true
          )

  /** Sends `outbound` and waits for the agent's turn, unless one is running.
    * Approval replies do not count towards `turnCount`: the gateway may reject
    * them before any agent turn runs.
    */
  private def startTurn(
      state: State,
      ctx: TransitionContext,
      outbound: Inbound,
      countsAsTurn: Boolean
  ): TransitionResult =
    import CliEffect.*
    if state.turnInFlight then
      TransitionResult(
        state,
        List(Render(Role.Error, "Turn already in progress. Please wait."))
      )
    else
      val spinnerState = SpinnerState(
        startedAtMillis = ctx.now,
        wordStartIdx = ctx.newSpinnerWordIdx,
        frameTick = 0
      )
      TransitionResult(
        state.copy(
          spinner = Some(spinnerState),
          turnCount = state.turnCount + (if countsAsTurn then 1 else 0),
          turnInFlight = true
        ),
        List(
          SendOutbound(outbound),
          RenderSpinner(spinnerState, ctx.now),
          SetEcho(false),
          StartSpinnerFiber
        )
      )

  val SpinnerFrames: Vector[String] = Vector("(ᐢ•(ｪ)•ᐢ)", "(ᐢ-(ｪ)-ᐢ)")
  val SpinnerBlinkEveryTicks: Int = 14
  val ThinkingWordRotateMs: Long = 3000L

  val ThinkingWords: Vector[String] = Vector(
    "Splooting",
    "Wallowing",
    "Soaking",
    "Basking",
    "Munching",
    "Nibbling",
    "Paddling",
    "Floating",
    "Lounging",
    "Ruminating",
    "Dozing",
    "Nuzzling",
    "Grazing",
    "Chomping",
    "Marinading",
    "Splashing",
    "Waddling",
    "Pondering",
    "Dilly-dallying",
    "Chilling"
  )

  def spinnerFrameAt(tick: Int): String =
    if tick % SpinnerBlinkEveryTicks == SpinnerBlinkEveryTicks - 1 then
      SpinnerFrames(1)
    else SpinnerFrames(0)

  def formatDuration(elapsedSec: Long): String =
    if elapsedSec < 60 then s"${elapsedSec}s"
    else if elapsedSec < 3600 then s"${elapsedSec / 60}m ${elapsedSec % 60}s"
    else s"${elapsedSec / 3600}h ${(elapsedSec % 3600) / 60}m"

  def selectThinkingWord(startIdx: Int, elapsedMs: Long): String =
    val idx =
      (startIdx + (elapsedMs / ThinkingWordRotateMs).toInt) % ThinkingWords.size
    ThinkingWords(idx)

  /** Empty input returns `List("")` so callers always emit one entry line. */
  def prepareEntryLines(text: String): List[String] =
    val nonEmpty = text.linesIterator.filter(_.nonEmpty).toList
    if nonEmpty.isEmpty then List("") else nonEmpty

  val ToolArgsMaxLen: Int = 80

  def formatToolCall(toolName: String, args: String): String =
    s"${compactWhitespace(toolName)}(${compactArgs(args)})"

  def compactArgs(args: String): String =
    truncate(compactWhitespace(args), ToolArgsMaxLen)

  /** Control characters go too: a call to an unknown tool, or with args that
    * do not parse, is still reported, with the name and args exactly as the
    * model sent them, so an escape sequence in them would reach the terminal.
    */
  private def compactWhitespace(text: String): String =
    text.replaceAll("[\\s\\p{Cc}]+", " ").trim

  private def truncate(text: String, maxLen: Int): String =
    if text.codePointCount(0, text.length) <= maxLen then text
    else s"${text.substring(0, text.offsetByCodePoints(0, maxLen - 1))}…"
