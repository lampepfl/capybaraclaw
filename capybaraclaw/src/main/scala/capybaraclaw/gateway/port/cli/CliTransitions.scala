package capybaraclaw.gateway.port.cli

import capybaraclaw.gateway.{GatewayMessage, Origin}

/** Pure logic of the CLI port. Runner lives in [[CliPort]]. */
object CliTransitions:

  sealed trait CliEvent
  final case class UserInput(raw: String) extends CliEvent
  final case class AssistantText(text: String) extends CliEvent
  final case class ErrorText(text: String) extends CliEvent
  final case class SpinnerTick(nowMillis: Long) extends CliEvent
  final case class InputReadFailed(error: Throwable) extends CliEvent
  case object TurnFinished extends CliEvent
  case object InputClosed extends CliEvent
  case object ShutdownRequested extends CliEvent

  final case class SpinnerState(
      startedAtMillis: Long,
      wordStartIdx: Int,
      frameTick: Int
  )

  final case class State(
      running: Boolean,
      spinner: Option[SpinnerState],
      turnCount: Int,
      turnInFlight: Boolean
  )

  object State:
    def initial: State =
      State(
        running = true,
        spinner = None,
        turnCount = 0,
        turnInFlight = false
      )

  enum Role:
    case User, Assistant, Error

  sealed trait CliEffect
  object CliEffect:
    final case class Render(role: Role, text: String) extends CliEffect
    final case class RenderSpinner(spinner: SpinnerState, nowMillis: Long)
        extends CliEffect
    case object StopSpinner extends CliEffect
    final case class SetEcho(enabled: Boolean) extends CliEffect
    case object StartSpinnerFiber extends CliEffect
    case object CancelSpinnerFiber extends CliEffect
    final case class SendOutbound(msg: GatewayMessage) extends CliEffect

  final case class TransitionContext(
      now: Long,
      newSpinnerWordIdx: Int,
      shouldRenderSpinner: Boolean,
      origin: Origin
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

      case AssistantText(text) =>
        if state.running then
          TransitionResult(state, List(Render(Role.Assistant, text)))
        else TransitionResult(state, Nil)

      case ErrorText(text) =>
        if state.running then
          TransitionResult(state, List(Render(Role.Error, text)))
        else TransitionResult(state, Nil)

      case TurnFinished =>
        TransitionResult(
          state.copy(spinner = None, turnInFlight = false),
          cancelSpinnerIfActive(state) ++ List(SetEcho(true), StopSpinner)
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

      case InputReadFailed(error) =>
        if state.running then
          TransitionResult(
            state,
            List(
              Render(Role.Error, s"Input reader failed: ${errorMessage(error)}")
            )
          )
        else TransitionResult(state, Nil)

      case InputClosed | ShutdownRequested =>
        TransitionResult(
          state.copy(spinner = None, running = false),
          cancelSpinnerIfActive(state)
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
    else if state.turnInFlight then
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
          turnCount = state.turnCount + 1,
          turnInFlight = true
        ),
        List(
          SendOutbound(GatewayMessage(ctx.origin, raw)),
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

  def errorMessage(error: Throwable): String =
    Option(error.getMessage)
      .filter(_.nonEmpty)
      .getOrElse:
        error.getClass.getSimpleName

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
