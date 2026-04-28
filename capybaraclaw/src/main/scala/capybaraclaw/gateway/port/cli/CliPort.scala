package capybaraclaw.gateway.port.cli

import capybaraclaw.agent.AgentConfig
import capybaraclaw.gateway.{ContextKey, GatewayMessage, Origin}
import capybaraclaw.gateway.port.Port
import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.AsyncOperations.sleep
import gears.async.default.given
import java.io.File
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.util.List as JList
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.Random
import layoutz.*
import org.jline.reader.{
  EndOfFileException,
  LineReader,
  LineReaderBuilder,
  UserInterruptException
}
import org.jline.terminal.{Attributes, Terminal, TerminalBuilder}
import org.jline.utils.{AttributedStringBuilder, AttributedStyle, Status}

/** Inline CLI port backed by jline. */
class CliPort(
    override val id: String = CliPort.Id,
    user: String = sys.env.getOrElse("USER", "cli"),
    workDirFile: File = java.io.File(".").getCanonicalFile
) extends Port:
  import CliPort.*

  private val outCh = UnboundedChannel[GatewayMessage]()
  private val events = UnboundedChannel[CliEvent]()
  private val inputReadPermits = UnboundedChannel[Unit]()
  private val agentConfig = AgentConfig.load(workDirFile.getPath)

  private val (terminal: Terminal, terminalOwnsStdio: Boolean) =
    buildTerminal()
  private val reader: LineReader = buildReader(terminal)
  private val status: Option[Status] =
    if terminalOwnsStdio then None else Option(Status.getStatus(terminal, true))

  private val threadKey: String = "stdin"
  private val sessionStartMillis = System.currentTimeMillis()
  private val spinnerActive = AtomicBoolean(false)
  private val spinnerTickQueued = AtomicBoolean(false)
  private val inputReadPermitQueued = AtomicBoolean(false)
  private val inputReadInProgress = AtomicBoolean(false)
  private val backgroundLoopsRunning = AtomicBoolean(true)
  private var preTurnAttributes: Option[Attributes] = None

  def incoming: ReadableChannel[GatewayMessage] = outCh.asReadable

  def start()(using Async.Spawn): Future[Unit] =
    Future:
      try
        printHeader()
        offerInputReadPermit()
        val _ = Future(readInputLoop())
        val _ = Future(spinnerTickLoop())
        val finalState = runEventLoop(State.initial)
        printGoodbye(finalState.turnCount)
      finally cleanup()

  def send(key: ContextKey, text: String): Unit =
    offerEvent(AssistantText(text))

  override def sendError(key: ContextKey, text: String): Unit =
    offerEvent(ErrorText(text))

  override def onTurnFinished(key: ContextKey): Unit =
    offerEvent(TurnFinished)

  def shutdown(): Unit =
    if !offerEvent(ShutdownRequested) then requestStop()

  private def readInputLoop()(using Async.Spawn): Unit =
    @tailrec
    def loop(): Unit =
      inputReadPermits.read() match
        case Right(_) =>
          inputReadPermitQueued.set(false)
          inputReadInProgress.set(true)
          val event =
            try
              Option(reader.readLine(userPrompt)) match
                case Some(line) => UserInput(line)
                case None       => InputClosed
            catch
              case _: EndOfFileException     => InputClosed
              case _: UserInterruptException => UserInput("")
              case NonFatal(error)           => InputReadFailed(error)
            finally inputReadInProgress.set(false)
          val shouldContinue = offerEvent(event) && event != InputClosed
          if shouldContinue then
            event match
              case InputReadFailed(_) => sleep(InputReadFailureBackoffMs)
              case _                  => ()
            loop()
        case Left(_) =>
          ()
    loop()

  private def spinnerTickLoop()(using Async.Spawn): Unit =
    @tailrec
    def loop(): Unit =
      sleep(SpinnerIntervalMs)
      val shouldContinue =
        backgroundLoopsRunning.get() &&
          (
            if spinnerActive.get() && spinnerTickQueued.compareAndSet(
                false,
                true
              )
            then
              if offerEvent(SpinnerTick(System.currentTimeMillis())) then true
              else
                spinnerTickQueued.set(false)
                false
            else true
          )
      if shouldContinue then loop()
    loop()

  private def runEventLoop(initial: State)(using
      Async.Spawn
  ): State =
    @tailrec
    def loop(state: State): State =
      events.read() match
        case Right(event) =>
          val next = handleEvent(state, event)
          offerInputReadPermitIfReady(next)
          if next.running then loop(next) else next
        case Left(_) =>
          state.copy(running = false)
    loop(initial)

  private def handleEvent(
      state: State,
      event: CliEvent
  ): State =
    event match
      case UserInput(raw) =>
        handleUserInput(state, raw)

      case AssistantText(text) =>
        if state.running then renderEntry(Role.Assistant, text)
        state

      case ErrorText(text) =>
        if state.running then renderEntry(Role.Error, text)
        state

      case TurnFinished =>
        spinnerActive.set(false)
        spinnerTickQueued.set(false)
        restoreTerminalEcho()
        stopSpinner()
        state.copy(
          spinner = None,
          turnInFlight = false
        )

      case SpinnerTick(now) =>
        spinnerTickQueued.set(false)
        state.spinner match
          case None          => state
          case Some(spinner) =>
            if shouldRenderSpinner then renderSpinner(spinner, now)
            state.copy(spinner =
              Some(spinner.copy(frameTick = spinner.frameTick + 1))
            )

      case InputReadFailed(error) =>
        if state.running then
          renderEntry(
            Role.Error,
            s"Input reader failed: ${errorMessage(error)}"
          )
        state

      case InputClosed | ShutdownRequested =>
        spinnerActive.set(false)
        spinnerTickQueued.set(false)
        requestStop()
        state.copy(running = false)

  private def handleUserInput(
      state: State,
      raw: String
  ): State =
    val trimmed = raw.trim
    if trimmed.isEmpty then state
    else if QuitCommands.contains(trimmed.toLowerCase) then
      requestStop()
      state.copy(running = false)
    else if state.turnInFlight then
      renderEntry(Role.Error, "Turn already in progress. Please wait.")
      state
    else
      val sent =
        try
          outCh.sendImmediately(
            GatewayMessage(Origin(id, threadKey, user), raw)
          )
          true
        catch
          case _: gears.async.ChannelClosedException =>
            requestStop()
            false
      if !sent then state.copy(running = false)
      else
        val now = System.currentTimeMillis()
        val nextSpinner =
          Some(
            SpinnerState(
              startedAtMillis = now,
              wordStartIdx = Random.nextInt(ThinkingWords.size),
              frameTick = 0
            )
          )
        spinnerActive.set(true)
        spinnerTickQueued.set(false)
        nextSpinner.foreach(renderSpinner(_, now))
        suppressTerminalEcho()
        state.copy(
          spinner = nextSpinner,
          turnCount = state.turnCount + 1,
          turnInFlight = true
        )

  private def renderSpinner(spinner: SpinnerState, now: Long): Unit =
    val frame = spinnerFrameAt(spinner.frameTick)
    val elapsedMs = now - spinner.startedAtMillis
    val elapsedSec = elapsedMs / 1000.0
    val wordIdx =
      (spinner.wordStartIdx + (elapsedMs / ThinkingWordRotateMs).toInt) %
        ThinkingWords.size
    val word = ThinkingWords(wordIdx)
    renderStatus(f"$frame $word ($elapsedSec%.1fs)")

  private def shouldRenderSpinner: Boolean =
    try !reader.isReading() || reader.getBuffer.length() == 0
    catch case _: Throwable => true

  private def printGoodbye(turns: Int): Unit =
    val elapsedSec = (System.currentTimeMillis() - sessionStartMillis) / 1000
    val duration =
      if elapsedSec < 60 then s"${elapsedSec}s"
      else if elapsedSec < 3600 then s"${elapsedSec / 60}m ${elapsedSec % 60}s"
      else s"${elapsedSec / 3600}h ${(elapsedSec % 3600) / 60}m"
    val turnsLabel = if turns == 1 then "1 turn" else s"$turns turns"
    val goodbye = rowTight(
      "✦ Goodbye".style(Style.Bold),
      s" • $turnsLabel • $duration".style(Style.Dim)
    ).render
    reader.printAbove("\n" + goodbye + "\n")

  private def printHeader(): Unit =
    val header = box()(
      layout(
        " >_ Capybara".style(Style.Bold),
        "",
        rowTight(" model:     ".style(Style.Dim), agentConfig.model),
        rowTight(" directory: ".style(Style.Dim), workDirFile.getPath)
      )
    ).border(Border.Round)
    reader.printAbove(header.render + "\n")

  private def renderEntry(role: Role, text: String): Unit =
    val (label, style) = role match
      case Role.User =>
        (s"› $user", userStyle)
      case Role.Assistant =>
        (
          "• capybara",
          AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN)
        )
      case Role.Error =>
        ("✗ error", AttributedStyle.DEFAULT.foreground(AttributedStyle.RED))
    val time = LocalTime.now.format(TimeFormatter)
    val timeCol = s"$time "
    val prefix = s"$label > "

    val nonEmpty = text.linesIterator.filter(_.nonEmpty).toList
    val lines = if nonEmpty.isEmpty then List("") else nonEmpty

    val builder = AttributedStringBuilder()
    builder
      .style(AttributedStyle.DEFAULT.faint)
      .append(timeCol)
      .style(style)
      .append(prefix)
      .style(AttributedStyle.DEFAULT)
      .append(lines.head)
      .append("\n")
    lines.tail.foreach(l => builder.append(l).append("\n"))
    reader.printAbove(builder.toAttributedString)

  private def userPrompt: String =
    val time = LocalTime.now.format(TimeFormatter)
    AttributedStringBuilder()
      .style(AttributedStyle.DEFAULT.faint)
      .append(s"$time ")
      .style(userStyle)
      .append(s"› $user > ")
      .style(AttributedStyle.DEFAULT)
      .toAttributedString
      .toAnsi(terminal)

  private def userStyle: AttributedStyle =
    AttributedStyle.DEFAULT.foreground(AttributedStyle.BLUE)

  private def stopSpinner(): Unit =
    renderStatus("")

  private def renderStatus(text: String): Unit =
    status.foreach: st =>
      val line = AttributedStringBuilder()
        .style(AttributedStyle.DEFAULT.faint)
        .append(text)
        .toAttributedString
      try st.update(JList.of(line))
      catch case _: Throwable => ()

  private def offerEvent(event: CliEvent): Boolean =
    try
      events.sendImmediately(event)
      true
    catch case _: gears.async.ChannelClosedException => false

  private def offerInputReadPermitIfReady(state: State): Unit =
    if state.running && !state.turnInFlight then offerInputReadPermit()

  private def offerInputReadPermit(): Unit =
    if !inputReadInProgress.get() &&
      inputReadPermitQueued.compareAndSet(false, true)
    then
      try inputReadPermits.sendImmediately(())
      catch
        case _: gears.async.ChannelClosedException =>
          inputReadPermitQueued.set(false)

  private def suppressTerminalEcho(): Unit =
    try
      if preTurnAttributes.isEmpty then
        val previous = terminal.getAttributes
        val next = Attributes(previous)
        next.setLocalFlag(Attributes.LocalFlag.ECHO, false)
        terminal.setAttributes(next)
        preTurnAttributes = Some(previous)
    catch case _: Throwable => ()

  private def restoreTerminalEcho(): Unit =
    preTurnAttributes.foreach: attributes =>
      try terminal.setAttributes(attributes)
      catch case _: Throwable => ()
    preTurnAttributes = None

  private def requestStop(): Unit =
    backgroundLoopsRunning.set(false)
    spinnerActive.set(false)
    spinnerTickQueued.set(false)
    inputReadPermitQueued.set(false)
    inputReadInProgress.set(false)
    try outCh.close()
    catch case _: Throwable => ()
    try inputReadPermits.close()
    catch case _: Throwable => ()
    restoreTerminalEcho()
    closeTerminalSafely()

  private def cleanup(): Unit =
    backgroundLoopsRunning.set(false)
    spinnerActive.set(false)
    spinnerTickQueued.set(false)
    inputReadPermitQueued.set(false)
    inputReadInProgress.set(false)
    stopSpinner()
    try status.foreach(_.close())
    catch case _: Throwable => ()
    try events.close()
    catch case _: Throwable => ()
    try inputReadPermits.close()
    catch case _: Throwable => ()
    try outCh.close()
    catch case _: Throwable => ()
    restoreTerminalEcho()
    try terminal.writer().flush()
    catch case _: Throwable => ()
    closeTerminalSafely()

  private def closeTerminalSafely(): Unit =
    if terminalOwnsStdio then
      try terminal.writer().flush()
      catch case _: Throwable => ()
    else
      try terminal.close()
      catch case _: Throwable => ()

object CliPort:
  val Id: String = "cli"
  val QuitCommands: Set[String] = Set("quit", "/quit", "exit", "/exit")
  val SpinnerFrames: Vector[String] = Vector("(ᐢ•(ｪ)•ᐢ)", "(ᐢ-(ｪ)-ᐢ)")
  val SpinnerBlinkEvery: Int = 14
  val SpinnerIntervalMs: Long = 100L
  val InputReadFailureBackoffMs: Long = 500L

  private sealed trait CliEvent
  private final case class UserInput(raw: String) extends CliEvent
  private final case class AssistantText(text: String) extends CliEvent
  private final case class ErrorText(text: String) extends CliEvent
  private final case class SpinnerTick(nowMillis: Long) extends CliEvent
  private final case class InputReadFailed(error: Throwable) extends CliEvent
  private case object TurnFinished extends CliEvent
  private case object InputClosed extends CliEvent
  private case object ShutdownRequested extends CliEvent

  /** Visual state shown while a turn is in flight. */
  private final case class SpinnerState(
      startedAtMillis: Long,
      wordStartIdx: Int,
      frameTick: Int
  )

  private final case class State(
      running: Boolean,
      spinner: Option[SpinnerState],
      turnCount: Int,
      turnInFlight: Boolean
  )

  private object State:
    def initial: State =
      State(
        running = true,
        spinner = None,
        turnCount = 0,
        turnInFlight = false
      )

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
  val ThinkingWordRotateMs: Long = 3000L

  val TimeFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("HH:mm")

  def spinnerFrameAt(tick: Int): String =
    if tick % SpinnerBlinkEvery == SpinnerBlinkEvery - 1 then SpinnerFrames(1)
    else SpinnerFrames(0)

  def errorMessage(error: Throwable): String =
    Option(error.getMessage)
      .filter(_.nonEmpty)
      .getOrElse:
        error.getClass.getSimpleName

  enum Role:
    case User, Assistant, Error

  private def buildTerminal(): (Terminal, Boolean) =
    val systemAttempts = Vector[() => Terminal](
      () =>
        TerminalBuilder
          .builder()
          .system(true)
          .provider("jni")
          .dumb(false)
          .build(),
      () =>
        TerminalBuilder
          .builder()
          .system(true)
          .provider("exec")
          .dumb(false)
          .build(),
      () => TerminalBuilder.builder().system(true).dumb(false).build()
    )
    systemAttempts.iterator
      .flatMap: mk =>
        try Some(mk())
        catch case _: Throwable => None
      .nextOption()
      .map(t => (t, false))
      .getOrElse:
        val dumb = TerminalBuilder
          .builder()
          .system(false)
          .streams(System.in, System.out)
          .dumb(true)
          .build()
        (dumb, true)

  private def buildReader(terminal: Terminal): LineReader =
    val reader = LineReaderBuilder
      .builder()
      .appName("capybara")
      .terminal(terminal)
      .build()
    reader.option(LineReader.Option.BRACKETED_PASTE, true)
    reader
