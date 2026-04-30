package capybaraclaw.gateway.port.cli

import capybaraclaw.agent.AgentConfig
import capybaraclaw.gateway.{ContextKey, GatewayMessage, Origin}
import capybaraclaw.gateway.port.Port

import gears.async.AsyncOperations.sleep
import gears.async.{
  Async,
  ChannelClosedException,
  Future,
  ReadableChannel,
  UnboundedChannel
}
import gears.async.default.given

import java.io.File
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.util.List as JList

import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.{Random, Success, Try}

import layoutz.*

import org.jline.reader.{
  EndOfFileException,
  LineReader,
  LineReaderBuilder,
  UserInterruptException
}
import org.jline.terminal.{Attributes, Terminal, TerminalBuilder}
import org.jline.utils.{AttributedStringBuilder, AttributedStyle, Status}

/** Runner for [[CliTransitions]] backed by jline. */
class CliPort(
    override val id: String = CliPort.Id,
    user: String = sys.env.getOrElse("USER", "cli"),
    workDirFile: File = java.io.File(".").getCanonicalFile
) extends Port:
  import CliPort.*
  import CliTransitions.*

  private val outCh = UnboundedChannel[GatewayMessage]()
  private val events = UnboundedChannel[CliEvent]()
  private val inputReadPermits = UnboundedChannel[Unit]()
  private val shutdownPromise: Future.Promise[Unit] = Future.Promise[Unit]()
  private val agentConfig = AgentConfig.load(workDirFile.getPath)

  private val (terminal: Terminal, terminalOwnsStdio: Boolean) =
    buildTerminal()
  private val reader: LineReader = buildReader(terminal)
  private val status: Option[Status] =
    if terminalOwnsStdio then None else Option(Status.getStatus(terminal, true))

  private val threadKey: String = "stdin"
  private val sessionStartMillis = System.currentTimeMillis()

  def incoming: ReadableChannel[GatewayMessage] = outCh.asReadable

  def start()(using Async.Spawn): Future[Unit] =
    Future:
      try
        printHeader()
        offerInputReadPermit()
        val _ = Future(readInputLoop())
        val finalState =
          Async.group:
            runEventLoop(RuntimeState.initial).state
        printGoodbye(finalState.turnCount)
      finally cleanup()

  def send(key: ContextKey, text: String): Unit =
    offerEvent(AssistantText(text))

  override def sendError(key: ContextKey, text: String): Unit =
    offerEvent(ErrorText(text))

  override def onTurnFinished(key: ContextKey): Unit =
    offerEvent(TurnFinished)

  def shutdown(): Unit =
    Try(shutdownPromise.complete(Success(())))

  private def readInputLoop()(using Async): Unit =
    @tailrec
    def loop(): Unit =
      inputReadPermits.read() match
        case Right(_) =>
          val event =
            try
              Option(reader.readLine(userPrompt)) match
                case Some(line) => UserInput(line)
                case None       => InputClosed
            catch
              case _: EndOfFileException     => InputClosed
              case _: UserInterruptException => UserInput("")
              case NonFatal(error)           => InputReadFailed(error)
          val shouldContinue = offerEvent(event) && event != InputClosed
          if shouldContinue then
            event match
              case InputReadFailed(_) => sleep(InputReadFailureBackoffMs)
              case _                  => ()
            loop()
        case Left(_) =>
          ()
    loop()

  private def runSpinner()(using Async): Unit =
    while true do
      sleep(SpinnerIntervalMs)
      offerEvent(SpinnerTick(System.currentTimeMillis()))

  private def runEventLoop(initial: RuntimeState)(using
      Async,
      Async.Spawn
  ): RuntimeState =
    @tailrec
    def loop(rs: RuntimeState): RuntimeState =
      val event: Option[CliEvent] = Async.select(
        events.readSource.handle:
          case Right(event) => Some(event): Option[CliEvent]
          case Left(_)      => None
        ,
        shutdownPromise.handle: (_: Try[Unit]) =>
          Some(ShutdownRequested): Option[CliEvent]
      )
      val next = event match
        case None     => rs.copy(state = rs.state.copy(running = false))
        case Some(ev) =>
          val ctx = TransitionContext(
            now = System.currentTimeMillis(),
            newSpinnerWordIdx = Random.nextInt(ThinkingWords.size),
            shouldRenderSpinner = shouldRenderSpinnerNow,
            origin = Origin(id, threadKey, user)
          )
          val result = transition(rs.state, ev, ctx)
          applyEffects(rs.copy(state = result.state), result.effects)
      offerInputReadPermitIfReady(next.state)
      if next.state.running then loop(next) else next
    loop(initial)

  private def applyEffects(
      rs: RuntimeState,
      effects: List[CliEffect]
  )(using Async.Spawn): RuntimeState =
    import CliEffect.*
    effects.foldLeft(rs): (rs, effect) =>
      effect match
        case Render(role, text) =>
          renderEntry(role, text)
          rs
        case RenderSpinner(s, now) =>
          renderSpinner(s, now)
          rs
        case StopSpinner =>
          stopSpinner()
          rs
        case SetEcho(enabled) =>
          setEcho(enabled)
          rs
        case StartSpinnerFiber =>
          rs.copy(spinnerFiber = Some(Future(runSpinner())))
        case CancelSpinnerFiber =>
          rs.spinnerFiber.foreach(_.cancel())
          rs.copy(spinnerFiber = None)
        case SendOutbound(msg) =>
          try
            outCh.sendImmediately(msg)
            rs
          catch
            case _: ChannelClosedException =>
              rs.copy(state = rs.state.copy(running = false))

  private def renderSpinner(spinner: SpinnerState, now: Long): Unit =
    val frame = spinnerFrameAt(spinner.frameTick)
    val elapsedMs = now - spinner.startedAtMillis
    val elapsedSec = elapsedMs / 1000.0
    val word = selectThinkingWord(spinner.wordStartIdx, elapsedMs)
    renderStatus(f"$frame $word ($elapsedSec%.1fs)")

  private def shouldRenderSpinnerNow: Boolean =
    try !reader.isReading() || reader.getBuffer.length() == 0
    catch case NonFatal(_) => true

  private def printGoodbye(turns: Int): Unit =
    val elapsedSec = (System.currentTimeMillis() - sessionStartMillis) / 1000
    val duration = formatDuration(elapsedSec)
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

    val lines = prepareEntryLines(text)
    val builder = AttributedStringBuilder()
    appendTimeColumn(builder)
      .style(style)
      .append(s"$label > ")
      .style(AttributedStyle.DEFAULT)
      .append(lines.head)
      .append("\n")
    lines.tail.foreach(l => builder.append(l).append("\n"))
    reader.printAbove(builder.toAttributedString)

  private def userPrompt: String =
    val builder = AttributedStringBuilder()
    appendTimeColumn(builder)
      .style(userStyle)
      .append(s"› $user > ")
      .style(AttributedStyle.DEFAULT)
      .toAttributedString
      .toAnsi(terminal)

  private def appendTimeColumn(
      builder: AttributedStringBuilder
  ): AttributedStringBuilder =
    builder
      .style(AttributedStyle.DEFAULT.faint)
      .append(s"${LocalTime.now.format(TimeFormatter)} ")

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
      catch case NonFatal(_) => ()

  private def offerEvent(event: CliEvent): Boolean =
    try
      events.sendImmediately(event)
      true
    catch case _: ChannelClosedException => false

  private def offerInputReadPermitIfReady(state: State): Unit =
    if state.running && !state.turnInFlight then offerInputReadPermit()

  private def offerInputReadPermit(): Unit =
    try inputReadPermits.sendImmediately(())
    catch case _: ChannelClosedException => ()

  private def setEcho(enabled: Boolean): Unit =
    Try:
      val current = terminal.getAttributes
      val updated = Attributes(current)
      updated.setLocalFlag(Attributes.LocalFlag.ECHO, enabled)
      terminal.setAttributes(updated)

  private def cleanup(): Unit =
    stopSpinner()
    Try(status.foreach(_.close()))
    Try(events.close())
    Try(inputReadPermits.close())
    Try(outCh.close())
    setEcho(true)
    releaseTerminal()

  private def releaseTerminal(): Unit =
    if terminalOwnsStdio then Try(terminal.writer().flush())
    else Try(terminal.close())

object CliPort:
  val Id: String = "cli"
  val SpinnerIntervalMs: Long = 100L
  val InputReadFailureBackoffMs: Long = 500L

  val TimeFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("HH:mm")

  private final case class RuntimeState(
      state: CliTransitions.State,
      spinnerFiber: Option[Future[Unit]]
  )

  private object RuntimeState:
    val initial: RuntimeState =
      RuntimeState(CliTransitions.State.initial, spinnerFiber = None)

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
