package capybaraclaw.gateway.port.cli

import capybaraclaw.agent.AgentConfig
import capybaraclaw.gateway.{ContextKey, GatewayMessage, Origin}
import capybaraclaw.gateway.port.Port
import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import java.io.File
import java.time.format.DateTimeFormatter
import java.util.List as JList
import java.util.concurrent.{
  Executors,
  ScheduledExecutorService,
  ScheduledFuture,
  TimeUnit
}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import layoutz.*
import org.jline.reader.{
  EndOfFileException,
  LineReader,
  LineReaderBuilder,
  UserInterruptException
}
import org.jline.terminal.{Terminal, TerminalBuilder}
import org.jline.utils.{
  AttributedString,
  AttributedStringBuilder,
  AttributedStyle,
  Status
}

/** Inline CLI port backed by jline. */
class CliPort(
    override val id: String = CliPort.Id,
    user: String = sys.env.getOrElse("USER", "cli"),
    workDirFile: File = java.io.File(".").getCanonicalFile
) extends Port:

  private val outCh = UnboundedChannel[GatewayMessage]()
  private val agentConfig = AgentConfig.load(workDirFile.getPath)

  private val (terminal: Terminal, terminalOwnsStdio: Boolean) =
    CliPort.buildTerminal()
  private val reader: LineReader = CliPort.buildReader(terminal)
  private val status: Option[Status] =
    if terminalOwnsStdio then None else Option(Status.getStatus(terminal, true))

  private val threadKey: String = "stdin"
  @volatile private var running: Boolean = true
  private val turnLock = Object()
  @volatile private var turnInProgress: Boolean = false
  @volatile private var turnWaitTimedOut: Boolean = false
  private val sessionStartMillis = System.currentTimeMillis()
  private val turnCount = AtomicInteger(0)

  private lazy val spinnerScheduler: ScheduledExecutorService =
    Executors.newSingleThreadScheduledExecutor: r =>
      val th = Thread(r, "capybara-spinner")
      th.setDaemon(true)
      th
  private val spinnerTask = AtomicReference[Option[ScheduledFuture[?]]](None)
  private val spinnerFrame = AtomicInteger(0)

  def incoming: ReadableChannel[GatewayMessage] = outCh.asReadable

  def start()(using Async.Spawn): Future[Unit] =
    Future:
      try
        printHeader()
        readLoop()
        printGoodbye()
      finally cleanup()

  def send(key: ContextKey, text: String): Unit =
    if running then renderEntry(CliPort.Role.Assistant, text)

  override def sendError(key: ContextKey, text: String): Unit =
    if running then renderEntry(CliPort.Role.Error, text)

  override def onTurnFinished(key: ContextKey): Unit =
    stopSpinner()
    val hadTimeout = turnWaitTimedOut
    turnLock.synchronized:
      turnInProgress = false
      turnWaitTimedOut = false
      turnLock.notifyAll()
    if hadTimeout && running then
      renderEntry(
        CliPort.Role.Assistant,
        "Previous turn finished. You can send messages again."
      )

  def shutdown(): Unit =
    running = false
    turnLock.synchronized:
      turnInProgress = false
      turnLock.notifyAll()
    try outCh.close()
    catch case _: Throwable => ()
    closeTerminalSafely()

  private def readLoop(): Unit =
    while running do
      val maybeLine: Option[String] =
        try Option(reader.readLine(CliPort.UserPrompt))
        catch
          case _: EndOfFileException     => None
          case _: UserInterruptException => Some("")
      maybeLine match
        case None      => running = false
        case Some("")  => ()
        case Some(raw) => handleSubmit(raw)

  private def handleSubmit(raw: String): Unit =
    val trimmed = raw.trim
    if trimmed.isEmpty then ()
    else if CliPort.QuitCommands.contains(trimmed.toLowerCase) then
      running = false
    else if turnInProgress then
      val msg =
        if turnWaitTimedOut then
          s"Turn still running after ${CliPort.TurnMaxWaitMs / 1000}s. Wait for completion or type quit."
        else "Turn already in progress. Please wait."
      renderEntry(CliPort.Role.Error, msg)
    else
      turnCount.incrementAndGet()
      turnInProgress = true
      turnWaitTimedOut = false
      startSpinner()
      try
        outCh.sendImmediately(GatewayMessage(Origin(id, threadKey, user), raw))
      catch case _: gears.async.ChannelClosedException => running = false
      val waitDeadlineMillis =
        System.currentTimeMillis() + CliPort.TurnMaxWaitMs
      var timedOut = false
      turnLock.synchronized:
        while turnInProgress && running && !timedOut do
          val now = System.currentTimeMillis()
          val remaining = waitDeadlineMillis - now
          if remaining <= 0L then timedOut = true
          else
            val waitMs = Math.min(remaining, CliPort.TurnWaitPollMs)
            try turnLock.wait(waitMs)
            catch case _: InterruptedException => ()
      if timedOut && turnInProgress && running then
        turnWaitTimedOut = true
        stopSpinner()
        renderEntry(
          CliPort.Role.Error,
          s"No turn completion after ${CliPort.TurnMaxWaitMs / 1000}s. Prompt recovered; new messages stay blocked until completion."
        )

  private def printGoodbye(): Unit =
    val turns = turnCount.get
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

  private def renderEntry(role: CliPort.Role, text: String): Unit =
    val (label, style) = role match
      case CliPort.Role.User =>
        ("› you", AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
      case CliPort.Role.Assistant =>
        (
          "• capybara",
          AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN)
        )
      case CliPort.Role.Error =>
        ("✗ error", AttributedStyle.DEFAULT.foreground(AttributedStyle.RED))
    val time = java.time.LocalTime.now.format(CliPort.TimeFormatter)
    val timeCol = s"$time "
    val prefix = s"$label > "
    val indent = " " * (timeCol.length + prefix.length)

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
    lines.tail.foreach(l => builder.append(indent).append(l).append("\n"))
    reader.printAbove(builder.toAttributedString)

  private def startSpinner(): Unit =
    if spinnerTask.get.isEmpty then
      spinnerFrame.set(0)
      val startMillis = System.currentTimeMillis()
      val startWordIdx = scala.util.Random.nextInt(CliPort.ThinkingWords.size)
      val runnable: Runnable = () =>
        val i = spinnerFrame.getAndIncrement()
        val frame = CliPort.spinnerFrameAt(i)
        val elapsedMs = System.currentTimeMillis() - startMillis
        val elapsedSec = elapsedMs / 1000.0
        val wordIdx =
          (startWordIdx + (elapsedMs / CliPort.ThinkingWordRotateMs).toInt) %
            CliPort.ThinkingWords.size
        val word = CliPort.ThinkingWords(wordIdx)
        renderStatus(f"$frame $word ($elapsedSec%.1fs)")
      val task = spinnerScheduler.scheduleAtFixedRate(
        runnable,
        0L,
        CliPort.SpinnerIntervalMs,
        TimeUnit.MILLISECONDS
      )
      spinnerTask.set(Some(task))

  private def stopSpinner(): Unit =
    spinnerTask.getAndSet(None).foreach(_.cancel(true))
    renderStatus("")

  private def renderStatus(text: String): Unit =
    status.foreach: st =>
      val line = AttributedStringBuilder()
        .style(AttributedStyle.DEFAULT.faint)
        .append(text)
        .toAttributedString
      try st.update(JList.of(line))
      catch case _: Throwable => ()

  private def cleanup(): Unit =
    stopSpinner()
    try status.foreach(_.close())
    catch case _: Throwable => ()
    try spinnerScheduler.shutdownNow()
    catch case _: Throwable => ()
    try terminal.writer().flush()
    catch case _: Throwable => ()
    closeTerminalSafely()
    try outCh.close()
    catch case _: Throwable => ()

  private def closeTerminalSafely(): Unit =
    if terminalOwnsStdio then
      try terminal.writer().flush()
      catch case _: Throwable => ()
    else
      try terminal.close()
      catch case _: Throwable => ()

object CliPort:
  val Id: String = "cli"
  val UserPrompt: String = "› "
  val QuitCommands: Set[String] = Set("quit", "/quit", "exit", "/exit")
  val SpinnerFrames: Vector[String] = Vector("(ᐢ•(ｪ)•ᐢ)", "(ᐢ-(ｪ)-ᐢ)")
  val SpinnerBlinkEvery: Int = 14
  val SpinnerIntervalMs: Long = 100L
  val TurnWaitPollMs: Long = 500L
  val TurnMaxWaitMs: Long = 120000L

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
