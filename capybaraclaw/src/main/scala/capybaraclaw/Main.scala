package capybaraclaw

import caseapp.*

import capybaraclaw.gateway.{Gateway, SessionId, SessionMetadata}
import capybaraclaw.gateway.port.Port
import capybaraclaw.gateway.port.cli.{CliPort, SessionFormatting}
import capybaraclaw.gateway.port.slack.{SlackBot, SlackPort}
import capybaraclaw.gateway.sqlite.SqliteContextProvider

import gears.async.{Async, Future}
import gears.async.default.given

import java.io.File

import language.experimental.captureChecking

import scala.util.Using
import scala.util.control.NonFatal

/** Entrypoint of Capybara Claw.
  *
  * By default, only the CLI port is enabled. Pass `--enable-slack` to additionally
  * connect to Slack (requires `SLACK_BOT_TOKEN` and `SLACK_APP_TOKEN` in the env).
  * An optional positional argument sets the working directory (defaults to `.`).
  */
@main def main(args: String*): Unit =
  try ClawMain.main(args.toArray)
  catch
    case ClawCaseAppExit(0) =>
      ()
    case ClawCaseAppExit(code) =>
      sys.exit(code)

@ProgName("claw")
private final case class CliOptions(
    @HelpMessage("Enable Slack Socket Mode port")
    enableSlack: Boolean = false,
    @HelpMessage("Resume a previous session by id")
    @ValueDescription("session-id")
    @ExtraName("r")
    resume: Option[String] = None,
    @HelpMessage("Continue the most recent session")
    @ExtraName("c")
    continue: Boolean = false,
    @HelpMessage("List all known sessions and exit")
    @ExtraName("l")
    listSessions: Boolean = false
)

private object ClawMain extends CaseApp[CliOptions]:
  override def exit(code: Int): Nothing =
    throw ClawCaseAppExit(code)

  def run(options: CliOptions, remainingArgs: RemainingArgs): Unit =
    if options.listSessions then printSessionsAndExit()
    else runGateway(options, remainingArgs)

  private def printSessionsAndExit(): Unit =
    Using.resource(SqliteContextProvider()): provider =>
      val entries = provider
        .listSessions()
        .map(m => (m.id, m.workdir, m.lastActivity.toEpochMilli))
      println(
        SessionFormatting.formatSessionsList(
          entries = entries,
          currentId = None,
          nowMillis = System.currentTimeMillis()
        )
      )

  private def runGateway(
      options: CliOptions,
      remainingArgs: RemainingArgs
  ): Unit =
    Using
      .Manager: use =>
        val contextProvider = use(SqliteContextProvider())
        val (sessionId, workDirFile) =
          bootstrapSession(contextProvider, options, remainingArgs)
        val workDir = workDirFile.getPath

        Async.blocking:
          val slackPort: Option[SlackPort] =
            if options.enableSlack then Some(use(SlackPort(SlackBot.fromEnv())))
            else None

          printStartupInfo(workDir, options.enableSlack)

          val cli = use(
            CliPort(
              workDirFile = workDirFile,
              sessionId = sessionId,
              contextProvider = contextProvider
            )
          )
          val ports: List[Port] = slackPort.toList :+ cli
          val gateway = Gateway(workDir, ports, contextProvider)
          println(s"Gateway ready. Ports: ${ports.map(_.id).mkString(", ")}.\n")
          slackPort.foreach(_.start())
          val cliFuture = cli.start()

          Future:
            try cliFuture.awaitResult
            finally gateway.shutdown()
          gateway.run()
      .get

  private def bootstrapSession(
      contextProvider: SqliteContextProvider,
      options: CliOptions,
      remainingArgs: RemainingArgs
  ): (SessionId, File) =
    (options.resume, options.continue) match
      case (Some(_), true) =>
        System.err.println(
          "[claw] --resume and --continue cannot be combined"
        )
        exit(1)
      case (Some(raw), false) =>
        resumeById(contextProvider, raw, remainingArgs)
      case (None, true) =>
        resumeLatest(contextProvider, remainingArgs)
      case (None, false) =>
        createNewSession(contextProvider, remainingArgs)

  private def resumeById(
      contextProvider: SqliteContextProvider,
      raw: String,
      remainingArgs: RemainingArgs
  ): (SessionId, File) =
    val sessionId =
      try SessionId(raw)
      catch
        case _: IllegalArgumentException =>
          System.err.println(s"[claw] invalid session id: $raw")
          exit(1)
    contextProvider.findSession(sessionId) match
      case None =>
        System.err.println(s"[claw] session '$raw' not found")
        exit(1)
      case Some(meta) =>
        adoptStoredWorkdir(meta, raw, remainingArgs)

  private def resumeLatest(
      contextProvider: SqliteContextProvider,
      remainingArgs: RemainingArgs
  ): (SessionId, File) =
    contextProvider.listSessions().headOption match
      case None =>
        System.err.println("[claw] no sessions recorded")
        exit(1)
      case Some(meta) =>
        adoptStoredWorkdir(meta, meta.id.toString, remainingArgs)

  private def createNewSession(
      contextProvider: SqliteContextProvider,
      remainingArgs: RemainingArgs
  ): (SessionId, File) =
    val workDirFile = resolveWorkDirOrExit(remainingArgs)
    val sessionId = contextProvider.createSession(workDirFile.getPath)
    (sessionId, workDirFile)

  private def adoptStoredWorkdir(
      meta: SessionMetadata,
      raw: String,
      remainingArgs: RemainingArgs
  ): (SessionId, File) =
    val dir = File(meta.workdir)
    if !dir.isDirectory then
      System.err.println(
        s"[claw] session $raw: workdir gone: ${meta.workdir}"
      )
      exit(1)
    if remainingArgs.all.nonEmpty then
      System.err.println(
        s"[claw] --resume ignores positional workdir; using stored: ${meta.workdir}"
      )
    (meta.id, dir.getCanonicalFile)

  private def resolveWorkDirOrExit(remainingArgs: RemainingArgs): File =
    resolveWorkDir(remainingArgs.all.toList) match
      case Right(file) => file
      case Left(error) =>
        System.err.println(error)
        exit(1)

private final case class ClawCaseAppExit(code: Int)
    extends RuntimeException(null, null, false, false)

private def resolveWorkDir(
    positional: List[String]
): Either[String, File] =
  positional match
    case Nil      => canonicalFile(".")
    case p :: Nil => canonicalFile(p)
    case many     =>
      Left(
        s"[claw] expected at most one workdir, got: ${many.mkString(", ")}"
      )

private def canonicalFile(path: String): Either[String, File] =
  try Right(File(path).getCanonicalFile)
  catch
    case NonFatal(e) =>
      Left(
        s"[claw] failed to resolve workdir '$path': ${Throwables.errorMessage(e)}"
      )

private def printStartupInfo(
    workDir: String,
    enableSlack: Boolean
): Unit =
  val clawJsonExists = File(workDir, "claw.json").exists()
  val clawMdExists = File(workDir, "CLAW.md").exists()
  val logFile =
    File(System.getProperty("user.home"), ".claw/logs/capybara.log").getPath
  println("Capybara Claw Gateway")
  println(s"  claw.json: ${if clawJsonExists then "found" else "defaults"}")
  println(s"  CLAW.md  : ${if clawMdExists then "found" else "not found"}")
  println(s"  logs     : $logFile")
  println(s"  slack    : ${
      if enableSlack then "enabled"
      else "disabled (pass --enable-slack to enable)"
    }")
  println()
