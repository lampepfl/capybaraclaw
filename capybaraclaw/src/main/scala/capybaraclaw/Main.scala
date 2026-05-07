package capybaraclaw

import caseapp.*

import capybaraclaw.gateway.Gateway
import capybaraclaw.gateway.port.Port
import capybaraclaw.gateway.port.cli.CliPort
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
    enableSlack: Boolean = false
)

private object ClawMain extends CaseApp[CliOptions]:
  override def exit(code: Int): Nothing =
    throw ClawCaseAppExit(code)

  def run(options: CliOptions, remainingArgs: RemainingArgs): Unit =
    val workDirFile =
      resolveWorkDir(remainingArgs.all.toList) match
        case Right(file) => file
        case Left(error) =>
          System.err.println(error)
          exit(1)
    val workDir = workDirFile.getPath

    printStartupInfo(workDir, options.enableSlack)

    Using
      .Manager: use =>
        val contextProvider = use(SqliteContextProvider(workDirFile))
        Async.blocking:
          val slackPort: Option[SlackPort] =
            if options.enableSlack then Some(use(SlackPort(SlackBot.fromEnv())))
            else None
          val cli = use(CliPort(workDirFile = workDirFile))
          val ports: List[Port] = slackPort.toList :+ cli
          val gateway = Gateway(workDir, ports, contextProvider)
          println(s"Gateway ready. Ports: ${ports.map(_.id).mkString(", ")}.")
          slackPort.foreach(_.start())
          val cliFuture = cli.start()

          Future:
            try cliFuture.awaitResult
            finally gateway.shutdown()
          gateway.run()
      .get

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
      Left(s"[claw] failed to resolve workdir '$path': ${errorMessage(e)}")

private def errorMessage(e: Throwable): String =
  Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)

private def printStartupInfo(workDir: String, enableSlack: Boolean): Unit =
  val clawJsonExists = File(workDir, "claw.json").exists()
  val clawMdExists = File(workDir, "CLAW.md").exists()
  val logFile =
    File(System.getProperty("user.home"), ".claw/logs/capybara.log").getPath
  println("Capybara Claw Gateway")
  println(s"  workdir  : $workDir")
  println(s"  claw.json: ${if clawJsonExists then "found" else "defaults"}")
  println(s"  CLAW.md  : ${if clawMdExists then "found" else "not found"}")
  println(s"  logs     : $logFile")
  println(s"  slack    : ${
      if enableSlack then "enabled"
      else "disabled (pass --enable-slack to enable)"
    }")
  println()
