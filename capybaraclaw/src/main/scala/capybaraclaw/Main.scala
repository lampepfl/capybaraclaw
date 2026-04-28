package capybaraclaw

import caseapp.*
import capybaraclaw.gateway.{Gateway, JsonlContextProvider}
import capybaraclaw.gateway.port.Port
import capybaraclaw.gateway.port.slack.{SlackBot, SlackPort}
import capybaraclaw.gateway.port.cli.CliPort
import gears.async.{Async, Future}
import gears.async.default.given
import language.experimental.captureChecking

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
      throw RuntimeException(
        s"[claw] argument parsing failed (exit code $code)"
      )

@ProgName("claw")
private final case class CliOptions(
    @HelpMessage("Enable Slack Socket Mode port")
    enableSlack: Boolean = false
)

private object ClawMain extends CaseApp[CliOptions]:
  override def exit(code: Int): Nothing =
    throw ClawCaseAppExit(code)

  def run(options: CliOptions, remainingArgs: RemainingArgs): Unit =
    val workDirFile = resolveWorkDir(remainingArgs.all.toList)
    val workDir = workDirFile.getPath

    printStartupInfo(workDir, options.enableSlack)

    val contextProvider = JsonlContextProvider(workDirFile)

    Async.blocking:
      val cli = CliPort(workDirFile = workDirFile)
      val slackPort: Option[SlackPort] =
        if options.enableSlack then Some(SlackPort(SlackBot.fromEnv()))
        else None

      val ports: List[Port] = slackPort.toList :+ cli

      try
        val gateway = Gateway(workDir, ports, contextProvider)
        println(s"Gateway ready. Ports: ${ports.map(_.id).mkString(", ")}.")
        slackPort.foreach(_.start())
        val cliFuture = cli.start()

        Future:
          try cliFuture.awaitResult
          finally slackPort.foreach(_.shutdown())
        gateway.run()
      finally
        slackPort.foreach(_.shutdown())
        cli.shutdown()

private final case class ClawCaseAppExit(code: Int)
    extends RuntimeException(null, null, false, false)

private def resolveWorkDir(positional: List[String]): java.io.File =
  positional match
    case Nil      => java.io.File(".").getCanonicalFile
    case p :: Nil => java.io.File(p).getCanonicalFile
    case many     =>
      throw IllegalArgumentException(
        s"[claw] expected at most one workdir, got: ${many.mkString(", ")}"
      )

private def printStartupInfo(workDir: String, enableSlack: Boolean): Unit =
  val clawJsonExists = java.io.File(workDir, "claw.json").exists()
  val clawMdExists = java.io.File(workDir, "CLAW.md").exists()
  val logFile = java.io
    .File(System.getProperty("user.home"), ".claw/logs/capybara.log")
    .getPath
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
