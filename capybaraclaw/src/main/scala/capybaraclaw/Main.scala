package capybaraclaw

import capybaraclaw.gateway.{Gateway, JsonlContextProvider}
import capybaraclaw.gateway.port.Port
import capybaraclaw.gateway.port.slack.{SlackBot, SlackPort}
import capybaraclaw.gateway.port.cli.CliPort
import gears.async.Async
import gears.async.default.given
import language.experimental.captureChecking

/** Entrypoint of Capybara Claw.
  *
  * By default, only the CLI port is enabled. Pass `--enable-slack` to additionally
  * connect to Slack (requires `SLACK_BOT_TOKEN` and `SLACK_APP_TOKEN` in the env).
  */
@main def main(args: String*): Unit =
  val enableSlack = args.contains("--enable-slack")
  val unknown = args.filter(a => a.startsWith("--") && a != "--enable-slack")
  unknown.foreach(a => System.err.println(s"[claw] unknown flag: $a"))

  val workDirFile = java.io.File(".").getCanonicalFile
  val workDir = workDirFile.getPath

  printStartupInfo(workDir, enableSlack)

  val contextProvider = JsonlContextProvider(workDirFile)

  Async.blocking:
    val cli = CliPort()
    val slackPort: Option[SlackPort] =
      if enableSlack then Some(SlackPort(SlackBot.fromEnv())) else None

    val ports: List[Port] = slackPort.toList :+ cli

    try
      val gateway = Gateway(workDir, ports, contextProvider)
      println(s"Gateway ready. Ports: ${ports.map(_.id).mkString(", ")}.")
      slackPort.foreach(_.start())
      cli.start()
      gateway.run()
    finally
      slackPort.foreach(_.shutdown())
      cli.shutdown()

private def printStartupInfo(workDir: String, enableSlack: Boolean): Unit =
  val clawJsonExists = java.io.File(workDir, "claw.json").exists()
  val clawMdExists = java.io.File(workDir, "CLAW.md").exists()
  println("Capybara Claw Gateway")
  println(s"  workdir  : $workDir")
  println(s"  claw.json: ${if clawJsonExists then "found" else "defaults"}")
  println(s"  CLAW.md  : ${if clawMdExists then "found" else "not found"}")
  println(s"  slack    : ${if enableSlack then "enabled" else "disabled (pass --enable-slack to enable)"}")
  println()
