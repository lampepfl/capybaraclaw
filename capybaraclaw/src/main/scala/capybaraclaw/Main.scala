package capybaraclaw

import capybaraclaw.connectors.slack.SlackBot
import capybaraclaw.gateway.{Gateway, JsonlContextProvider, SlackPort, CliPort}
import gears.async.Async
import gears.async.default.given
import language.experimental.captureChecking

/** Entrypoint of Capybara Claw.
  *
  * Boots a Gateway with two Ports (Slack + CLI) and one JSONL-backed ContextProvider
  * rooted at the working directory. The Gateway lazily spawns a `ClawAgent` per
  * (port, thread) on first message, rehydrating prior conversation from disk.
  */
@main def main(): Unit =
  val workDirFile = java.io.File(".").getCanonicalFile
  val workDir = workDirFile.getPath

  printStartupInfo(workDir)

  val contextProvider = JsonlContextProvider(workDirFile)

  Async.blocking:
    SlackBot.usingBot: bot =>
      val slack = SlackPort(bot)
      val cli = CliPort()

      val gateway = Gateway(workDir, List(slack, cli), contextProvider)

      slack.start()
      cli.start()

      println("Gateway ready. Ports: slack, cli. Ctrl+C to stop.")
      gateway.run()

private def printStartupInfo(workDir: String): Unit =
  val clawJsonExists = java.io.File(workDir, "claw.json").exists()
  val clawMdExists = java.io.File(workDir, "CLAW.md").exists()
  println("Capybara Claw Gateway")
  println(s"  workdir  : $workDir")
  println(s"  claw.json: ${if clawJsonExists then "found" else "defaults"}")
  println(s"  CLAW.md  : ${if clawMdExists then "found" else "not found"}")
  println()
