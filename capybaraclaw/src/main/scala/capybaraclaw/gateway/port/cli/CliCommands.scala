package capybaraclaw.gateway.port.cli

object CliCommands:
  val Quit: Set[String] = Set("quit", "/quit", "exit", "/exit")

  val All: Set[String] = Quit

  def isQuit(input: String): Boolean =
    Quit.contains(input.trim.toLowerCase)
