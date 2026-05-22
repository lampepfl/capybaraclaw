package capybaraclaw.gateway.port.cli

object CliCommands:
  val Quit: Set[String] = Set("quit", "/quit", "exit", "/exit")
  val Sessions: Set[String] = Set("/sessions")
  val Current: Set[String] = Set("/current")

  val All: Set[String] = Quit ++ Sessions ++ Current

  def isQuit(input: String): Boolean =
    Quit.contains(normalize(input))

  def isSessions(input: String): Boolean =
    Sessions.contains(normalize(input))

  def isCurrent(input: String): Boolean =
    Current.contains(normalize(input))

  def isSlashCommand(input: String): Boolean =
    normalize(input).startsWith("/")

  private def normalize(input: String): String =
    input.trim.toLowerCase
