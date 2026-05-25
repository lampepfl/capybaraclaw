package capybaraclaw.gateway.port.cli

object CliCommands:
  val Quit: Set[String] = Set("/quit", "/exit")
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

  enum CommandStatus:
    case Known, InProgress, Unknown, Plain

  def commandStatus(input: String): CommandStatus =
    val needle = normalize(input)
    if !needle.startsWith("/") then CommandStatus.Plain
    else if All.contains(needle) then CommandStatus.Known
    else if All.exists(_.startsWith(needle)) then CommandStatus.InProgress
    else CommandStatus.Unknown

  private def normalize(input: String): String =
    input.trim.toLowerCase
