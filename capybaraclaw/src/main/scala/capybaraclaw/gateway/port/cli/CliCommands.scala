package capybaraclaw.gateway.port.cli

import capybaraclaw.gateway.ApprovalDecision

object CliCommands:
  val Quit: Set[String] = Set("/quit", "/exit")
  val Sessions: Set[String] = Set("/sessions")
  val Current: Set[String] = Set("/current")
  val Approve: String = "/approve"
  val Deny: String = "/deny"

  val All: Set[String] = Quit ++ Sessions ++ Current ++ Set(Approve, Deny)

  final case class ApprovalCommand(
      requestId: Option[Int],
      decision: ApprovalDecision
  )

  def isQuit(input: String): Boolean =
    Quit.contains(normalize(input))

  def isSessions(input: String): Boolean =
    Sessions.contains(normalize(input))

  def isCurrent(input: String): Boolean =
    Current.contains(normalize(input))

  def isSlashCommand(input: String): Boolean =
    normalize(input).startsWith("/")

  /** `/approve [n]` or `/deny [n]` (`n` may be written `#n`). `None` for any
    * other input, `Left` with a usage message for a malformed request id.
    */
  def parseApproval(input: String): Option[Either[String, ApprovalCommand]] =
    normalize(input).split("\\s+").toList match
      case command :: args if command == Approve || command == Deny =>
        val decision =
          if command == Approve then ApprovalDecision.Approve
          else ApprovalDecision.Deny
        val requestId = args match
          case Nil       => Right(None)
          case List(arg) =>
            arg
              .stripPrefix("#")
              .toIntOption
              .filter(_ > 0)
              .map(Some(_))
              .toRight(usage(command))
          case _ => Left(usage(command))
        Some(requestId.map(ApprovalCommand(_, decision)))
      case _ => None

  enum CommandStatus:
    case Known, InProgress, Unknown, Plain

  def commandStatus(input: String): CommandStatus =
    val needle = normalize(input)
    if !needle.startsWith("/") then CommandStatus.Plain
    else if All.contains(needle) then CommandStatus.Known
    else if Set(Approve, Deny).contains(commandWord(needle)) then
      CommandStatus.Known
    else if All.exists(_.startsWith(needle)) then CommandStatus.InProgress
    else CommandStatus.Unknown

  /** The command without its arguments, e.g. `/approve` for `/approve 3`. */
  def commandWord(input: String): String =
    normalize(input).takeWhile(!_.isWhitespace)

  private def usage(command: String): String =
    s"Usage: $command [request number], e.g. $command 3."

  private def normalize(input: String): String =
    input.trim.toLowerCase
