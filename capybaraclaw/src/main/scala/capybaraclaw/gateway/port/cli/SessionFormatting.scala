package capybaraclaw.gateway.port.cli

import capybaraclaw.gateway.SessionId

object SessionFormatting:

  def resumeCommand(sessionId: SessionId): String =
    s"claw -r $sessionId"

  def turnsLabel(turnCount: Int): String =
    if turnCount == 1 then "1 turn" else s"$turnCount turns"

  /** Replace a `$HOME` prefix with `~`. Returns the path unchanged when it does
    * not live under the given home directory.
    */
  def tildify(
      path: String,
      homeDir: Option[String] = Option(System.getProperty("user.home"))
  ): String =
    homeDir match
      case Some(home) if home.nonEmpty && path == home => "~"
      case Some(home) if path.startsWith(s"$home/")    =>
        "~" + path.drop(home.length)
      case _ => path

  def formatSessionsList(
      entries: List[(SessionId, String, Long)],
      currentId: Option[SessionId],
      nowMillis: Long,
      homeDir: Option[String] = Option(System.getProperty("user.home"))
  ): String =
    if entries.isEmpty then "No sessions recorded."
    else
      val noun = if entries.size == 1 then "session" else "sessions"
      val header = s"${entries.size} $noun:"
      val sections = entries
        .groupBy(_._2)
        .toList
        .sortBy { case (_, items) => -items.map(_._3).max }
        .map: (workdir, items) =>
          val rows = items
            .sortBy(-_._3)
            .map: (id, _, lastActivityMillis) =>
              val age = formatAge((nowMillis - lastActivityMillis) / 1000)
              currentId match
                case Some(curr) =>
                  val marker = if id == curr then ">" else " "
                  s"  $marker $id   ($age)"
                case None =>
                  s"  $id   ($age)"
          (tildify(workdir, homeDir) :: rows).mkString("\n")
      (header :: sections).mkString("\n\n")

  def formatAge(elapsedSec: Long): String =
    if elapsedSec < 60 then "just now"
    else if elapsedSec < 3600 then s"${elapsedSec / 60}m ago"
    else if elapsedSec < 86400 then s"${elapsedSec / 3600}h ago"
    else s"${elapsedSec / 86400}d ago"
