package capybaraclaw.util

import java.nio.file.{Path, Paths}

/** Capybara's own state: `state.db`, `identities.json`, `memories/` and
  * `logs/`. `~/.claw` unless `CAPYBARACLAW_HOME_DIR` names another directory,
  * e.g. to keep a demo's people, transcripts and memories apart from the
  * operator's.
  */
object ClawHome:
  val EnvVar = "CAPYBARACLAW_HOME_DIR"

  /** The system property `logback.xml` reads, set by [[publish]]. */
  val LogProperty = "claw.home"

  def dir: Path = resolve(sys.env.get(EnvVar), System.getProperty("user.home"))

  /** A blank value counts as unset; a leading `~` is the user's home and a
    * relative path is taken from the current directory.
    */
  def resolve(configured: Option[String], home: String): Path =
    configured.map(_.trim).filter(_.nonEmpty) match
      case None => Paths.get(home, ".claw")
      case Some(p) =>
        val expanded =
          if p == "~" then home
          else if p.startsWith("~/") then home + p.drop(1)
          else p
        Paths.get(expanded).toAbsolutePath.normalize

  /** Lets `logback.xml` log under [[dir]], which it cannot work out itself
    * (it does not expand `~`). Call before the first logger is created.
    */
  def publish(): Unit =
    val _ = System.setProperty(LogProperty, dir.toString)
