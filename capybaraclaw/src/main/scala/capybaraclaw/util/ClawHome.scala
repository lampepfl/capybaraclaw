package capybaraclaw.util

import capybaraclaw.agent.ConfigError

import java.io.File
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Capybara's own state: `state.db`, `identities.json`, `memories/` and
  * `logs/`. `~/.claw` unless `CAPYBARACLAW_HOME_DIR` names another directory,
  * e.g. to keep a demo's people, transcripts and memories apart from the
  * operator's.
  *
  * The agent must never reach it: whoever can write `identities.json` gives
  * themselves any role, and `state.db` and `memories/` hold everyone's
  * transcripts and memories. So it may not overlap the workdir
  * ([[requireApart]]), and the sandbox treats it as read-only and classified
  * wherever it is (`ReplEnvironment`).
  */
object ClawHome:
  val EnvVar = "CAPYBARACLAW_HOME_DIR"

  /** The system property `logback.xml` reads, set by [[publish]]. */
  val LogProperty = "claw.home"

  /** Resolved once, so the database, identities, memories and logs all use
    * the same directory. Symlinks are resolved: the overlap check and the
    * sandbox's patterns compare real paths.
    */
  lazy val dir: Path =
    canonical(
      resolve(
        sys.env.get(EnvVar),
        System.getProperty("user.home"),
        Paths.get("").toAbsolutePath
      )
    )

  /** A blank value counts as unset. `~/…` is under the user's home and a
    * relative path is taken from `cwd`. `~` alone (the state would sit
    * loose in the home directory) and `~user` are refused.
    */
  def resolve(configured: Option[String], home: String, cwd: Path): Path =
    configured.map(_.trim).filter(_.nonEmpty) match
      case None                             => Paths.get(home, ".claw")
      case Some(p) if p == "~" || p == "~/" =>
        throw ConfigError(
          s"$EnvVar must name a directory of its own, not your home directory, e.g. ~/.claw-demo."
        )
      case Some(p) if p.startsWith("~") && !p.startsWith("~/") =>
        throw ConfigError(
          s"$EnvVar: '$p' is not supported; use ~/… or an absolute path."
        )
      case Some(p) =>
        val expanded = if p.startsWith("~/") then home + p.drop(1) else p
        cwd.resolve(expanded).normalize

  /** The real path: symlinks in the part that exists are resolved. */
  def canonical(path: Path): Path = File(path.toString).getCanonicalFile.toPath

  /** Refuses a state directory inside the workdir or containing it: either
    * way agent code could read and rewrite it without asking anyone.
    */
  def requireApart(workDir: Path, home: Path): Unit =
    val w = canonical(workDir)
    val h = canonical(home)
    if h.startsWith(w) || w.startsWith(h) then
      throw ConfigError(
        s"Capybara's state directory $h overlaps the workdir $w, where the agent could read and change it. " +
          s"Set $EnvVar to a directory outside the workdir (and not containing it)."
      )

  /** Creates the state directory owner-only (`rwx------`) if it does not
    * exist yet; for an existing one, the returned warnings say if others can
    * read or write it (a `~/.claw` from before this check is usually
    * `rwxr-xr-x`, its `state.db` `rw-r--r--`) or someone else owns it. Its
    * permissions are left alone.
    */
  def prepare(home: Path, user: String): List[String] =
    if !Files.exists(home) then
      Option(home.getParent).foreach(Files.createDirectories(_))
      try
        val _ = Files.createDirectory(
          home,
          PosixFilePermissions.asFileAttribute(
            PosixFilePermissions.fromString("rwx------")
          )
        )
      catch
        case _: UnsupportedOperationException =>
          val _ = Files.createDirectory(home)
      Nil
    else
      import PosixFilePermission.*
      val perms = Try(
        Files.getPosixFilePermissions(home).asScala.toSet
      ).toOption
      val shared = perms.flatMap: p =>
        if p(GROUP_WRITE) || p(OTHERS_WRITE) then
          Some(
            s"$home is writable by other users; transcripts, roles and memories there are not protected."
          )
        else if p(GROUP_READ) || p(OTHERS_READ) then
          Some(
            s"$home is readable by other users, and so are the transcripts and memories in it; consider chmod 700 $home."
          )
        else None
      val foreign = Try(Files.getOwner(home).getName).toOption
        .filter(_ != user)
        .map(owner => s"$home is owned by $owner, not $user.")
      shared.toList ++ foreign

  /** Lets `logback.xml` log under [[dir]], which it cannot work out itself.
    * Call before the first logger is created.
    */
  def publish(): Unit =
    val _ = System.setProperty(LogProperty, dir.toString)
