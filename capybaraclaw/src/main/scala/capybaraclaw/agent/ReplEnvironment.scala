package capybaraclaw.agent

import tacit.core.{Context as TacitContext, Config as TacitConfig, LoadedPlugin}
import tacit.executor.{ManagedRepl, ReplSession}
import tacit.library.Interface as TacitLibraryInterface

import io.circe.Json
import io.circe.syntax.*

import scala.util.Try

final class ReplEnvironment(
    workDir: String,
    classifiedPaths: List[String],
    permissionOracle: Option[String => String] = None
):
  val loadedPlugins: List[LoadedPlugin] = Plugins.load(workDir)

  private val context: TacitContext = TacitContext(
    TacitConfig(
      libraryJarPath = ReplEnvironment.resolveLibraryJarPath(),
      libraryConfig = Json.obj(
        "readOnlyPaths" -> ReplEnvironment.readOnlyPaths(workDir).asJson,
        "allowedRoots" -> List(java.io.File(workDir).getCanonicalPath).asJson,
        "commandPermissions" -> List.empty[String].asJson,
        "networkPermissions" -> List.empty[String].asJson,
        "classifiedPaths" -> ReplEnvironment
          .classifiedPatterns(workDir, classifiedPaths)
          .asJson
      )
    ),
    recorder = None,
    plugins = loadedPlugins,
    permissionOracle = permissionOracle
  )

  val coreInterface: String =
    ManagedRepl
      .readLibraryResource("Interface.scala.txt")(using context)
      .getOrElse:
        throw IllegalStateException(
          "Interface.scala.txt missing from tacit-library JAR (build issue)"
        )

  val repl: ReplSession = ReplSession.create(using context)

object ReplEnvironment:
  /** Workdir files that steer the gateway itself: plugin jars run as trusted
    * code in every new session, and `claw.json` and `CLAW.md` configure it.
    * The agent may read them but never change them; tacit resolves the paths
    * through symlinks and matches them case-insensitively.
    */
  private[agent] def readOnlyPaths(workDir: String): List[String] =
    val base = java.nio.file.Path.of(workDir).toAbsolutePath.normalize
    List("plugins", "claw.json", "CLAW.md").map(base.resolve(_).toString)

  /** Mirrors tacit's `InterfaceImpl.DefaultClassifiedPatterns`, which tacit
    * only applies when `classifiedPaths` is absent from the library config.
    * We always send the key, so the defaults are merged in here instead.
    */
  private val DefaultClassifiedPatterns: List[String] = List(
    ".ssh",
    ".gnupg",
    ".env",
    ".env.*",
    ".netrc",
    ".npmrc",
    ".pypirc",
    ".docker",
    ".kube",
    ".aws",
    ".azure",
    ".gcloud"
  )

  /** Turn `classified_paths` entries into tacit classified patterns, merged
    * with [[DefaultClassifiedPatterns]]. Tacit matches a pattern without `/`
    * against any path component, an absolute pattern against the full path,
    * and a relative pattern with `/` against the root the agent requested.
    * Only the last kind is resolved against `workDir`, so that it means the
    * same thing whatever root the agent picks. `~` expands to the user's home
    * and `$workdir` to `workDir`, so `$workdir/emails` classifies only the
    * top-level `emails` while a bare `emails` matches at any depth.
    */
  private val WorkDirVar = "$workdir"

  private[agent] def classifiedPatterns(
      workDir: String,
      classifiedPaths: List[String]
  ): List[String] =
    val home = System.getProperty("user.home")
    def under(base: String, rest: String): String =
      java.nio.file.Path.of(base).resolve(rest).normalize.toString
    val user = classifiedPaths.map: p =>
      if p == "~" || p.startsWith("~/") then under(home, p.drop(2))
      else if p == WorkDirVar || p.startsWith(s"$WorkDirVar/") then
        under(workDir, p.drop(WorkDirVar.length + 1))
      else if p.startsWith("/") || !p.stripSuffix("/").contains('/') then p
      else under(workDir, p)
    (DefaultClassifiedPatterns ++ user).distinct

  private def resolveLibraryJarPath(): String =
    val fromProperty =
      Option(System.getProperty("tacit.library.jar")).filter(_.nonEmpty)
    val fromCodeSource =
      Try:
        val url = classOf[
          TacitLibraryInterface
        ].getProtectionDomain.getCodeSource.getLocation
        java.io.File(url.toURI).getAbsolutePath
      .toOption
        .filter(_.nonEmpty)

    fromProperty
      .orElse(fromCodeSource)
      .getOrElse:
        throw RuntimeException(
          "Unable to resolve tacit-library path. Set -Dtacit.library.jar or ensure tacit-library is on classpath."
        )
