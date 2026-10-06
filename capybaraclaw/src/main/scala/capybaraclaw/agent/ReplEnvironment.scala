package capybaraclaw.agent

import tacit.core.{Context as TacitContext, Config as TacitConfig}
import tacit.executor.ReplSession
import tacit.library.Interface as TacitLibraryInterface

import io.circe.Json
import io.circe.syntax.*

import scala.util.Try

final class ReplEnvironment(workDir: String, classifiedPaths: List[String]):
  private val context: TacitContext = TacitContext(
    TacitConfig(
      libraryJarPath = ReplEnvironment.resolveLibraryJarPath(),
      libraryConfig = Json.obj(
        "classifiedPaths" -> ReplEnvironment
          .classifiedPatterns(workDir, classifiedPaths)
          .asJson
      )
    ),
    recorder = None
  )

  val repl: ReplSession = ReplSession.create(using context)

object ReplEnvironment:
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
