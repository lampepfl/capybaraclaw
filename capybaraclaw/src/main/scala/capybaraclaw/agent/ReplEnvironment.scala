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
        "classifiedPaths" -> classifiedPaths
          .map(p => java.io.File(workDir, p).getCanonicalPath)
          .asJson
      )
    ),
    recorder = None
  )

  val repl: ReplSession = ReplSession.create(using context)

object ReplEnvironment:
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
