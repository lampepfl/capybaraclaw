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
        "allowedRoots" -> List(java.io.File(workDir).getCanonicalPath).asJson,
        "commandPermissions" -> List.empty[String].asJson,
        "networkPermissions" -> List.empty[String].asJson,
        "classifiedPaths" -> classifiedPaths
          .map(p => java.io.File(workDir, p).getCanonicalPath)
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
