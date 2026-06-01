package capybaraclaw.agent

import tacit.core.{
  Context as TacitContext,
  Config as TacitConfig,
  LoadedPlugin,
  PluginLoader
}
import tacit.executor.ReplSession
import tacit.library.Interface as TacitLibraryInterface

import io.circe.Json
import io.circe.syntax.*

import scala.util.Try

/** Owns the TACIT REPL for one workdir: resolves the library JAR, loads any
  * plugins under `${workDir}/plugins/`, and builds the capability-scoped
  * [[TacitContext]] the REPL runs in. `loadedPlugins` is exposed so the agent's
  * `show_interface` tool can compose its API surface from what was actually
  * loaded.
  */
final class ReplEnvironment(
    workDir: String,
    classifiedPaths: List[String],
    privateLlm: Option[PrivateLlmConfig] = None
):
  // The safe-libs REPL preamble reads these JVM properties to scope its
  // `given FileSystem[FullAccess]` to this directory and to reach the private
  // LLM. They must be set before TacitContext starts the REPL, so the preamble
  // sees them when it runs. Last agent in this JVM wins; that's fine for the
  // current single-workdir gateway.
  System.setProperty("safemode.workdir", workDir)
  privateLlm match
    case Some(p) =>
      System.setProperty("safemode.private.provider", p.provider)
      System.setProperty("safemode.private.baseUrl", p.baseUrl)
      System.setProperty("safemode.private.model", p.model)
    case None =>
      System.clearProperty("safemode.private.provider")
      System.clearProperty("safemode.private.baseUrl")
      System.clearProperty("safemode.private.model")

  private val pluginsDir: String =
    java.io.File(workDir, "plugins").getCanonicalPath

  private val pluginScanDirs: List[String] =
    if java.io.File(pluginsDir).isDirectory then List(pluginsDir) else Nil

  val loadedPlugins: List[LoadedPlugin] =
    PluginLoader.loadAll(jars = Nil, scanDirs = pluginScanDirs) match
      case Left(err) =>
        System.err.println(s"Error: $err")
        sys.exit(1)
      case Right(plugins) => plugins

  private val context: TacitContext = TacitContext(
    TacitConfig(
      libraryJarPath = ReplEnvironment.resolveLibraryJarPath(),
      libraryConfig = Json.obj(
        "classifiedPaths" -> classifiedPaths
          .map(p => java.io.File(workDir, p).getCanonicalPath)
          .asJson
      ),
      pluginScanDirs = pluginScanDirs
    ),
    recorder = None,
    plugins = loadedPlugins
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
