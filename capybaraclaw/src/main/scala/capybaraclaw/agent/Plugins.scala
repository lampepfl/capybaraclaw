package capybaraclaw.agent

import java.io.File

import tacit.core.{ApiMode, LoadedPlugin, PluginLoader}

object Plugins:
  private def dir(workDir: String): File = File(workDir, "plugins")

  def scanDirs(workDir: String): List[String] =
    val d = dir(workDir)
    Option.when(d.exists)(d.getCanonicalPath).toList

  def load(workDir: String): List[LoadedPlugin] =
    PluginLoader.loadAll(jars = Nil, scanDirs = scanDirs(workDir)) match
      case Right(plugins) => plugins
      case Left(err)      =>
        throw ConfigError(s"plugins: $err")

  def describe(plugin: LoadedPlugin): String =
    val m = plugin.manifest
    s"${m.name} ${m.version} (${renderApiMode(m.apiMode)})"

  def renderApiMode(mode: ApiMode): String = mode match
    case ApiMode.ExtendCore  => "extend-core"
    case ApiMode.ReplaceCore => "replace-core"
