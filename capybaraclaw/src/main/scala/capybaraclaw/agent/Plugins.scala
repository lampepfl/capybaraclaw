package capybaraclaw.agent

import java.io.File

import scala.annotation.tailrec

import org.slf4j.LoggerFactory

import tacit.core.{ApiMode, LoadedPlugin, PluginLoader}

/** Plugins are loaded from the workdir's `plugins/` folder for every new
  * session, so jars can be added while the gateway runs. A broken plugin is
  * the operator's to fix: it is logged and shown in the startup banner, and
  * sessions start without it. Nothing about it reaches users or the agent.
  */
object Plugins:
  private val logger = LoggerFactory.getLogger(getClass)

  /** The plugins to load, and why any jar, or the whole set, was left out. */
  final case class Report(loaded: List[LoadedPlugin], problems: List[String])

  private def dir(workDir: String): File = File(workDir, "plugins")

  def hasDir(workDir: String): Boolean = dir(workDir).exists

  /** Each jar loads on its own, so one broken jar does not take the others
    * down; plugins that require a plugin which did not load are skipped too.
    * The remaining checks across the set (duplicate ids, self-requirements,
    * cycles, mixed api modes) do not single out one plugin, so they load
    * nothing.
    */
  def scan(workDir: String): Report =
    val d = dir(workDir)
    if !d.exists then Report(Nil, Nil)
    else
      PluginLoader.scanDir(d.getCanonicalPath) match
        case Left(err)    => Report(Nil, List(err))
        case Right(paths) =>
          val (failed, loaded) = paths.partitionMap(PluginLoader.load)
          val (kept, skipped) = withRequirementsMet(loaded)
          val problems = failed ++ skipped
          PluginLoader.validateAndOrder(kept) match
            case Right(ordered) => Report(ordered, problems)
            case Left(err)      =>
              Report(Nil, problems :+ s"$err; no plugins loaded")

  /** Drops plugins that require one that is not loaded, repeating until
    * nothing changes, so a dropped plugin also drops whatever requires it.
    */
  @tailrec
  private def withRequirementsMet(
      plugins: List[LoadedPlugin],
      skipped: List[String] = Nil
  ): (List[LoadedPlugin], List[String]) =
    val ids = plugins.map(_.manifest.id).toSet
    val (met, unmet) = plugins.partition(_.manifest.requires.forall(ids))
    if unmet.isEmpty then (plugins, skipped)
    else
      val reasons = unmet.map: p =>
        val absent = p.manifest.requires.filterNot(ids).mkString(", ")
        s"${p.manifest.id} (${File(p.jarPath).getName}) skipped: requires $absent, not loaded"
      withRequirementsMet(met, skipped ++ reasons)

  /** Never throws; problems are logged. */
  def load(workDir: String): List[LoadedPlugin] =
    val report = scan(workDir)
    report.problems.foreach(p => logger.warn(s"plugins: $p"))
    report.loaded

  def describe(plugin: LoadedPlugin): String =
    val m = plugin.manifest
    s"${m.name} ${m.version} (${renderApiMode(m.apiMode)})"

  def renderApiMode(mode: ApiMode): String = mode match
    case ApiMode.ExtendCore  => "extend-core"
    case ApiMode.ReplaceCore => "replace-core"
