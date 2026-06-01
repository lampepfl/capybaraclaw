package capybaraclaw.agent

import java.io.{File, FileNotFoundException}
import java.util.regex.Matcher.quoteReplacement

import scala.io.Source
import scala.util.matching.Regex

/** Reads a [[Source]] fully and always closes it. */
private[agent] def readAll(source: Source): String =
  try source.mkString
  finally source.close()

private[agent] object SystemPrompt:
  private val Placeholder: Regex = raw"\{\{([a-zA-Z0-9_]+)\}\}".r

  private val resourceLoader = getClass.getClassLoader.nn

  def build(config: AgentConfig): String =
    val sections = List(
      Some(
        renderResource(
          "prompts/system.md",
          Map("work_dir" -> config.workDir)
        )
      ),
      Option.when(config.classifiedPaths.nonEmpty):
        renderResource(
          "prompts/classified-paths.md",
          Map(
            "output_hint" -> classifiedOutputHint(config),
            "paths" -> config.classifiedPaths.map(p => s"- $p").mkString("\n")
          )
        )
      ,
      loadClawMd(config.workDir).map: md =>
        renderResource(
          "prompts/project-instructions.md",
          Map("instructions" -> md)
        ),
      Some(renderMemory(config.memorySnapshot))
    ).flatten

    sections.mkString("\n\n")

  private def renderMemory(snap: MemorySnapshot): String =
    renderResource(
      "prompts/memory.md",
      Map(
        "memory_usage" -> snap.memoryPct.toString,
        "memory_chars" -> snap.memoryChars.toString,
        "memory_capacity" -> MemoryFile.Memory.capacity.toString,
        "memory_content" ->
          Option.when(snap.memory.nonEmpty)(snap.memory).getOrElse("(empty)"),
        "user_usage" -> snap.userPct.toString,
        "user_chars" -> snap.userChars.toString,
        "user_capacity" -> MemoryFile.User.capacity.toString,
        "user_content" ->
          Option.when(snap.user.nonEmpty)(snap.user).getOrElse("(empty)")
      )
    )

  private[agent] def renderResource(
      path: String,
      values: Map[String, String]
  ): String =
    Placeholder.replaceAllIn(
      loadRequiredResource(path).stripSuffix("\n"),
      m =>
        val key = m.group(1).nn
        val value = values.getOrElse(
          key,
          throw IllegalStateException(
            s"No value for placeholder {{$key}} in template: $path"
          )
        )
        quoteReplacement(value)
    )

  private def loadResource(path: String): Option[String] =
    try Some(readAll(Source.fromResource(path, resourceLoader)))
    catch case _: FileNotFoundException => None

  private def loadRequiredResource(path: String): String =
    loadResource(path).getOrElse:
      throw IllegalStateException(s"Missing system prompt template: $path")

  /** The classified-output instruction depends on whether plugins are loaded:
    * a plugin (replace-core) exposes its own write function, surfaced by
    * `show_interface`; the bare core uses `writeClassified`.
    */
  private def classifiedOutputHint(config: AgentConfig): String =
    if config.pluginsConfigured then
      "Use the plugin's documented classified output function, e.g. " +
        "`writePrivateAnswer`, exactly as shown by `show_interface`."
    else "Use `writeClassified` exactly as shown by `show_interface`."

  private def loadClawMd(workDir: String): Option[String] =
    val file = File(workDir, "CLAW.md")
    Option.when(file.exists())(readAll(Source.fromFile(file)))
