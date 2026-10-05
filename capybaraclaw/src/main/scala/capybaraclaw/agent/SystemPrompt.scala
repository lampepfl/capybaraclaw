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
          Map(
            "work_dir" -> config.workDir,
            "interface_source" -> loadInterfaceSource()
          )
        )
      ),
      Option.when(config.classifiedPaths.nonEmpty):
        renderResource(
          "prompts/classified-paths.md",
          Map(
            "paths" -> config.classifiedPaths.map(p => s"- $p").mkString("\n")
          )
        )
      ,
      loadClawMd(config.workDir).map: md =>
        renderResource(
          "prompts/project-instructions.md",
          Map("instructions" -> md)
        ),
      Option.when(config.memorySnapshot.sections.nonEmpty):
        renderMemory(config.memorySnapshot, freshBoundary())
    ).flatten

    sections.mkString("\n\n")

  /** `boundary` fences each memory block: it is fresh for every prompt, so a
    * stored entry cannot guess it to end its block early and pose as prompt.
    */
  private[agent] def renderMemory(
      snap: MemorySnapshot,
      boundary: String
  ): String =
    val blocks = snap.sections.map: s =>
      val access = if s.writable then "read-write" else "read-only"
      val content = if s.content.nonEmpty then s.content else "(empty)"
      s"""<memory target="${s.file.target}" visible="${s.about}" access="$access" usage="${s.pct}%" chars="${s.chars}/${s.file.capacity}" boundary="$boundary">
         |$content
         |</memory boundary="$boundary">""".stripMargin
    renderResource(
      "prompts/memory.md",
      Map(
        "conversation" -> snap.conversation,
        "boundary" -> boundary,
        "memory_blocks" -> blocks.mkString("\n\n")
      )
    )

  private def freshBoundary(): String =
    val bytes = Array.ofDim[Byte](8)
    java.security.SecureRandom().nextBytes(bytes)
    java.util.HexFormat.of().formatHex(bytes)

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

  private def loadInterfaceSource(): String =
    loadResource("Interface.scala")
      .getOrElse("(Interface.scala not found on classpath)")

  private def loadClawMd(workDir: String): Option[String] =
    val file = File(workDir, "CLAW.md")
    Option.when(file.exists())(readAll(Source.fromFile(file)))
