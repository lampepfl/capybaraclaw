package capybaraclaw.agent

import scala.collection.Map
import scala.io.Source
import scala.util.control.NonFatal

import tacit.agents.llm.endpoint.{EffortLevel, LLMConfig, ThinkingMode}

final class ConfigError(message: String) extends RuntimeException(message)

/** Configuration for a Claw agent instance.
  */
case class AgentConfig(
    workDir: String,
    provider: String = "openrouter",
    model: String = "minimax/minimax-m2.7",
    maxTokens: Int = 16000,
    thinking: Option[ThinkingMode] = None,
    classifiedPaths: List[String] = Nil,
    memorySnapshot: MemorySnapshot = MemorySnapshot.empty
):
  def toLLMConfig: LLMConfig =
    LLMConfig(
      model = model,
      systemPrompt = Some(SystemPrompt.build(this)),
      maxTokens = Some(maxTokens),
      thinking = thinking
    )

object AgentConfig:
  /** Load `${workDir}/claw.json` if present; otherwise use defaults. The
    * `thinking` mode is derived from the provider. A malformed file or a
    * wrong-typed field raises [[ConfigError]] with a message naming the file
    * and field.
    */
  def load(
      workDir: String,
      memorySnapshot: MemorySnapshot = MemorySnapshot.empty
  ): AgentConfig =
    val file = java.io.File(workDir, "claw.json")
    if !file.exists() then
      AgentConfig(workDir = workDir, memorySnapshot = memorySnapshot)
    else
      val raw = readAll(Source.fromFile(file))
      val obj =
        try ujson.read(raw).obj
        catch
          case NonFatal(e) =>
            throw ConfigError(
              s"claw.json is not a valid JSON object: ${e.getMessage}"
            )
      val provider =
        field(obj, "provider", "a string")(_.str).getOrElse("openrouter")
      AgentConfig(
        workDir = workDir,
        provider = provider,
        model = field(obj, "model", "a string")(_.str)
          .getOrElse("minimax/minimax-m2.7"),
        maxTokens =
          field(obj, "max_tokens", "a number")(_.num.toInt).getOrElse(16000),
        thinking = deriveThinking(provider),
        classifiedPaths = field(obj, "classified_paths", "an array of strings")(
          _.arr.map(_.str).toList
        ).getOrElse(Nil),
        memorySnapshot = memorySnapshot
      )

  /** Read an optional field through `extract`, turning any type mismatch into a
    * [[ConfigError]] that names the field and the expected shape. Returns None
    * when the key is absent so the caller can fall back to a default.
    */
  private def field[A](
      obj: Map[String, ujson.Value],
      key: String,
      expected: String
  )(extract: ujson.Value => A): Option[A] =
    obj
      .get(key)
      .map: v =>
        try extract(v)
        catch
          case NonFatal(_) =>
            throw ConfigError(s"claw.json: '$key' must be $expected")

  private def deriveThinking(provider: String): Option[ThinkingMode] =
    provider match
      case "anthropic"                        => Some(ThinkingMode.Budget(2048))
      case "openai" | "openrouter" | "ollama" =>
        Some(ThinkingMode.Effort(EffortLevel.Medium))
      case _ => None
