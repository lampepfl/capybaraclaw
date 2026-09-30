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
    classifiedPaths: List[String] = Nil
):
  def toLLMConfig: LLMConfig =
    LLMConfig(
      model = model,
      systemPrompt = Some(SystemPrompt.build(this)),
      maxTokens = Some(maxTokens),
      thinking = thinking
    )

object AgentConfig:
  private val knownProviders =
    List("anthropic", "openai", "openrouter", "ollama")

  /** Load `${workDir}/claw.json` if present; otherwise use defaults. The
    * `thinking` mode is derived from the provider. An unreadable or malformed
    * file, a wrong-typed field or an unknown provider raises [[ConfigError]]
    * with a message naming the file and field.
    */
  def load(workDir: String): AgentConfig =
    val file = java.io.File(workDir, "claw.json")
    val path = file.getPath
    val obj =
      if !file.exists() then ujson.Obj().value
      else
        val raw =
          try readAll(Source.fromFile(file, "UTF-8"))
          catch
            case NonFatal(e) =>
              throw ConfigError(s"cannot read $path: ${e.getMessage}")
        try ujson.read(raw).obj
        catch
          case NonFatal(e) =>
            throw ConfigError(
              s"$path is not a valid JSON object: ${e.getMessage}"
            )
    val provider =
      field(obj, path, "provider", "a string")(_.str).getOrElse("openrouter")
    if !knownProviders.contains(provider) then
      throw ConfigError(
        s"$path: 'provider' must be one of ${knownProviders.mkString(", ")}"
      )
    AgentConfig(
      workDir = workDir,
      provider = provider,
      model = field(obj, path, "model", "a string")(_.str)
        .getOrElse("minimax/minimax-m2.7"),
      maxTokens = field(obj, path, "max_tokens", "a positive integer")(
        positiveInt
      ).getOrElse(16000),
      thinking = deriveThinking(provider),
      classifiedPaths =
        field(obj, path, "classified_paths", "an array of strings")(
          _.arr.map(_.str).toList
        ).getOrElse(Nil)
    )

  private def positiveInt(v: ujson.Value): Int =
    v.num match
      case n if n.isValidInt && n > 0 => n.toInt
      case n => throw IllegalArgumentException(s"$n is not a positive integer")

  /** Read an optional field through `extract`, turning any type mismatch into a
    * [[ConfigError]] that names the file, the field and the expected shape.
    * Returns None when the key is absent so the caller can fall back to a
    * default.
    */
  private def field[A](
      obj: Map[String, ujson.Value],
      path: String,
      key: String,
      expected: String
  )(extract: ujson.Value => A): Option[A] =
    obj
      .get(key)
      .map: v =>
        try extract(v)
        catch
          case NonFatal(_) =>
            throw ConfigError(s"$path: '$key' must be $expected")

  private def deriveThinking(provider: String): Option[ThinkingMode] =
    provider match
      case "anthropic"                        => Some(ThinkingMode.Budget(2048))
      case "openai" | "openrouter" | "ollama" =>
        Some(ThinkingMode.Effort(EffortLevel.Medium))
      case _ => None
