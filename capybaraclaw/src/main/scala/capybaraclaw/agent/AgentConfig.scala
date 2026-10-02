package capybaraclaw.agent

import scala.collection.Map
import scala.io.Source
import scala.util.control.NonFatal

import tacit.agents.llm.endpoint.*

final class ConfigError(message: String) extends RuntimeException(message)

/** An LLM provider selectable via `provider` in `claw.json`. */
enum Provider(
    val id: String,
    val thinking: ThinkingMode,
    endpoint: EndpointProvider
):
  case Anthropic
      extends Provider(
        "anthropic",
        ThinkingMode.Budget(2048),
        AnthropicEndpoint
      )
  case OpenAI
      extends Provider(
        "openai",
        ThinkingMode.Effort(EffortLevel.Medium),
        OpenAIEndpoint
      )
  case OpenRouter
      extends Provider(
        "openrouter",
        ThinkingMode.Effort(EffortLevel.Medium),
        OpenRouterEndpoint
      )
  case Ollama
      extends Provider(
        "ollama",
        ThinkingMode.Effort(EffortLevel.Medium),
        OllamaEndpoint
      )

  def createEndpoint(): Endpoint = endpoint.createFromEnv()

object Provider:
  def fromId(id: String): Option[Provider] = values.find(_.id == id)

/** Configuration for a Claw agent instance.
  */
case class AgentConfig(
    workDir: String,
    provider: Provider = Provider.OpenRouter,
    model: String = "minimax/minimax-m2.7",
    maxTokens: Int = 16000,
    classifiedPaths: List[String] = Nil,
    memorySnapshot: MemorySnapshot = MemorySnapshot.empty
):
  def toLLMConfig: LLMConfig =
    LLMConfig(
      model = model,
      systemPrompt = Some(SystemPrompt.build(this)),
      maxTokens = Some(maxTokens),
      thinking = Some(provider.thinking)
    )

object AgentConfig:
  /** Load `${workDir}/claw.json` if present; otherwise use defaults. An unreadable or malformed
    * file, a wrong-typed field or an unknown provider raises [[ConfigError]]
    * with a message naming the file and field.
    */
  def load(
      workDir: String,
      memorySnapshot: MemorySnapshot = MemorySnapshot.empty
  ): AgentConfig =
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
      field(obj, path, "provider", "a string")(_.str)
        .map: id =>
          Provider
            .fromId(id)
            .getOrElse(
              throw ConfigError(
                s"$path: 'provider' must be one of ${Provider.values.map(_.id).mkString(", ")}"
              )
            )
        .getOrElse(Provider.OpenRouter)
    AgentConfig(
      workDir = workDir,
      provider = provider,
      model = field(obj, path, "model", "a string")(_.str)
        .getOrElse("minimax/minimax-m2.7"),
      maxTokens = field(obj, path, "max_tokens", "a positive integer")(
        positiveInt
      ).getOrElse(16000),
      classifiedPaths =
        field(obj, path, "classified_paths", "an array of strings")(
          _.arr.map(_.str).toList
        ).getOrElse(Nil),
      memorySnapshot = memorySnapshot
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
