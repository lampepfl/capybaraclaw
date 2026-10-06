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
    classifiedPaths: List[String] = Nil
):
  def toLLMConfig: LLMConfig =
    LLMConfig(
      model = model,
      systemPrompt = Some(SystemPrompt.build(this)),
      maxTokens = Some(maxTokens),
      thinking = Some(provider.thinking)
    )

object AgentConfig:
  private val KnownKeys =
    List("provider", "model", "max_tokens", "classified_paths")

  /** Load `${workDir}/claw.json` if present; otherwise use defaults. An unreadable or malformed
    * file, an unknown key, a wrong-typed field or an unknown provider raises
    * [[ConfigError]] with a message naming the file and field.
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
    rejectUnknownKeys(obj, path)
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
      model = field(obj, path, "model", "a non-empty string")(nonBlankString)
        .getOrElse("minimax/minimax-m2.7"),
      maxTokens = field(obj, path, "max_tokens", "a positive integer")(
        positiveInt
      ).getOrElse(16000),
      classifiedPaths =
        field(obj, path, "classified_paths", "an array of non-empty strings")(
          _.arr.map(nonBlankString).toList
        ).getOrElse(Nil)
    )

  /** Fail on keys outside [[KnownKeys]]: a typo such as `classifed_paths` would
    * otherwise silently drop that setting. Suggests the closest known key.
    */
  private def rejectUnknownKeys(
      obj: Map[String, ujson.Value],
      path: String
  ): Unit =
    obj.keys.toList.sorted
      .find(!KnownKeys.contains(_))
      .foreach: key =>
        val closest = KnownKeys.minBy(editDistance(key, _))
        val hint =
          if editDistance(key, closest) <= 2 then s"did you mean '$closest'?"
          else s"known keys: ${KnownKeys.mkString(", ")}"
        throw ConfigError(s"$path: unknown key '$key', $hint")

  private def editDistance(a: String, b: String): Int =
    var prev = Array.range(0, b.length + 1)
    for i <- 1 to a.length do
      val curr = Array.ofDim[Int](b.length + 1)
      curr(0) = i
      for j <- 1 to b.length do
        val cost = if a(i - 1) == b(j - 1) then 0 else 1
        curr(j) =
          math.min(math.min(curr(j - 1), prev(j)) + 1, prev(j - 1) + cost)
      prev = curr
    prev(b.length)

  private def nonBlankString(v: ujson.Value): String =
    v.str match
      case s if s.isBlank => throw IllegalArgumentException("blank string")
      case s              => s

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
