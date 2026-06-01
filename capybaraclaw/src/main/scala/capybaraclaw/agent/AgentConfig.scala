package capybaraclaw.agent

import scala.collection.Map
import scala.io.Source
import scala.util.control.NonFatal

import tacit.agents.llm.endpoint.{EffortLevel, LLMConfig, ThinkingMode}

final class ConfigError(message: String) extends RuntimeException(message)

/** Private model used by safe-libs for confidential post-processing. This is
  * intentionally separate from the public agent model in `claw.json`.
  */
case class PrivateLlmConfig(
    provider: String = "ollama",
    baseUrl: String = "http://localhost:11434",
    model: String = "qwen3.5:latest"
)

/** Configuration for a Claw agent instance.
  */
case class AgentConfig(
    workDir: String,
    provider: String = "openrouter",
    model: String = "minimax/minimax-m2.7",
    maxTokens: Int = 16000,
    thinking: Option[ThinkingMode] = None,
    classifiedPaths: List[String] = Nil,
    memorySnapshot: MemorySnapshot = MemorySnapshot.empty,
    /** True when `${workDir}/plugins/` exists and contains at least one `*.jar`.
      * Derived at load time; informs only the system-prompt hint about which
      * classified-output function the agent should use. The authoritative
      * plugin list is read by TACIT via `pluginScanDirs`.
      */
    pluginsConfigured: Boolean = false,
    privateLlm: Option[PrivateLlmConfig] = None
):
  def toLLMConfig: LLMConfig =
    LLMConfig(
      model = model,
      systemPrompt = Some(SystemPrompt.build(this)),
      maxTokens = Some(maxTokens),
      thinking = thinking
    )

object AgentConfig:
  private val DefaultProvider = "openrouter"

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
      AgentConfig(
        workDir = workDir,
        provider = DefaultProvider,
        thinking = deriveThinking(DefaultProvider),
        memorySnapshot = memorySnapshot,
        pluginsConfigured = detectPluginsConfigured(workDir),
        privateLlm = loadPrivateLlm(workDir, Map.empty)
      )
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
        field(obj, "provider", "a string")(_.str).getOrElse(DefaultProvider)
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
        memorySnapshot = memorySnapshot,
        pluginsConfigured = detectPluginsConfigured(workDir),
        privateLlm = loadPrivateLlm(workDir, obj)
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

  /** True when `${workDir}/plugins/` is a directory containing at least one
    * `*.jar`. The actual plugin loading happens inside TACIT — capybaraclaw
    * only uses this flag to phrase the system prompt's classified-output hint.
    */
  private def detectPluginsConfigured(workDir: String): Boolean =
    val dir = java.io.File(workDir, "plugins")
    dir.isDirectory && Option(dir.list((_, n) => n.endsWith(".jar")))
      .exists(_.nonEmpty)

  private def loadPrivateLlm(
      workDir: String,
      obj: scala.collection.Map[String, ujson.Value]
  ): Option[PrivateLlmConfig] =
    val configName = obj
      .get("private_llm_config")
      .map(_.str)
      .getOrElse("claw.private.json")
    val file = java.io.File(workDir, configName)
    if !file.exists() then None
    else
      val privateObj = ujson.read(scala.io.Source.fromFile(file).mkString).obj
      Some(
        PrivateLlmConfig(
          provider = privateObj.get("provider").map(_.str).getOrElse("ollama"),
          baseUrl = privateObj
            .get("base_url")
            .orElse(privateObj.get("url"))
            .map(_.str)
            .getOrElse("http://localhost:11434"),
          model = privateObj.get("model").map(_.str).getOrElse("qwen3.5:latest")
        )
      )

  private def deriveThinking(provider: String): Option[ThinkingMode] =
    provider match
      case "anthropic"             => Some(ThinkingMode.Budget(2048))
      case "openai" | "openrouter" =>
        Some(ThinkingMode.Effort(EffortLevel.Medium))
      // Ollama's /v1/responses accepts `reasoning` but doesn't actually run
      // reasoning tokens — it just disturbs tool-call emission on non-thinking
      // models (qwen2.5 etc. start replying in prose instead of tool_calls), so
      // it falls through to None below.
      case _ => None
