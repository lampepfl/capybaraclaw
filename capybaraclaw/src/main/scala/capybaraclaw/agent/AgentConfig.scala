package capybaraclaw.agent

import scala.io.Source

import tacit.agents.llm.endpoint.{EffortLevel, LLMConfig, ThinkingMode}

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
  /** Load `${workDir}/claw.json` if present; otherwise use defaults. The `thinking`
    * mode is derived from the provider unless explicitly set in the JSON.
    */
  def load(workDir: String): AgentConfig =
    val file = java.io.File(workDir, "claw.json")
    val obj =
      if file.exists() then ujson.read(readAll(Source.fromFile(file))).obj
      else ujson.Obj().value
    val provider = obj.get("provider").map(_.str).getOrElse("openrouter")
    AgentConfig(
      workDir = workDir,
      provider = provider,
      model = obj.get("model").map(_.str).getOrElse("minimax/minimax-m2.7"),
      maxTokens = obj.get("max_tokens").map(_.num.toInt).getOrElse(16000),
      thinking = deriveThinking(provider),
      classifiedPaths =
        obj.get("classified_paths").map(_.arr.map(_.str).toList).getOrElse(Nil)
    )

  private def deriveThinking(provider: String): Option[ThinkingMode] =
    provider match
      case "anthropic"                        => Some(ThinkingMode.Budget(2048))
      case "openai" | "openrouter" | "ollama" =>
        Some(ThinkingMode.Effort(EffortLevel.Medium))
      case _ => None
