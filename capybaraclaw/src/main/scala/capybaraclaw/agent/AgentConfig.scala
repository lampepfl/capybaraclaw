package capybaraclaw.agent

import tacit.agents.llm.endpoint.{EffortLevel, LLMConfig, ThinkingMode}

/** Configuration for a Claw agent instance.
  */
case class AgentConfig(
  workDir: String,
  provider: String = "openrouter",
  model: String = "minimax/minimax-m2.7",
  maxTokens: Int = 16000,
  thinking: Option[ThinkingMode] = None,
  classifiedPaths: List[String] = Nil,
):
  def toLLMConfig: LLMConfig =
    LLMConfig(
      model = model,
      systemPrompt = Some(AgentConfig.buildSystemPrompt(this)),
      maxTokens = Some(maxTokens),
      thinking = thinking,
    )

object AgentConfig:
  /** Load `${workDir}/claw.json` if present; otherwise use defaults. The `thinking`
    * mode is derived from the provider unless explicitly set in the JSON.
    */
  def load(workDir: String): AgentConfig =
    val file = java.io.File(workDir, "claw.json")
    val obj =
      if file.exists() then ujson.read(scala.io.Source.fromFile(file).mkString).obj
      else ujson.Obj().value
    val provider = obj.get("provider").map(_.str).getOrElse("openrouter")
    AgentConfig(
      workDir = workDir,
      provider = provider,
      model = obj.get("model").map(_.str).getOrElse("minimax/minimax-m2.7"),
      maxTokens = obj.get("max_tokens").map(_.num.toInt).getOrElse(16000),
      thinking = deriveThinking(provider),
      classifiedPaths = obj.get("classified_paths").map(_.arr.map(_.str).toList).getOrElse(Nil),
    )

  private def deriveThinking(provider: String): Option[ThinkingMode] = provider match
    case "anthropic"                        => Some(ThinkingMode.Budget(2048))
    case "openai" | "openrouter" | "ollama" => Some(ThinkingMode.Effort(EffortLevel.Medium))
    case _                                  => None

  private def loadInterfaceSource(): String =
    val stream = classOf[AgentConfig].getClassLoader.getResourceAsStream("Interface.scala")
    if stream != null then
      try scala.io.Source.fromInputStream(stream).mkString
      finally stream.close()
    else "(Interface.scala not found on classpath)"

  private def loadClawMd(workDir: String): Option[String] =
    val file = java.io.File(workDir, "CLAW.md")
    if file.exists() then Some(scala.io.Source.fromFile(file).mkString)
    else None

  private def buildSystemPrompt(config: AgentConfig): String =
    val interfaceSource = loadInterfaceSource()
    val clawMd = loadClawMd(config.workDir)

    val sb = StringBuilder()

    sb.append(s"""<role>
You are a helpful assistant with access to a Scala 3 REPL.
You can evaluate Scala code using the evaluate_scala tool. The REPL session is persistent: definitions and values carry across calls.
</role>

<environment>
Working directory: ${config.workDir}
File system access is restricted to this directory. When using requestFileSystem, always use this path as the root.
</environment>

<config>
$config
</config>

<library_api>
The REPL has the following library API pre-loaded (all functions available at top level):

```scala
$interfaceSource
```
</library_api>""")

    if config.classifiedPaths.nonEmpty then
      sb.append(s"""

<classified_paths>
The following paths should be classified:
${config.classifiedPaths.map(p => s"- $p").mkString("\n")}
</classified_paths>""")

    clawMd.foreach: md =>
      sb.append(s"""

<project_instructions>
$md
</project_instructions>""")

    sb.toString
