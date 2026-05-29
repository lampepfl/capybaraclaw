package capybaraclaw.agent.tools

import tacit.agents.llm.agentic.Agent
import tacit.agents.llm.utils.IsToolArg
import tacit.executor.ReplSession

object EvalScalaTool:
  case class Args(code: String) derives IsToolArg

  val name: String = "evaluate_scala"
  val description: String =
    "Evaluate a Scala expression in a persistent REPL session"

  def register(agent: Agent, repl: ReplSession): Unit =
    agent.handle[Args](name, description): (args, _) =>
      val result = repl.execute(args.code)
      if result.success then
        if result.output.nonEmpty then result.output
        else "(executed successfully, no output)"
      else
        val msg = StringBuilder("Execution failed.\n")
        if result.output.nonEmpty then
          msg.append(s"Output:\n${result.output}\n")
        result.error.foreach(e => msg.append(s"Error:\n$e\n"))
        msg.toString
