package capybaraclaw.agent.tools

import tacit.agents.llm.agentic.Agent
import tacit.agents.llm.utils.IsToolArg
import tacit.executor.ReplSession

import capybaraclaw.gateway.SessionId

import org.slf4j.LoggerFactory

object EvalScalaTool:
  case class Args(code: String) derives IsToolArg

  private val logger = LoggerFactory.getLogger(getClass)

  val name: String = "evaluate_scala"
  val description: String =
    "Evaluate a Scala expression in a persistent REPL session"

  /** Appended while the REPL refuses code because a timed-out evaluation
    * keeps running. The agent cannot start another REPL; only the user can,
    * by starting a new conversation.
    */
  val BlockedGuidance: String =
    "Code from an earlier call timed out and keeps running, so this conversation cannot run code until it ends. " +
      "Tell the user. A new conversation gets a fresh REPL: a new Slack thread or top-level message, " +
      "or capybara restarted without --resume on the CLI."

  def register(agent: Agent, repl: ReplSession, sessionId: SessionId): Unit =
    agent.handle[Args](name, description): (args, _) =>
      evaluate(repl, sessionId, args.code)

  def evaluate(repl: ReplSession, sessionId: SessionId, code: String): String =
    val result = repl.execute(code)
    if result.success then
      if result.output.nonEmpty then result.output
      else "(executed successfully, no output)"
    else
      val msg = StringBuilder("Execution failed.\n")
      if result.output.nonEmpty then msg.append(s"Output:\n${result.output}\n")
      result.error.foreach(e => msg.append(s"Error:\n$e\n"))
      if repl.blocked then
        logger.warn(
          s"session $sessionId: a timed-out evaluation keeps running; its REPL refuses code until it ends"
        )
        msg.append(BlockedGuidance).append('\n')
      msg.toString
