package capybaraclaw.agent.tools

import tacit.agents.llm.agentic.Agent
import tacit.agents.llm.utils.IsToolArg

import capybaraclaw.agent.{MemoryFile, MemoryResult, MemoryStore}

object MemoryTool:
  case class Args(
      action: String,
      target: String,
      content: Option[String] = None,
      old_text: Option[String] = None
  ) derives IsToolArg

  val name: String = "memory"
  val description: String =
    "Save durable information to persistent memory that survives across sessions. " +
      "Keep it compact and focused on facts that still matter later. " +
      "Use target='user' for preferences and communication style; target='memory' " +
      "for environment facts, project conventions, tool quirks, and lessons learned. " +
      "Use action='add' with content for a new entry; action='replace' with old_text " +
      "and content to replace an entry; action='remove' with old_text to delete an entry. " +
      "old_text is a short unique substring identifying a complete entry. " +
      "If a write is refused for drift, use action='read' to view the current contents " +
      "and any backups, then action='reconcile' with content set to the full corrected, " +
      "§-separated list to repair the file in one step. " +
      "Save right away when the user states a preference, corrects you, or you learn a " +
      "durable fact - don't wait to be asked. " +
      "Write entries as declarative facts, not instructions to yourself: " +
      "'User prefers concise responses' ✓ — 'Always respond concisely' ✗. " +
      "Skip anything that will be stale within a week: task progress, completed-work logs, " +
      "PR or issue numbers, commit SHAs, 'Phase N done', file counts, raw dumps, and " +
      "easily rediscovered facts."

  def register(agent: Agent, store: MemoryStore): Unit =
    agent.handle[Args](name, description): (args, _) =>
      run(store, args)

  /** Dispatch a tool call to the matching [[MemoryStore]] operation and render
    * its result as the JSON string returned to the model.
    *
    * @param store
    *   the memory store to act on.
    * @param args
    *   the parsed tool arguments (`action`, `target`, and the `content` /
    *   `old_text` the chosen action needs).
    * @return
    *   the [[MemoryResult]] serialized to JSON; on invalid input a JSON object
    *   with `success: false` and an `error` message.
    */
  private[agent] def run(store: MemoryStore, args: Args): String =
    def required(value: Option[String], field: String): Either[String, String] =
      value
        .filter(_.nonEmpty)
        .toRight(s"$field is required for '${args.action}' action.")

    val outcome: Either[String, MemoryResult] =
      for
        target <- MemoryFile
          .fromTarget(args.target)
          .toRight(s"Invalid target '${args.target}'. Use 'memory' or 'user'.")
        result <- args.action match
          case "add" =>
            required(args.content, "Content").map(store.add(target, _))
          case "replace" =>
            for
              old <- required(args.old_text, "old_text")
              content <- required(args.content, "Content")
            yield store.replace(target, old, content)
          case "remove" =>
            required(args.old_text, "old_text").map(store.remove(target, _))
          case "read" =>
            Right(store.inspect(target))
          case "reconcile" =>
            args.content
              .toRight(
                "content is required for 'reconcile' (use \"\" to clear)."
              )
              .map(store.reconcile(target, _))
          case other =>
            Left(
              s"Unknown action '$other'. Use: add, replace, remove, read, reconcile"
            )
      yield result

    val result = outcome.fold(MemoryResult.Rejected(_), identity)
    MemoryResult.toJson(result).render()
