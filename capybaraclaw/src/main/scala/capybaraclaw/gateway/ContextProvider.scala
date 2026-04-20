package capybaraclaw.gateway

import tacit.agents.llm.endpoint.{Message, Role, Content}
import java.io.{File, FileWriter, BufferedWriter}

/** Persistent transcript store, keyed by `ContextKey` so each (port, thread) has its
  * own conversation history. Used by the Gateway to seed a fresh `ClawAgent` on first
  * message and to append each new user/assistant message as the turn proceeds.
  */
trait ContextProvider:
  /** Load prior conversation for this key. Empty list for never-seen keys. */
  def load(key: ContextKey): List[Message]

  /** Append a single message to this key's transcript. */
  def append(key: ContextKey, msg: Message): Unit

/** JSONL-on-disk `ContextProvider`.
  *
  * Layout: `{baseDir}/.claw/history/{port}/{sanitize(thread)}.jsonl`. Each line is a
  * JSON object: `{"role": "user|assistant", "text": "..."}`.
  *
  * v1 limitation: only `Role.User` / `Role.Assistant` messages with pure `Content.Text`
  * are persisted. Tool-use and tool-result content is mid-turn scaffolding and is NOT
  * kept — the LLM reconstructs it as needed on future turns. Thinking content is also
  * dropped.
  */
class JsonlContextProvider(baseDir: File) extends ContextProvider:

  def load(key: ContextKey): List[Message] =
    val f = fileFor(key)
    if !f.exists() then return Nil
    val src = scala.io.Source.fromFile(f)
    try
      src.getLines().flatMap(decode).toList
    finally src.close()

  def append(key: ContextKey, msg: Message): Unit =
    encode(msg) match
      case None => () // not persistable (tool-use, tool-result, thinking-only, etc.)
      case Some(line) =>
        val f = fileFor(key)
        val parent = f.getParentFile
        if parent != null && !parent.exists() then parent.mkdirs()
        val bw = BufferedWriter(FileWriter(f, /*append=*/ true))
        try
          bw.write(line)
          bw.newLine()
        finally bw.close()

  private def fileFor(key: ContextKey): File =
    val historyRoot = File(File(baseDir, ".claw"), "history")
    File(historyRoot, s"${key.port}/${sanitize(key.thread)}.jsonl")

  private def sanitize(s: String): String =
    // Slack thread ids look like "C0123456/1234567890.123456"; preserve readability
    // but keep filenames safe on all platforms.
    s.map:
      case c if c.isLetterOrDigit => c
      case c @ ('.' | '-' | '_')  => c
      case _                      => '_'

  private def encode(m: Message): Option[String] =
    val roleStr = m.role match
      case Role.User      => "user"
      case Role.Assistant => "assistant"
      case Role.System    => return None // system seed comes from the agent config
    val text = m.content.collect { case Content.Text(t) => t }.mkString
    if text.isEmpty then None
    else
      Some(ujson.write(ujson.Obj("role" -> roleStr, "text" -> text)))

  private def decode(line: String): Option[Message] =
    if line.trim.isEmpty then return None
    val obj = ujson.read(line)
    val role = obj.obj.get("role").map(_.str).getOrElse("")
    val text = obj.obj.get("text").map(_.str).getOrElse("")
    role match
      case "user"      => Some(Message.user(text))
      case "assistant" => Some(Message.assistant(text))
      case _           => None
