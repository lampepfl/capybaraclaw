package capybaraclaw.gateway

import java.io.File
import java.nio.charset.StandardCharsets

import org.slf4j.LoggerFactory

import capybaraclaw.agent.{MemoryAccess, MemoryFile, MemoryScope, MemoryStore}

/** Decides which memory a session sees, from who started it and whether its
  * conversation is private. Layout under `root`:
  *
  *   - `public/MEMORY.md`: read everywhere, curated by administrators
  *   - `persons/<person>/{USER,MEMORY}.md`: only in that person's DMs, on any
  *     port linked to them in [[Identities]] (looked up when a session
  *     starts, so a changed link applies to new sessions)
  *   - `channels/<port>/<channel>/MEMORY.md`: only in that channel
  */
final class MemoryDirectory(
    val root: File,
    identities: () => Identities
):
  import MemoryDirectory.*

  private val logger = LoggerFactory.getLogger(classOf[MemoryDirectory])

  private val publicStore = MemoryStore(File(root, "public"))

  def person(port: PortId, user: UserId): Person =
    identities().person(port, user)

  def access(
      port: PortId,
      user: UserId,
      conversation: Conversation
  ): MemoryAccess =
    val public = MemoryScope(
      MemoryFile.Public,
      publicStore,
      "visible in every conversation, including shared channels",
      readOnlyReason = Some(PublicReadOnly)
    )
    conversation match
      case Conversation.Direct =>
        val p = person(port, user)
        val store = personStore(p)
        MemoryAccess(
          s"a private conversation with ${p.id}",
          List(
            MemoryScope(
              MemoryFile.User,
              store,
              s"profile of ${p.id}; only visible in private conversations with them"
            ),
            MemoryScope(
              MemoryFile.Private,
              store,
              s"your notes on work with ${p.id}; only visible in private conversations with them"
            ),
            public
          )
        )
      case Conversation.Group(channel) =>
        MemoryAccess(
          s"a shared conversation in $port channel $channel, where several people read everything",
          List(
            MemoryScope(
              MemoryFile.Channel,
              MemoryStore(
                File(root, s"channels/${segment(port)}/${segment(channel)}")
              ),
              s"notes for $port channel $channel; visible to everyone in it"
            ),
            public
          )
        )

  /** Memory written before it was scoped lived in `root` and could come from
    * any port; it moves into `owner`'s private memory, the closest match to
    * whose it was. Files already in place are left alone.
    */
  def migrateLegacy(owner: Person): Unit =
    val target = File(root, s"persons/${segment(owner.id)}")
    val legacy = Option(root.listFiles()).toList.flatten.filter: f =>
      f.isFile && LegacyNames.exists(n =>
        f.getName == n || f.getName.startsWith(s"$n.bak.")
      )
    legacy.foreach: f =>
      val dest = File(target, f.getName)
      if dest.exists() then
        logger.warn(
          s"not migrating ${f.getPath}: ${dest.getPath} already exists"
        )
      else
        target.mkdirs()
        java.nio.file.Files.move(f.toPath, dest.toPath)
        logger.info(s"migrated ${f.getPath} to ${dest.getPath}")

  private def personStore(p: Person): MemoryStore =
    MemoryStore(File(root, s"persons/${segment(p.id)}"))

object MemoryDirectory:
  val PublicReadOnly: String =
    "Public memory is curated by administrators and cannot be written with the memory tool."

  private val LegacyNames = List("MEMORY.md", "USER.md")

  /** `~/.claw/memories/`. */
  def default(identities: () => Identities): MemoryDirectory =
    MemoryDirectory(
      File(System.getProperty("user.home"), ".claw/memories"),
      identities
    )

  /** A single path segment for an id that comes from outside (a `$USER`, a
    * Slack id, a person's name): everything but `[A-Za-z0-9_-]` is
    * percent-encoded, so no id can contain `/` or be `.` or `..`, and distinct
    * ids never share a directory.
    */
  private[gateway] def segment(id: String): String =
    val sb = StringBuilder()
    id.getBytes(StandardCharsets.UTF_8)
      .foreach: b =>
        val c = (b & 0xff).toChar
        if c.isLetterOrDigit && c < 128 || c == '_' || c == '-' then sb += c
        else sb ++= f"%%${b & 0xff}%02X"
    sb.toString
