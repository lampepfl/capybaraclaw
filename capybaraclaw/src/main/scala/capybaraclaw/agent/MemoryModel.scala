package capybaraclaw.agent

private[agent] def codepointLength(s: String): Int =
  s.codePointCount(0, s.length)

/** One memory file kind: `target` is the name the agent uses in the `memory`
  * tool, `fileName` the file inside its scope's directory.
  */
final case class MemoryFile(target: String, fileName: String, capacity: Int)

object MemoryFile:
  /** The person's profile: preferences and communication style. */
  val User: MemoryFile = MemoryFile("user", "USER.md", 1375)

  /** The agent's private notes about work with one person. */
  val Private: MemoryFile = MemoryFile("private", "MEMORY.md", 2200)

  /** Notes shared by everyone in one channel. */
  val Channel: MemoryFile = MemoryFile("channel", "MEMORY.md", 2200)

  /** Notes visible in every conversation, curated by administrators. */
  val Public: MemoryFile = MemoryFile("public", "MEMORY.md", 2200)

/** A memory file one session can see. `about` tells the agent who sees it;
  * `readOnlyReason` is set when the session may not write it.
  */
final case class MemoryScope(
    file: MemoryFile,
    store: MemoryStore,
    about: String,
    readOnlyReason: Option[String] = None
):
  def writable: Boolean = readOnlyReason.isEmpty

/** The memory of one session, fixed when the session starts: `conversation`
  * describes it to the agent, e.g. "a private conversation with lukasz".
  */
final case class MemoryAccess(conversation: String, scopes: List[MemoryScope]):
  def scope(target: String): Option[MemoryScope] =
    scopes.find(_.file.target == target)

  def snapshot(): MemorySnapshot =
    MemorySnapshot(
      conversation,
      scopes.map: s =>
        MemorySection(
          s.file,
          s.about,
          s.writable,
          s.store.render(s.store.entries(s.file))
        )
    )

object MemoryAccess:
  /** No memory at all, e.g. for tests that do not exercise it. */
  val none: MemoryAccess = MemoryAccess("a conversation without memory", Nil)

final case class MemorySection(
    file: MemoryFile,
    about: String,
    writable: Boolean,
    content: String
):
  val chars: Int = codepointLength(content)
  def pct: Int = MemorySnapshot.percent(chars, file.capacity)

final case class MemorySnapshot(
    conversation: String,
    sections: List[MemorySection]
)

object MemorySnapshot:
  val empty: MemorySnapshot = MemorySnapshot("", Nil)

  private[agent] def percent(used: Int, cap: Int): Int =
    if cap <= 0 then 0
    else math.min(100, math.max(0, (used.toDouble / cap * 100).toInt))
