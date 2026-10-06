package capybaraclaw.agent

private[agent] def codepointLength(s: String): Int =
  s.codePointCount(0, s.length)

enum MemoryFile(val target: String, val fileName: String, val capacity: Int):
  case Memory extends MemoryFile("memory", "MEMORY.md", 2200)
  case User extends MemoryFile("user", "USER.md", 1375)

object MemoryFile:
  def fromTarget(s: String): Option[MemoryFile] = s match
    case "memory" => Some(MemoryFile.Memory)
    case "user"   => Some(MemoryFile.User)
    case _        => None

final case class MemorySnapshot(memory: String, user: String):
  val memoryChars: Int = codepointLength(memory)
  val userChars: Int = codepointLength(user)
  def memoryPct: Int =
    MemorySnapshot.percent(memoryChars, MemoryFile.Memory.capacity)
  def userPct: Int =
    MemorySnapshot.percent(userChars, MemoryFile.User.capacity)

object MemorySnapshot:
  val empty: MemorySnapshot = MemorySnapshot("", "")

  private[agent] def percent(used: Int, cap: Int): Int =
    if cap <= 0 then 0
    else math.min(100, math.max(0, (used.toDouble / cap * 100).toInt))
