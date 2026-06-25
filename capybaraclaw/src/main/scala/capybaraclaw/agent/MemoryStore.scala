package capybaraclaw.agent

import java.io.File
import java.nio.file.{
  AtomicMoveNotSupportedException,
  FileAlreadyExistsException,
  Path
}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.HexFormat

import scala.util.control.NonFatal

import org.slf4j.LoggerFactory

import capybaraclaw.util.FileMutex

final class MemoryStore(val baseDir: File):
  import MemoryStore.EntryDelimiter

  private val logger = LoggerFactory.getLogger(classOf[MemoryStore])

  private val base: os.Path = os.Path(baseDir.getAbsoluteFile.nn)

  /** @param f
    *   which memory file (MEMORY.md / USER.md) to read.
    * @return
    *   the raw on-disk content, or `""` if the file is absent or unreadable.
    */
  def read(f: MemoryFile): String =
    try readRaw(f)
    catch
      case NonFatal(e) =>
        logger.warn(
          s"Failed to read ${f.fileName}; treating memory as empty",
          e
        )
        ""

  /** @param f
    *   which memory file to read.
    * @return
    *   the file's entries, split on `§` boundaries and deduplicated.
    */
  def entries(f: MemoryFile): List[String] =
    parseEntries(read(f)).distinct

  /** @return
    *   a [[MemorySnapshot]] of both memory files (normalized and deduplicated),
    *   used to render the session-start system-prompt block.
    */
  def snapshot(): MemorySnapshot =
    MemorySnapshot(
      renderEntries(entries(MemoryFile.Memory)),
      renderEntries(entries(MemoryFile.User))
    )

  /** Append `content` as a new entry.
    *
    * @param f
    *   target memory file.
    * @param content
    *   the entry text; must be non-empty and contain no line that is just `§`.
    * @return
    *   a [[MemoryResult]] describing the outcome.
    */
  def add(f: MemoryFile, content: String): MemoryResult =
    val entry = content.trim
    if entry.isEmpty then failure("Content cannot be empty.")
    else if containsDelimiterLine(entry) then
      failure(
        """Entry contains a reserved separator line ('§' alone on a line).
          |Rephrase so no line is just '§'.""".stripMargin
      )
    else
      mutate(f): current =>
        if current.contains(entry) then
          Left(
            success(f, current, "Entry already exists (no duplicate added).")
          )
        else
          val updated = current :+ entry
          val used = charCount(current)
          val projected = charCount(updated)
          if projected > f.capacity then
            Left(
              capExceeded(
                f,
                current,
                s"""Memory at $used/${f.capacity} chars.
                   |Adding this entry (+${projected - used} chars)
                   |would exceed the limit ($projected/${f.capacity}).
                   |Replace or remove existing entries first.""".stripMargin
              )
            )
          else Right((updated, success(f, updated, "Entry added.")))

  /** Replace the single entry identified by `oldText` with `newContent`.
    *
    * @param f
    *   target memory file.
    * @param oldText
    *   a substring uniquely identifying one existing entry.
    * @param newContent
    *   the replacement text; non-empty, with no lone `§` line.
    * @return
    *   a [[MemoryResult]] describing the outcome.
    */
  def replace(
      f: MemoryFile,
      oldText: String,
      newContent: String
  ): MemoryResult =
    val old = oldText.trim
    val content = newContent.trim
    if old.isEmpty then failure("old_text cannot be empty.")
    else if content.isEmpty then
      failure("Content cannot be empty. Use 'remove' to delete entries.")
    else if containsDelimiterLine(content) then
      failure(
        """Replacement contains a reserved separator line ('§' alone on a line).
          |Rephrase so no line is just '§'.""".stripMargin
      )
    else
      mutate(f): current =>
        matchingEntries(current, old) match
          case Left(result) => Left(result)
          case Right(index) =>
            val updated = current.updated(index, content).distinct
            val merged = updated.length < current.length
            val message =
              if merged then
                "Entry replaced (merged into an existing identical entry)."
              else "Entry replaced."
            val projected = charCount(updated)
            if projected > f.capacity then
              Left(
                capExceeded(
                  f,
                  current,
                  s"""Replacement would put memory at
                     |$projected/${f.capacity} chars.
                     |Shorten the new content or remove other entries first.""".stripMargin
                )
              )
            else Right((updated, success(f, updated, message)))

  /** Delete the single entry identified by `oldText`.
    *
    * @param f
    *   target memory file.
    * @param oldText
    *   a substring uniquely identifying one existing entry.
    * @return
    *   a [[MemoryResult]] describing the outcome.
    */
  def remove(f: MemoryFile, oldText: String): MemoryResult =
    val old = oldText.trim
    if old.isEmpty then failure("old_text cannot be empty.")
    else
      mutate(f): current =>
        matchingEntries(current, old) match
          case Left(result) => Left(result)
          case Right(index) =>
            val updated = current.patch(index, Nil, 1)
            Right((updated, success(f, updated, "Entry removed.")))

  /** Read-only view for drift recovery: the raw bytes, parsed entries, whether
    * the file drifts, and every backup's (name, content).
    *
    * @param f
    *   which memory file (MEMORY.md / USER.md) to inspect.
    * @return
    *   a [[MemoryResult]] describing the outcome.
    */
  def inspect(f: MemoryFile): MemoryResult =
    try
      withLock(f):
        val raw = readRaw(f)
        val drift = detectsDrift(raw)
        val backups =
          if drift then
            writeDriftBackup(f, raw)
            listBackups(f)
          else listBackups(f)
        MemoryResult.Inspected(
          f.target,
          raw,
          parseEntries(raw).distinct,
          drift,
          backups
        )
    catch
      case NonFatal(e) =>
        logger.warn(s"Memory inspect on ${f.fileName} failed", e)
        MemoryResult.IoFailure(
          s"Failed to read ${f.fileName}: ${Option(e.getMessage)
              .getOrElse(e.getClass.getName)}",
          f.target
        )

  /** Repair a drifted file by overwriting it with `content` as a clean,
    * §-delimited list. Allowed ONLY while the file drifts (otherwise add/replace/
    * remove are the right tools).
    *
    * @param f
    *   which memory file (MEMORY.md / USER.md) to repair.
    * @param content
    *   the full corrected memory as a `§`-separated list. An empty string clears
    *   the file; the backup retains the old content.
    * @return
    *   a [[MemoryResult]] describing the outcome.
    */
  def reconcile(f: MemoryFile, content: String): MemoryResult =
    try
      withLock(f):
        if !detectsDrift(readRaw(f)) then
          failure(
            s"""No drift to resolve on ${f.fileName}; the file already round-trips.
               |Use add/replace/remove.""".stripMargin
          )
        else
          val entries = parseEntries(content).distinct
          val projected = charCount(entries)
          if projected > f.capacity then
            capExceeded(
              f,
              entries,
              s"""Reconciled content would put memory at $projected/${f.capacity} chars.
                 |Drop or shorten entries first.""".stripMargin
            )
          else
            writeAtomic(f, renderEntries(entries))
            cleanupSubsumedBackups(f, entries)
            success(f, entries, "Reconciled.")
    catch
      case NonFatal(e) =>
        logger.warn(s"Memory reconcile on ${f.fileName} failed", e)
        MemoryResult.IoFailure(
          s"Failed to reconcile ${f.fileName}: ${Option(e.getMessage)
              .getOrElse(e.getClass.getName)}",
          f.target
        )

  /** Run a read-modify-write under the file lock: load+validate, apply `update`,
    * then atomically persist and garbage-collect reconciled backups.
    *
    * @param f
    *   the memory file to mutate.
    * @param update
    *   given the current entries, returns either `Left(result)` to abort with
    *   that [[MemoryResult]] (no write), or `Right((updated, result))` to persist
    *   `updated` and return `result`.
    * @return
    *   the `update`'s result on success; [[MemoryResult.Drift]] if the file does
    *   not round-trip; or [[MemoryResult.IoFailure]] on an I/O error.
    */
  private def mutate(
      f: MemoryFile
  )(
      update: List[String] => Either[MemoryResult, (List[String], MemoryResult)]
  ): MemoryResult =
    try
      withLock(f):
        loadValidated(f) match
          case Left(backup) =>
            MemoryResult.Drift(
              s"""Refusing to write ${f.fileName}: the on-disk content would not
                 |round-trip through the memory tool (likely a manual edit or a
                 |concurrent session).
                 |A snapshot was saved to $backup. Resolve the drift first, then retry.""".stripMargin,
              backup,
              """Use action='read' to view the backup and current contents, then
                |action='reconcile' with the full corrected, §-separated content
                |to repair the file in one step.""".stripMargin
            )
          case Right(current) =>
            update(current) match
              case Left(result)             => result
              case Right((updated, result)) =>
                writeAtomic(f, renderEntries(updated))
                cleanupSubsumedBackups(f, updated)
                result
    catch
      case NonFatal(e) =>
        logger.warn(s"Memory operation on ${f.fileName} failed", e)
        MemoryResult.IoFailure(
          s"Failed to update ${f.fileName}: ${Option(e.getMessage)
              .getOrElse(e.getClass.getName)}",
          f.target
        )

  private def loadValidated(f: MemoryFile): Either[String, List[String]] =
    val raw = readRaw(f)
    if raw.trim.isEmpty then Right(Nil)
    else if detectsDrift(raw) then Left(writeDriftBackup(f, raw))
    else Right(parseEntries(raw).distinct)

  private def matchingEntries(
      current: List[String],
      oldText: String
  ): Either[MemoryResult, Int] =
    val matches =
      current.zipWithIndex.filter((entry, _) => entry.contains(oldText))
    matches match
      case Nil =>
        Left(failure(s"No entry matched '$oldText'."))
      case (_, index) :: Nil =>
        Right(index)
      case many =>
        Left(
          MemoryResult.Ambiguous(
            s"Multiple entries matched '$oldText'. Be more specific.",
            many.map: (entry, _) =>
              if entry.length <= 80 then entry else s"${entry.take(80)}..."
          )
        )

  private def detectsDrift(raw: String): Boolean =
    raw.trim.nonEmpty &&
      raw.trim != renderEntries(parseEntries(raw).distinct)

  private def writeDriftBackup(f: MemoryFile, raw: String): String =
    val hash = HexFormat
      .of()
      .formatHex(
        MessageDigest
          .getInstance("SHA-256")
          .digest(raw.getBytes(StandardCharsets.UTF_8))
      )
    val backup = base / s"${f.fileName}.bak.$hash"
    try os.write(backup, raw)
    catch case _: FileAlreadyExistsException => ()
    backup.toString

  private def listBackups(f: MemoryFile): List[(String, String)] =
    val prefix = s"${f.fileName}.bak."
    os.list(base)
      .filter(_.last.startsWith(prefix))
      .flatMap: backup =>
        try Some(backup.last -> os.read(backup))
        catch
          case NonFatal(e) =>
            logger.debug(s"Could not read backup ${backup.last}", e); None
      .toList

  private def cleanupSubsumedBackups(
      f: MemoryFile,
      current: List[String]
  ): Unit =
    val currentSet = current.toSet
    val prefix = s"${f.fileName}.bak."
    try
      os.list(base)
        .filter(_.last.startsWith(prefix))
        .foreach: backup =>
          try
            val backupEntries = parseEntries(os.read(backup)).distinct
            if backupEntries.nonEmpty && backupEntries.forall(
                currentSet.contains
              )
            then os.remove(backup, checkExists = false)
          catch
            case NonFatal(e) =>
              logger.debug(s"Could not evaluate backup ${backup.last}", e)
    catch case NonFatal(e) => logger.debug("Backup cleanup failed", e)

  private def readRaw(f: MemoryFile): String =
    val path = targetPath(f)
    if os.exists(path) then os.read(path) else ""

  private def writeAtomic(f: MemoryFile, content: String): Unit =
    val tmp = os.temp(content, dir = base, prefix = ".mem_", suffix = ".tmp")
    try os.move(tmp, targetPath(f), replaceExisting = true, atomicMove = true)
    catch
      case _: AtomicMoveNotSupportedException =>
        logger.warn(
          "Atomic move is not supported for {}; falling back to non-atomic replacement",
          targetPath(f).toString
        )
        os.move(tmp, targetPath(f), replaceExisting = true)
    finally os.remove(tmp, checkExists = false)

  private def withLock[A](f: MemoryFile)(body: => A): A =
    os.makeDir.all(base)
    FileMutex.withLock(lockPath(f))(body)

  private def targetPath(f: MemoryFile): os.Path = base / f.fileName

  private def lockPath(f: MemoryFile): Path =
    (base / s"${f.fileName}.lock").toNIO

  private def success(
      f: MemoryFile,
      current: List[String],
      message: String = ""
  ): MemoryResult =
    val used = charCount(current)
    val pct = MemorySnapshot.percent(used, f.capacity)
    MemoryResult.Updated(
      f.target,
      current,
      s"$pct% — $used/${f.capacity} chars",
      current.length,
      Option.when(message.nonEmpty)(message)
    )

  private def failure(message: String): MemoryResult =
    MemoryResult.Rejected(message)

  private def capExceeded(
      f: MemoryFile,
      current: List[String],
      detail: String
  ): MemoryResult =
    MemoryResult.CapExceeded(
      detail,
      current,
      s"${charCount(current)}/${f.capacity}"
    )

  private def containsDelimiterLine(s: String): Boolean =
    s.linesIterator.exists(_.trim == EntryDelimiter.trim)

  private def parseEntries(raw: String): List[String] =
    raw
      .split(java.util.regex.Pattern.quote(EntryDelimiter), -1)
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)

  private def renderEntries(current: List[String]): String =
    current.mkString(EntryDelimiter)

  private def charCount(current: List[String]): Int =
    codepointLength(renderEntries(current))

object MemoryStore:
  private val EntryDelimiter = "\n§\n"

  /** Default location: `~/.claw/memories/`. */
  def default(): MemoryStore =
    val home = System.getProperty("user.home")
    MemoryStore(File(home, ".claw/memories"))
