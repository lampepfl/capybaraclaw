package capybaraclaw.agent

enum MemoryResult:
  /** A successful mutation. */
  case Updated(
      target: String,
      entries: List[String],
      usage: String,
      entryCount: Int,
      message: Option[String]
  )

  /** Plain validation or lookup failure (empty input, no match, bad action). */
  case Rejected(error: String)

  /** Mutation would exceed the file cap; `current` is what the model can evict. */
  case CapExceeded(error: String, current: List[String], usage: String)

  /** On-disk content does not round-trip; a backup snapshot was saved. */
  case Drift(error: String, backup: String, remediation: String)

  /** Read-only view of a file: raw bytes, parsed entries, drift flag, and the
    * (name, content) of every backup on disk — enough to drive drift recovery.
    */
  case Inspected(
      target: String,
      raw: String,
      entries: List[String],
      drift: Boolean,
      backups: List[(String, String)]
  )

  /** old_text matched more than one entry; `matches` are the (truncated) hits. */
  case Ambiguous(error: String, matches: List[String])

  /** Filesystem I/O failed during a mutation. */
  case IoFailure(error: String, target: String)

object MemoryResult:
  def toJson(r: MemoryResult): ujson.Obj = r match
    case Updated(target, entries, usage, entryCount, message) =>
      val obj = ujson.Obj(
        "success" -> true,
        "target" -> target,
        "entries" -> ujson.Arr.from(entries),
        "usage" -> usage,
        "entry_count" -> entryCount
      )
      message.foreach(m => obj("message") = m)
      obj
    case Rejected(error) =>
      ujson.Obj("success" -> false, "error" -> error)
    case CapExceeded(error, current, usage) =>
      ujson.Obj(
        "success" -> false,
        "error" -> error,
        "current_entries" -> ujson.Arr.from(current),
        "usage" -> usage
      )
    case Drift(error, backup, remediation) =>
      ujson.Obj(
        "success" -> false,
        "error" -> error,
        "drift_backup" -> backup,
        "remediation" -> remediation
      )
    case Inspected(target, raw, entries, drift, backups) =>
      ujson.Obj(
        "success" -> true,
        "target" -> target,
        "raw" -> raw,
        "entries" -> ujson.Arr.from(entries),
        "drift" -> drift,
        "backups" -> ujson.Arr.from(
          backups.map((name, content) =>
            ujson.Obj("name" -> name, "content" -> content)
          )
        )
      )
    case Ambiguous(error, matches) =>
      ujson.Obj(
        "success" -> false,
        "error" -> error,
        "matches" -> ujson.Arr.from(matches)
      )
    case IoFailure(error, target) =>
      ujson.Obj("success" -> false, "error" -> error, "target" -> target)
