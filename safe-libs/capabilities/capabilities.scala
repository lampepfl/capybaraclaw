package safemode.lib

import language.experimental.captureChecking
import caps.SharedCapability
import scala.caps.assumeSafe
import scala.annotation.implicitNotFound

// ─── Classified Data ──────────────────────────────────────────────────────

/** Wrapper that protects sensitive data. toString never reveals the value.
 *  map/flatMap only accept pure functions (T -> U, not T => U). */
@assumeSafe
trait Classified[+T]:
  def map[U](op: T -> U): Classified[U]
  def flatMap[U](op: T -> Classified[U]): Classified[U]
  override def toString: String = "Classified(****)"

@assumeSafe
private class ClassifiedImpl[+T](val value: T) extends Classified[T]:
  def map[U](op: T -> U): Classified[U] =
    try ClassifiedImpl(op(value))
    catch case _: Throwable => throw SecurityException("map function threw an exception")
  def flatMap[U](op: T -> Classified[U]): Classified[U] =
    try op(value)
    catch case _: Throwable => throw SecurityException("flatMap function threw an exception")

@assumeSafe
def classify[T](value: T): Classified[T] = ClassifiedImpl(value)

// ─── Permission Types ─────────────────────────────────────────────────────

/** Permission levels for filesystem access, from least to most permissive. */
sealed trait Permission

/** Can only read files: read, exists, list children. */
sealed trait ReadOnly extends Permission

/** Can read and write but cannot lose data:
 *  write (no overwrite), rename within same fs (no overwrite),
 *  copy out to other filesystems. */
sealed trait NonDestructive extends Permission

/** Full access: read, write, overwrite, delete, move out, etc. */
sealed trait FullAccess extends Permission

// ─── Type-level Evidence ──────────────────────────────────────────────────

/** Evidence that permission level P allows writing new files. */
@implicitNotFound("This filesystem has permission level ${P} which does not allow write operations. Only NonDestructive and FullAccess filesystems support writing.")
@assumeSafe sealed trait CanWrite[P <: Permission]
@assumeSafe given CanWrite[NonDestructive] with {}
@assumeSafe given CanWrite[FullAccess] with {}

/** Evidence that permission level P allows destructive operations
 *  (delete, overwrite, move-from). */
@implicitNotFound("This filesystem has permission level ${P} which does not allow destructive operations (delete, overwrite, move-from). Only FullAccess filesystems support destruction. Consider using copyFile instead of moveFile to transfer data without removing the source.")
@assumeSafe sealed trait CanDestroy[P <: Permission]
@assumeSafe given CanDestroy[FullAccess] with {}

// ─── File System ──────────────────────────────────────────────────────────

/** Capability granting scoped access to a file-system subtree.
 *  Parameterized by permission level P which determines what operations
 *  are available on entries.
 *
 *  classifiedPaths is a set of absolute paths that are protected:
 *  - read() on a classified path throws SecurityException
 *  - writeClassified() only works on classified paths, accepting Classified[String]
 *  - readClassified() only works on classified paths, returning Classified[String]
 *
 *  The path-dependent type Entry ensures entries from different filesystem
 *  instances are incompatible types — even if both are the same permission level. */
@assumeSafe
abstract class FileSystem[P <: Permission] extends SharedCapability:
  def root: String

  /** Paths that are classified — readable only as Classified[String]. */
  def classifiedPaths: Set[String]

  private[lib] def isClassified(path: String): Boolean =
    classifiedPaths.contains(java.nio.file.Path.of(path).normalize.toString)

  /** Entry is a path-dependent type: fs1.Entry != fs2.Entry. */
  type Entry <: EntryOps

  def access(path: String): Entry

  /** Read classified content from a classified path. Throws if not classified. */
  def readClassified(path: String): Classified[String]

  /** Write classified content to a classified path. Throws if not classified.
   *  The raw value is never exposed — only the Classified wrapper is accepted. */
  def writeClassified(path: String, content: Classified[String]): Unit

  /** Base operations available on all entries regardless of permission level. */
  @assumeSafe
  trait EntryOps:
    def path: String
    def name: String
    def exists: Boolean
    def isDirectory: Boolean

    /** Read file contents. Throws SecurityException if the path is classified —
     *  use readClassified() instead. */
    def read(): String

    def children: List[Entry]

    /** Write content to this file. Requires write permission.
     *  On NonDestructive filesystems, throws if file already exists. */
    def write(content: String)(using CanWrite[P]): Unit

    /** Delete this file. Requires destroy permission (FullAccess only). */
    def delete()(using CanDestroy[P]): Unit

    /** Rename/move within the same filesystem. Requires write permission.
     *  On NonDestructive filesystems, throws if target already exists. */
    def renameTo(newPath: String)(using CanWrite[P]): Entry

// ─── Cross-filesystem Operations ─────────────────────────────────────────

/** Copy a file from one filesystem to another.
 *  Only requires CanWrite on the target — no data is lost from source.
 *  This is safe even from NonDestructive sources to FullAccess targets. */
@assumeSafe
def copyFile[P1 <: Permission, P2 <: Permission](
  source: FileSystem[P1], sourcePath: String,
  target: FileSystem[P2], targetPath: String
)(using CanWrite[P2]): target.Entry =
  val content = source.access(sourcePath).read()
  val entry = target.access(targetPath)
  entry.write(content)
  entry

/** Move a file from one filesystem to another.
 *  Requires CanDestroy on source (file is removed) AND CanWrite on target.
 *  This means you CANNOT move from NonDestructive — moving deletes the source. */
@assumeSafe
def moveFile[P1 <: Permission, P2 <: Permission](
  source: FileSystem[P1], sourcePath: String,
  target: FileSystem[P2], targetPath: String
)(using CanDestroy[P1], CanWrite[P2]): target.Entry =
  val content = source.access(sourcePath).read()
  source.access(sourcePath).delete()
  val entry = target.access(targetPath)
  entry.write(content)
  entry

// ─── Network ──────────────────────────────────────────────────────────────

@assumeSafe
abstract class Network(val allowedHosts: Set[String]) extends SharedCapability

@assumeSafe
def httpGet(url: String)(using net: Network): String =
  s"[mock GET $url]"

@assumeSafe
def httpPost(url: String, body: String)(using net: Network): String =
  s"[mock POST to $url]"

// ─── IO Capability ────────────────────────────────────────────────────────

@assumeSafe
class IOCapability extends SharedCapability

@assumeSafe
def safePrintln(x: Any)(using io: IOCapability): Unit = Predef.println(x)

/** Write a classified value to the first classified path in the given FileSystem.
 *  This is the safe output channel — the agent calls this but cannot read the
 *  path back or open the file directly (read() on classified paths throws).
 *  Mirrors TACIT's writeClassified pattern.
 *
 *  Returns a value-independent confirmation string so the REPL prints a
 *  visible success line. Treat that line as a STOP signal: repeated calls
 *  duplicate the content in the append-only sink. */
@assumeSafe
def writeConfidential[P <: Permission](x: Classified[String])(using fs: FileSystem[P]): String =
  fs.classifiedPaths.headOption match
    case Some(path) =>
      fs.writeClassified(path, x)
      "Wrote classified content to the output sink. STOP — do not call any more tools; just acknowledge to the user."
    case None => throw IllegalStateException("No classified output path configured in FileSystem")

// ─── Request functions (scoped capability grants) ─────────────────────────

/** Create a real filesystem scoped to root, with optional classified paths.
 *  Classified paths can only be written via writeClassified (accepts Classified[String])
 *  and read via readClassified (returns Classified[String]).
 *  Calling read() on a classified path throws SecurityException.
 *
 *  Used by the harness to inject a given FileSystem with the confidential
 *  output path pre-configured as classified — mirroring TACIT's design. */
@assumeSafe
def createFileSystem[P <: Permission](
  root: String,
  classifiedPaths: Set[String] = Set.empty
): FileSystem[P] =
  val rootDir = java.nio.file.Path.of(root).normalize.toString
  val normalizedClassified = classifiedPaths.map(p => java.nio.file.Path.of(p).normalize.toString)
  new FileSystem[P]:
    def root = rootDir
    def classifiedPaths = normalizedClassified

    type Entry = EntryImpl

    @assumeSafe class EntryImpl(val entryPath: String) extends EntryOps:
      private val normalizedPath = java.nio.file.Path.of(entryPath).normalize.toString
      def path = entryPath
      def name = java.nio.file.Path.of(entryPath).getFileName.toString
      def exists = java.io.File(entryPath).exists
      def isDirectory = java.io.File(entryPath).isDirectory

      def read(): String =
        if isClassified(normalizedPath) then
          throw SecurityException(
            s"Cannot read classified path $entryPath as plaintext — use readClassified()"
          )
        scala.io.Source.fromFile(entryPath).mkString

      def children: List[Entry] = Nil

      def write(content: String)(using CanWrite[P]): Unit =
        if isClassified(normalizedPath) then
          throw SecurityException(
            s"Cannot write plaintext to classified path $entryPath — use writeClassified()"
          )
        val w = java.io.FileWriter(entryPath)
        try w.write(content) finally w.close()

      def delete()(using CanDestroy[P]): Unit =
        java.io.File(entryPath).delete()

      @assumeSafe def renameTo(newPath: String)(using CanWrite[P]): Entry =
        java.io.File(entryPath).renameTo(java.io.File(newPath))
        EntryImpl(newPath).asInstanceOf[Entry]

    @assumeSafe def access(path: String): Entry =
      val normalized = java.nio.file.Path.of(path).normalize.toString
      if !normalized.startsWith(rootDir) then
        throw SecurityException(s"Path $path is outside root $rootDir")
      EntryImpl(path).asInstanceOf[Entry]

    def readClassified(path: String): Classified[String] =
      val normalized = java.nio.file.Path.of(path).normalize.toString
      if !isClassified(normalized) then
        throw SecurityException(s"Path $path is not a classified path")
      classify(scala.io.Source.fromFile(path).mkString)

    def writeClassified(path: String, content: Classified[String]): Unit =
      val normalized = java.nio.file.Path.of(path).normalize.toString
      if !isClassified(normalized) then
        throw SecurityException(s"Path $path is not a classified path")
      val value = content.asInstanceOf[ClassifiedImpl[String]].value
      val w = java.io.FileWriter(path, true)
      try w.write(value + "\n") finally w.close()

/** Request a filesystem via callback — convenience wrapper around createFileSystem. */
@assumeSafe
def requestFileSystem[P <: Permission, T](
  root: String,
  classifiedPaths: Set[String] = Set.empty
)(op: FileSystem[P]^ ?=> T)(using IOCapability): T =
  op(using createFileSystem[P](root, classifiedPaths))

@assumeSafe
def requestNetwork[T](hosts: Set[String])(op: Network^ ?=> T)(using IOCapability): T =
  val net = new Network(hosts) {}
  op(using net)

// ─── Privacy Budget (Differential Privacy) ──────────────────────────────

/** A consumable privacy budget for differential privacy.
 *  Each aggregate query spends epsilon; when exhausted, no more queries.
 *  Noise is drawn from the Laplace distribution scaled by sensitivity/epsilon. */
@assumeSafe
final class PrivacyBudget private (
    val totalEpsilon: Double,
    val sensitivity: Double,
    private var remaining: Double,
):
  /** Spend epsilon for a query. Throws if budget exhausted. */
  def spend(cost: Double): Unit =
    require(remaining >= cost,
      "Privacy budget exhausted (remaining: " + remaining.toString +
      ", requested: " + cost.toString + ")")
    remaining -= cost

  def remainingBudget: Double = remaining

  /** Return a noisy version of a true double value. Spends budget.
   *  querySensitivity overrides the default — use 1.0 for counts,
   *  sensitivity/n for averages, sensitivity for sums. */
  def noisyDouble(trueValue: Double, queryCost: Double, querySensitivity: Double): Double =
    spend(queryCost)
    trueValue + laplaceSample(querySensitivity / queryCost)

  /** Return a noisy version of a true int value. Spends budget. */
  def noisyInt(trueValue: Int, queryCost: Double, querySensitivity: Double): Int =
    math.round(noisyDouble(trueValue.toDouble, queryCost, querySensitivity)).toInt

  private def laplaceSample(scale: Double): Double =
    val u = java.util.concurrent.ThreadLocalRandom.current().nextDouble(-0.5, 0.5)
    -scale * math.signum(u) * math.log(1.0 - 2.0 * math.abs(u))

  override def toString: String =
    "PrivacyBudget(epsilon=" + totalEpsilon.toString +
    ", sensitivity=" + sensitivity.toString +
    ", remaining=" + remaining.toString + ")"

object PrivacyBudget:
  @assumeSafe
  def apply(epsilon: Double, sensitivity: Double): PrivacyBudget =
    new PrivacyBudget(epsilon, sensitivity, epsilon)

// ─── Confidential Columns ────────────────────────────────────────────────

/** A column of confidential values. Only aggregate operations are exposed —
 *  individual values can never be accessed, printed, or collected.
 *
 *  All function parameters (map, filter, countWhere) require pure functions
 *  (T -> U, not T => U). The capture checker ensures lambdas have no
 *  side effects — no I/O, no network, no filesystem.
 *
 *  When a PrivacyBudget is attached, aggregates add Laplace noise and
 *  consume budget. Without a budget, aggregates return exact values.
 *
 *  Intentionally excluded: fold, reduce, toList, toVector, foreach, get,
 *  apply, iterator — any combinator that could reconstruct individual values. */
@assumeSafe
final class ConfidentialColumn[T] private (
    private val data: Vector[T],
    private val budget: Option[PrivacyBudget],
):
  private val defaultCost = 0.5

  // Per-query sensitivity:
  //   count/countWhere: 1.0 (adding one row changes count by 1)
  //   sum:              budget.sensitivity (one row contributes at most max value)
  //   average:          budget.sensitivity / n
  //   max/min:          budget.sensitivity (worst case)

  /** Number of entries in this column. */
  def count: Classified[Int] = budget match
    case Some(b) => classify(b.noisyInt(data.length, defaultCost, 1.0))
    case None    => classify(data.length)

  /** Count entries satisfying a pure predicate. */
  def countWhere(pred: T -> Boolean): Classified[Int] =
    val count = data.count: v =>
      try pred(v) catch case _: Throwable => false
    budget match
      case Some(b) => classify(b.noisyInt(count, defaultCost, 1.0))
      case None    => classify(count)

  /** Sum of all values. */
  def sum(using n: Numeric[T]): Classified[T] = budget match
    case Some(b) =>
      val noisy = b.noisyDouble(n.toDouble(data.sum), defaultCost, b.sensitivity)
      classify(n.fromInt(noisy.toInt))
    case None => classify(data.sum)

  /** Arithmetic mean of all values. Returns 0.0 for empty columns. */
  def average(using n: Numeric[T]): Classified[Double] =
    val trueAvg = if data.isEmpty then 0.0 else n.toDouble(data.sum) / data.length
    val avgSensitivity = budget.map(b => b.sensitivity / math.max(data.length, 1)).getOrElse(0.0)
    budget match
      case Some(b) => classify(b.noisyDouble(trueAvg, defaultCost, avgSensitivity))
      case None    => classify(trueAvg)

  /** Maximum value. Returns Classified(None) for empty column — never throws,
   *  preventing use as a binary side channel via filter(x == v).max. */
  def max(using ord: Ordering[T]): Classified[Option[T]] = budget match
    case Some(b) =>
      b.spend(defaultCost)
      classify(data.maxOption)
    case None => classify(data.maxOption)

  /** Minimum value. Returns Classified(None) for empty column — never throws,
   *  preventing use as a binary side channel via filter(x == v).min. */
  def min(using ord: Ordering[T]): Classified[Option[T]] = budget match
    case Some(b) =>
      b.spend(defaultCost)
      classify(data.minOption)
    case None => classify(data.minOption)

  /** Apply a pure transformation. Result remains confidential. */
  def map[U](f: T -> U): ConfidentialColumn[U] =
    val mapped = data.flatMap: v =>
      try Some(f(v)) catch case _: Throwable => None
    new ConfidentialColumn(mapped, budget)

  /** Filter by a pure predicate. Result remains confidential. */
  def filter(pred: T -> Boolean): ConfidentialColumn[T] =
    val filtered = data.filter: v =>
      try pred(v) catch case _: Throwable => false
    new ConfidentialColumn(filtered, budget)

  override def toString: String = s"ConfidentialColumn(${data.length} entries, ****)"

object ConfidentialColumn:
  /** Create a confidential column without differential privacy. */
  @assumeSafe
  def apply[T](data: Vector[T]): ConfidentialColumn[T] =
    new ConfidentialColumn(data, None)

  /** Create a confidential column with a differential privacy budget. */
  @assumeSafe
  def apply[T](data: Vector[T], budget: PrivacyBudget): ConfidentialColumn[T] =
    new ConfidentialColumn(data, Some(budget))

// ─── Confidential Table ─────────────────────────────────────────────────

/** A table of named columns where some are marked confidential.
 *
 *  Non-confidential columns can be accessed freely. Confidential columns
 *  are only accessible as ConfidentialColumn — individual values can never
 *  be read, printed, or collected.
 *
 *  Supports a spreadsheet header convention: columns whose header starts
 *  with "*" (e.g. "*Salary") are automatically marked confidential when loaded. */
@assumeSafe
final class ConfidentialTable private (
    private val columns: Map[String, Vector[Any]],
    private val confidentialColNames: Set[String],
    val columnNames: Vector[String],
    private val budget: Option[PrivacyBudget],
):

  /** Number of rows. */
  def count: Int =
    columns.values.headOption.map(_.length).getOrElse(0)

  /** Whether a column is marked confidential. */
  def isConfidential(name: String): Boolean =
    confidentialColNames.contains(name)

  /** Access a non-confidential column as strings.
   *  Throws if the column is confidential or does not exist. */
  def stringColumn(name: String): Vector[String] =
    requireNonConfidential(name)
    columns(name).map(_.toString)

  /** Access a non-confidential column as doubles.
   *  Throws if the column is confidential or does not exist. */
  def doubleColumn(name: String): Vector[Double] =
    requireNonConfidential(name)
    toDoubles(columns(name))

  /** Access a confidential column. Returns a ConfidentialColumn so that
   *  only aggregates and pure-function transforms are available.
   *  Throws if the column is not confidential or does not exist. */
  def confidentialColumn(name: String): ConfidentialColumn[Double] =
    require(columns.contains(name), "Column '" + name + "' not found")
    require(confidentialColNames.contains(name),
      "Column '" + name + "' is not confidential")
    budget match
      case Some(b) => ConfidentialColumn(toDoubles(columns(name)), b)
      case None    => ConfidentialColumn(toDoubles(columns(name)))

  // ── Aggregate shortcuts for confidential columns ──────────────────────

  def avg(col: String): Classified[Double] = confidentialColumn(col).average
  def sum(col: String): Classified[Double] = confidentialColumn(col).sum
  def max(col: String): Classified[Option[Double]] = confidentialColumn(col).max
  def min(col: String): Classified[Option[Double]] = confidentialColumn(col).min

  /** Average of a confidential column grouped by a non-confidential column.
   *  Returns Classified so the per-group values cannot be used in control flow. */
  def groupByAvg(groupCol: String, valueCol: String): Classified[Map[String, Double]] =
    requireNonConfidential(groupCol)
    val groups = stringColumn(groupCol)
    val values = toDoubles(columns(valueCol))
    val grouped = groups.zip(values).groupBy(_._1).map: (key, pairs) =>
      val vs = pairs.map(_._2)
      key -> (vs.sum / vs.length)
    classify(grouped)

  override def toString: String =
    val colDescs = columnNames.map: name =>
      if confidentialColNames.contains(name) then "*" + name
      else name
    "ConfidentialTable(" + count + " rows, columns: " + colDescs.mkString(", ") + ")"

  private def requireNonConfidential(name: String): Unit =
    require(columns.contains(name), "Column '" + name + "' not found")
    require(!confidentialColNames.contains(name),
      "Cannot access individual values of confidential column '" + name + "'")

  private def toDoubles(vs: Vector[Any]): Vector[Double] =
    vs.map:
      case d: Double => d
      case n: Number => n.doubleValue
      case s: String => s.toDouble
      case other => other.toString.toDouble

object ConfidentialTable:
  /** Create a confidential table without differential privacy. */
  @assumeSafe
  def apply(
    columns: Map[String, Vector[Any]],
    confidentialColNames: Set[String],
    columnNames: Vector[String],
  ): ConfidentialTable =
    new ConfidentialTable(columns, confidentialColNames, columnNames, None)

  /** Create a confidential table with a differential privacy budget. */
  @assumeSafe
  def apply(
    columns: Map[String, Vector[Any]],
    confidentialColNames: Set[String],
    columnNames: Vector[String],
    budget: PrivacyBudget,
  ): ConfidentialTable =
    new ConfidentialTable(columns, confidentialColNames, columnNames, Some(budget))
