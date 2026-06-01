# Safemode Capabilities

Foundation API: information-flow types and capability brokers used by every
other safemode plugin. The REPL preamble pre-imports `safemode.lib.{*, given}`
and binds a `given IOCapability` plus a `given FileSystem[FullAccess]` scoped
to the host workdir, so the symbols below are available to the agent without
further imports.

## Classified containment

```scala
trait Classified[+T]:
  def map[U](op: T -> U): Classified[U]              // pure function (capture-checked)
  def flatMap[U](op: T -> Classified[U]): Classified[U]
  override def toString: String                       // always "Classified(****)"
```

`Classified[T]` wraps a sensitive value. `toString` reveals nothing. `map` and
`flatMap` only accept **pure** arrows (`T -> U`, not `T => U`); capturing an
`IOCapability`, `Network`, or writable `FileSystem` in those arrows is a
compile-time error.

## Permission lattice

```scala
sealed trait Permission
sealed trait ReadOnly        extends Permission
sealed trait NonDestructive  extends Permission  // read + write-no-overwrite
sealed trait FullAccess      extends Permission  // read + write + delete + move
```

Evidence types `CanWrite[P]` and `CanDestroy[P]` gate operations at the type
level. Attempting a write through a `FileSystem[ReadOnly]` is a compile error
with a guided message.

## FileSystem

```scala
abstract class FileSystem[P <: Permission] extends SharedCapability:
  val Entry: AnyRef                          // path-dependent — entries from
  abstract type EntryT <: Entry.type         // different FS instances are
                                             // incompatible at the type level
  def root: String
  def classifiedPaths: Set[String]
  def entry(relative: String): EntryT
  def read(entry: EntryT)(using CanRead[P]): String
  def write(entry: EntryT, content: String)(using CanWrite[P]): Unit
  def writeClassified(entry: EntryT, content: Classified[String])(using CanWrite[P]): Unit
  def readClassified(entry: EntryT)(using CanRead[P]): Classified[String]
  def list(entry: EntryT)(using CanRead[P]): List[EntryT]
  // ... copy, move, delete gated by CanDestroy[P]
```

Calling `read()` on a classified path throws `SecurityException` at runtime.
Use `readClassified` instead. Reading from a classified path always returns
`Classified[String]`.

## Capability brokers

```scala
def requestFileSystem[P <: Permission, T](
    root: String, classifiedPaths: Set[String] = Set.empty
)(op: FileSystem[P]^ ?=> T): T

def requestNetwork[T](hosts: Set[String])(op: Network^ ?=> T)(using IOCapability): T

def requestExecPermission[T](commands: Set[String])(op: ProcessPermission^ ?=> T)(using IOCapability): T
```

Brokers grant a scoped capability for the duration of `op` and revoke on exit.

## Top-level utilities

```scala
@assumeSafe def safePrintln(x: Any)(using IOCapability): Unit
@assumeSafe def httpGet(url: String)(using Network): String
@assumeSafe def httpPost(url: String, body: String)(using Network): String

@assumeSafe def writeConfidential[P <: Permission](
    x: Classified[String]
)(using FileSystem[P]): String
// Appends to the configured classified path. Default sink:
// ${safemode.workdir}/.safe-output.txt.
// Returns a fixed STOP confirmation string so the REPL prints a visible
// success line; treat that line as a STOP signal (the sink is append-only;
// repeated calls duplicate the content).

@assumeSafe def copyFile[P1, P2](...) / moveFile[P1, P2](...)
@assumeSafe def classify[T](value: T): Classified[T]   // for tests/local seal
```

## ConfidentialTable and ConfidentialColumn

```scala
final class ConfidentialColumn[T]:
  def map[U](op: T -> U): ConfidentialColumn[U]
  def filter(p: T -> Boolean): ConfidentialColumn[T]
  def count: Classified[Int]
  def sum(implicit ev: Numeric[T]): Classified[T]
  def avg(implicit ev: Numeric[T]): Classified[Double]
  def max / min (implicit ev: Ordering[T]): Classified[T]
  def countWhere(p: T -> Boolean): Classified[Int]

final class ConfidentialTable:
  def stringColumn(name: String): Either[Throwable, List[String]]      // public column
  def doubleColumn(name: String): Either[Throwable, List[Double]]      // public column
  def confidentialColumn(name: String): ConfidentialColumn[Double]     // sealed
  def groupByAvg(byColumn: String, valueColumn: String): Classified[Map[String, Double]]
  // ... avg / sum / max / min over confidential columns
```

## Differential privacy

```scala
final class PrivacyBudget:
  val totalEpsilon: Double
  def remaining: Double
  def spend(epsilon: Double): Unit       // throws if exhausted

object PrivacyBudget:
  @assumeSafe def apply(epsilon: Double): PrivacyBudget
  @assumeSafe def laplaceSample(scale: Double): Double
```

Aggregates that accept a `PrivacyBudget` add Laplace noise scaled by
`sensitivity / epsilon`. Out-of-the-box demos run with `epsilon = 1.0`, which
is the default in dependent plugins (e.g. compreview).
