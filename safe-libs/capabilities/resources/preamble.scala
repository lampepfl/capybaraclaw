// REPL preamble contributed by the safemode-capabilities plugin. Establishes
// the capability layer: imports `safemode.lib.{*, given}` so Classified[T],
// FileSystem[P], capability brokers, etc. are in scope, and provides default
// IOCapability + FileSystem[FullAccess] givens bound to the host's workdir.
//
// JVM system properties the host (capybaraclaw) sets before starting the
// REPL:
//   safemode.workdir         — directory the FileSystem capability is scoped to
//   safemode.classifiedPath  — path where writeConfidential appends. Must be
//                              inside workdir. Defaults to
//                              <workdir>/.safe-output.txt.
import safemode.lib.{*, given}

given IOCapability = IOCapability()

@scala.caps.assumeSafe given FileSystem[FullAccess] =
  val workdir = System.getProperty("safemode.workdir", ".")
  val classified = System.getProperty(
    "safemode.classifiedPath",
    java.io.File(workdir, ".safe-output.txt").getAbsolutePath
  )
  createFileSystem[FullAccess](workdir, Set(classified))
