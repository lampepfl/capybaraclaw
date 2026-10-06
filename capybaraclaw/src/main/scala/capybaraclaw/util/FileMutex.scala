package capybaraclaw.util

import java.nio.channels.FileChannel
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import java.util.concurrent.ConcurrentHashMap

object FileMutex:
  private val monitors: ConcurrentHashMap[String, AnyRef] =
    ConcurrentHashMap[String, AnyRef]()

  private def monitorFor(key: String): AnyRef =
    monitors.computeIfAbsent(key, _ => Object()).nn

  def withLock[A](lockFile: Path)(body: => A): A =
    val key = lockFile.toAbsolutePath.nn.normalize.nn.toString
    monitorFor(key).synchronized:
      val channel = FileChannel.open(
        lockFile,
        StandardOpenOption.CREATE,
        StandardOpenOption.READ,
        StandardOpenOption.WRITE
      )
      try
        val fileLock = channel.lock()
        try body
        finally fileLock.release()
      finally channel.close()
