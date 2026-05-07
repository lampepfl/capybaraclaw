package capybaraclaw.gateway.sqlite

import java.io.File
import java.sql.{Connection, DriverManager}
import java.util.concurrent.ArrayBlockingQueue

private[sqlite] class SqliteReaderPool(dbFile: File, size: Int)
    extends AutoCloseable:

  private val (allConnections, available) = initialize()

  def withReader[A](body: Connection => A): A =
    val conn = available.take()
    if conn.isClosed then
      bestEffort:
        available.put(conn)
      throw IllegalStateException("reader pool closed")
    try body(conn)
    finally available.put(conn)

  override def close(): Unit =
    allConnections.foreach: c =>
      bestEffort:
        c.close()

  private def initialize(): (List[Connection], ArrayBlockingQueue[Connection]) =
    val queue = ArrayBlockingQueue[Connection](size)
    val opened = scala.collection.mutable.ListBuffer[Connection]()
    try
      (1 to size).foreach: _ =>
        val c = openReader()
        opened += c
        queue.put(c)
      (opened.toList, queue)
    catch
      case e: Throwable =>
        opened.foreach: c =>
          bestEffort:
            c.close()
        throw e

  private def openReader(): Connection =
    val c = DriverManager.getConnection(s"jdbc:sqlite:${dbFile.getPath}")
    try
      SqliteJdbc.execute(c, "PRAGMA busy_timeout = 5000")
      SqliteJdbc.execute(c, "PRAGMA query_only = 1")
      c
    catch
      case e: Throwable =>
        bestEffort:
          c.close()
        throw e
