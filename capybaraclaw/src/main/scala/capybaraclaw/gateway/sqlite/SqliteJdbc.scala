package capybaraclaw.gateway.sqlite

import java.sql.{Connection, PreparedStatement, ResultSet}

private[sqlite] def bestEffort(body: => Unit): Unit =
  try body
  catch case _: Throwable => ()

private[sqlite] object SqliteJdbc:

  def execute(conn: Connection, sql: String): Unit =
    val stmt = conn.createStatement()
    try
      stmt.execute(sql)
      ()
    finally stmt.close()

  def withStatement[A](conn: Connection, sql: String)(
      body: PreparedStatement => A
  ): A =
    val stmt = conn.prepareStatement(sql)
    try body(stmt)
    finally stmt.close()

  def withResultSet[A](rs: ResultSet)(body: ResultSet => A): A =
    try body(rs)
    finally rs.close()
