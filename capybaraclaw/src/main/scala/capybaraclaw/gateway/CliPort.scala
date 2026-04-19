package capybaraclaw.gateway

import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}

/** Simple stdin/stdout Port. Useful for local testing without Slack. One thread
  * ("stdin"), one user (taken from `$USER`). Type `quit` (or EOF) to close.
  */
class CliPort(
  override val id: String = CliPort.Id,
  user: String = sys.env.getOrElse("USER", "cli"),
  thread: String = "stdin",
) extends Port:

  private val outCh = UnboundedChannel[GatewayMessage]()

  def incoming: ReadableChannel[GatewayMessage] = outCh.asReadable

  def start()(using Async.Spawn): Future[Unit] =
    Future(readLoop())

  private def readLoop(): Unit =
    var running = true
    while running do
      val line = scala.io.StdIn.readLine()
      if line == null || line.trim == "quit" then
        try outCh.close() catch case _: Throwable => ()
        running = false
      else if line.nonEmpty then
        outCh.sendImmediately(GatewayMessage(Origin(id, thread, user), line))

  def send(key: ContextKey, text: String): Unit =
    println()
    println(text)
    print("> ")
    System.out.flush()

  def shutdown(): Unit =
    try outCh.close() catch case _: Throwable => ()

object CliPort:
  val Id: String = "cli"
