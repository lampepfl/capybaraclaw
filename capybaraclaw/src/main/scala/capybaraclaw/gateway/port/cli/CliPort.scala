package capybaraclaw.gateway.port.cli

import capybaraclaw.gateway.{ContextKey, GatewayMessage, Origin}
import capybaraclaw.gateway.port.Port
import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}

/** Simple stdin/stdout Port. Useful for local testing without Slack. One thread
  * ("stdin"), one user (taken from `$USER`). Type `quit`, `/quit`, `exit`, `/exit`,
  * or hit Ctrl-D to close.
  */
class CliPort(
    override val id: String = CliPort.Id,
    user: String = sys.env.getOrElse("USER", "cli"),
    thread: String = "stdin"
) extends Port:

  private val outCh = UnboundedChannel[GatewayMessage]()

  def incoming: ReadableChannel[GatewayMessage] = outCh.asReadable

  def start()(using Async.Spawn): Future[Unit] =
    Future(readLoop())

  private def readLoop(): Unit =
    printHint()
    prompt()
    var running = true
    while running do
      val line = scala.io.StdIn.readLine()
      line match
        case null =>
          println()
          println("bye!")
          closeChannel()
          running = false
        case l if CliPort.QuitCommands.contains(l.trim.toLowerCase) =>
          println("bye!")
          closeChannel()
          running = false
        case l if l.trim.isEmpty =>
          prompt()
        case l =>
          outCh.sendImmediately(GatewayMessage(Origin(id, thread, user), l))

  def send(key: ContextKey, text: String): Unit =
    println()
    println(formatReply(text))
    println()
    prompt()

  def shutdown(): Unit = closeChannel()

  private def printHint(): Unit =
    println()
    println(s"Signed in as $user. Type 'quit' or Ctrl-D to exit.")
    println()

  private def prompt(): Unit =
    print(CliPort.UserPrompt)
    System.out.flush()

  /** Prefix the first line of the agent's reply with `claw > ` and indent every
    * continuation line to align with it, so multi-line replies stay readable.
    */
  private def formatReply(text: String): String =
    val lines = if text.isEmpty then List("") else text.linesIterator.toList
    val indent = " " * CliPort.AgentLabel.length
    val head = CliPort.AgentLabel + lines.head
    (head :: lines.tail.map(indent + _)).mkString("\n")

  private def closeChannel(): Unit =
    try outCh.close()
    catch case _: Throwable => ()

object CliPort:
  val Id: String = "cli"
  private val UserPrompt = "you > "
  private val AgentLabel = "claw > "
  private val QuitCommands = Set("quit", "/quit", "exit", "/exit")
