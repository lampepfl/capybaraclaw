package capybaraclaw.gateway.port.slack

import capybaraclaw.gateway.{ContextKey, GatewayMessage, Origin}
import capybaraclaw.gateway.port.Port
import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}

/** Gateway Port backed by Slack Socket Mode.
  *
  * Thread encoding:
  *   - in a thread: `Origin.thread = s"$channelId/$threadTs"`
  *   - top-level:   `Origin.thread = channelId`
  *
  * Outbound `send(key, text)` decodes that back so replies land in the originating
  * thread (or top-level channel).
  */
class SlackPort(bot: SlackBot) extends Port:
  val id: String = SlackPort.Id

  private val outCh = UnboundedChannel[GatewayMessage]()

  def incoming: ReadableChannel[GatewayMessage] = outCh.asReadable

  /** Spawn a reader fiber that pumps Slack messages into the gateway channel. */
  def start()(using Async.Spawn): Future[Unit] =
    Future(readLoop())

  private def readLoop()(using Async.Spawn): Unit =
    var running = true
    while running do
      bot.messageChannel.read() match
        case Right(slackMsg) =>
          val origin = toOrigin(slackMsg)
          logIn(origin, slackMsg.text)
          outCh.sendImmediately(GatewayMessage(origin, slackMsg.text))
        case Left(_) =>
          running = false

  def send(key: ContextKey, text: String): Unit =
    val (channelId, threadTs) = decodeThread(key.thread)
    logOut(key, text)
    bot.sendMessage(channelId, text, threadTs)

  def shutdown(): Unit =
    try outCh.close()
    catch case _: Throwable => ()
    try bot.shutdown()
    catch case _: Throwable => ()

  private def toOrigin(msg: Message): Origin =
    val thread = msg.threadTs match
      case Some(ts) => s"${msg.origin.channelId}/$ts"
      case None     => msg.origin.channelId
    Origin(id, thread, msg.userId)

  private def decodeThread(thread: String): (String, Option[String]) =
    thread.indexOf('/') match
      case -1 => (thread, None)
      case i  => (thread.substring(0, i), Some(thread.substring(i + 1)))

  private def logIn(origin: Origin, text: String): Unit =
    println(s"[slack <-] (${origin.thread}) ${origin.user}: ${snippet(text)}")

  private def logOut(key: ContextKey, text: String): Unit =
    println(s"[slack ->] (${key.thread}) ${snippet(text)}")

  private def snippet(text: String, max: Int = 200): String =
    val oneLine = text.replace('\n', ' ').replace('\r', ' ')
    if oneLine.length <= max then oneLine
    else oneLine.substring(0, max) + "…"

object SlackPort:
  val Id: String = "slack"
