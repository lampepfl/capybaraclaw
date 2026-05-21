package capybaraclaw.gateway.port.slack

import capybaraclaw.gateway.{
  GatewayMessage,
  Origin,
  PortId,
  SessionHandle,
  SessionId,
  SessionRef,
  UserId
}
import capybaraclaw.gateway.port.Port
import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import org.slf4j.LoggerFactory

/** Gateway Port backed by Slack Socket Mode.
  *
  * SessionHandle encoding:
  *   - in a thread: `SessionHandle.value = s"$channelId/$threadTs"`
  *   - top-level:   `SessionHandle.value = channelId`
  *
  * Outbound sends use the original local id because the canonical session id is a
  * UUID and cannot be decoded into Slack coordinates.
  */
class SlackPort(bot: SlackApi) extends Port:
  val id: PortId = SlackPort.Id

  private val logger = LoggerFactory.getLogger(classOf[SlackPort])
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
          val handle = getSlackHandle(origin)
          logIncomingMessage(handle, origin.user, slackMsg.text)
          outCh.sendImmediately(GatewayMessage(origin, slackMsg.text))
        case Left(_) =>
          running = false

  override def validateOriginForReply(origin: Origin): Unit =
    val _ = decodeHandle(getSlackHandle(origin))

  override def rejectInbound(origin: Origin, text: String): Unit =
    origin.session match
      case SessionRef.External(handle) if handle.kind == SlackPort.Id =>
        try
          val (channelId, threadTs) = decodeHandle(handle)
          bot.sendMessage(channelId, formatRejectMessage(text), threadTs)
        catch
          case e: Exception =>
            logger.warn(
              s"[slack] failed to deliver reject for handle ${handle.value}",
              e
            )
      case SessionRef.Direct(sessionId) =>
        logger.warn(
          "[slack] dropping reject for direct session {}: {}",
          sessionId,
          text
        )
      case SessionRef.External(_) =>
        ()

  def send(sessionId: SessionId, origin: Origin, text: String): Unit =
    val handle = getSlackHandle(origin)
    val (channelId, threadTs) = decodeHandle(handle)
    logOutgoingMessage(sessionId, handle, text)
    bot.sendMessage(channelId, text, threadTs)

  def shutdown(): Unit =
    try outCh.close()
    catch case _: Throwable => ()
    try bot.shutdown()
    catch case _: Throwable => ()

  private def toOrigin(msg: Message): Origin =
    val raw = msg.threadTs match
      case Some(ts) => s"${msg.origin.channelId}/$ts"
      case None     => msg.origin.channelId
    Origin(
      port = id,
      user = UserId(msg.userId),
      session = SessionRef.External(SessionHandle(SlackPort.Id, raw))
    )

  private def getSlackHandle(origin: Origin): SessionHandle =
    origin.session match
      case SessionRef.External(handle) => handle
      case SessionRef.Direct(_)        =>
        throw IllegalArgumentException("Slack replies require a session handle")

  private def decodeHandle(
      handle: SessionHandle
  ): (String, Option[String]) =
    val raw = handle.value
    raw.indexOf('/') match
      case -1 =>
        if raw.isEmpty then
          throw IllegalArgumentException("Slack channel id must not be empty")
        (raw, None)
      case i =>
        val channelId = raw.substring(0, i)
        val threadTs = raw.substring(i + 1)
        if channelId.isEmpty || threadTs.isEmpty then
          throw IllegalArgumentException(
            s"Invalid Slack handle value: ${handle.value}"
          )
        (channelId, Some(threadTs))

  private def formatRejectMessage(text: String): String =
    s""":warning: *Capybara Claw could not process that message*
       |```$text```""".stripMargin

  private def logIncomingMessage(
      handle: SessionHandle,
      user: UserId,
      text: String
  ): Unit =
    logger.info(
      "[slack <-] ({}) {} ({} chars)",
      handle.value,
      user,
      text.length
    )
    logger.debug(
      "[slack <-] ({}) {}: {}",
      handle.value,
      user,
      snippet(text)
    )

  private def logOutgoingMessage(
      sessionId: SessionId,
      handle: SessionHandle,
      text: String
  ): Unit =
    logger.info(
      "[slack ->] ({}, {}) ({} chars)",
      sessionId,
      handle.value,
      text.length
    )
    logger.debug(
      "[slack ->] ({}, {}) {}",
      sessionId,
      handle.value,
      snippet(text)
    )

  private def snippet(text: String, max: Int = 200): String =
    val oneLine = text.replace('\n', ' ').replace('\r', ' ')
    if oneLine.length <= max then oneLine
    else oneLine.substring(0, max) + "…"

object SlackPort:
  val Id: PortId = PortId("slack")
