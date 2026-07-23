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
import capybaraclaw.gateway.port.{Port, ReplyStream}
import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import org.slf4j.LoggerFactory
import scala.util.control.NonFatal

/** Gateway Port backed by Slack Socket Mode.
  *
  * SessionHandle encoding (`SessionHandle.value`):
  *   - in a thread: `s"$channelId/$threadTs"`
  *   - top-level:   `s"$channelId/$ts"` - anchored on the message's own ts, so the
  *     reply threads under it (and can stream) and the message plus every follow-up
  *     in that thread share one session. A new top-level message starts a fresh
  *     thread = fresh session.
  *   - `channelId` alone is only a defensive fallback for a missing ts.
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

  override def openReply(sessionId: SessionId, origin: Origin): ReplyStream =
    val handle = getSlackHandle(origin)
    val (channelId, threadTs) = decodeHandle(handle)

    import SlackPort.StreamState
    import SlackPort.StreamState.*

    final class SlackReply(state: StreamState) extends ReplyStream:
      def delta(text: String): ReplyStream =
        if text.isEmpty then new SlackReply(state)
        else
          (threadTs, state) match
            case (Some(tts), NotStarted) =>
              startAndAppend(tts, text)
            case (Some(_), Streaming(ts)) =>
              try
                bot.appendStream(channelId, ts, text)
                new SlackReply(state)
              catch case NonFatal(e) => abandon(Some(ts), e)
            case _ => new SlackReply(state)
      end delta

      def complete(finalText: String): Unit =
        logOutgoingMessage(sessionId, handle, finalText)
        try
          state match
            case Streaming(ts) =>
              bot.stopStream(channelId, ts)
            case Abandoned(Some(ts)) =>
              try
                if finalText.nonEmpty then
                  bot.sendMessage(channelId, finalText, threadTs)
              finally closeQuietly(ts)
            case Abandoned(None) | NotStarted =>
              if finalText.nonEmpty then
                bot.sendMessage(channelId, finalText, threadTs)
        catch
          case NonFatal(e) =>
            logger.warn(
              s"[slack] failed to deliver reply for handle ${handle.value}",
              e
            )
      end complete

      def abort(reason: String): Unit =
        state match
          case Streaming(ts) =>
            try
              bot.appendStream(channelId, ts, s"\nERROR: $reason")
              bot.stopStream(channelId, ts)
            catch
              case NonFatal(e) =>
                logger.warn(
                  s"[slack] failed to abort stream for handle ${handle.value}, posting error directly",
                  e
                )
                closeQuietly(ts)
                postErrorDirectly(reason)
          case Abandoned(Some(ts)) =>
            postErrorDirectly(reason)
            closeQuietly(ts)
          case Abandoned(None) | NotStarted =>
            complete(s"ERROR: $reason")
      end abort

      private def startAndAppend(tts: String, text: String): ReplyStream =
        try
          new SlackReply(
            Streaming(bot.startStream(channelId, tts, Some(origin.user), text))
          )
        catch
          case NonFatal(e) =>
            abandonLog(e)
            new SlackReply(Abandoned(None))
      end startAndAppend

      private def abandon(ts: Option[String], e: Throwable): ReplyStream =
        abandonLog(e)
        new SlackReply(Abandoned(ts))

      private def abandonLog(e: Throwable): Unit =
        logger.warn(
          s"[slack] streaming failed for handle ${handle.value}, falling back",
          e
        )

      private def postErrorDirectly(reason: String): Unit =
        try bot.sendMessage(channelId, s"ERROR: $reason", threadTs)
        catch case NonFatal(_) => ()

      private def closeQuietly(ts: String): Unit =
        try bot.stopStream(channelId, ts)
        catch case NonFatal(_) => ()
    end SlackReply

    new SlackReply(NotStarted)
  end openReply

  def shutdown(): Unit =
    try outCh.close()
    catch case _: Throwable => ()
    try bot.shutdown()
    catch case _: Throwable => ()

  private def toOrigin(msg: Message): Origin =
    val raw = SlackPort.handleValue(msg.origin.channelId, msg.threadTs, msg.ts)
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
end SlackPort

object SlackPort:
  val Id: PortId = PortId("slack")

  private enum StreamState:
    case NotStarted
    case Streaming(ts: String)
    case Abandoned(ts: Option[String])

  def handleValue(
      channelId: String,
      threadTs: Option[String],
      ts: String
  ): String =
    threadTs.orElse(Option(ts).filter(_.nonEmpty)) match
      case Some(t) => s"$channelId/$t"
      case None    => channelId
end SlackPort
