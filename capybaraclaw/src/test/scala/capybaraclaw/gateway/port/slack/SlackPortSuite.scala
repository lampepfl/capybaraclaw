package capybaraclaw.gateway.port.slack

import capybaraclaw.gateway.{
  Origin,
  PortId,
  SessionHandle,
  SessionId,
  SessionRef
}
import gears.async.{ReadableChannel, UnboundedChannel}

class SlackPortSuite extends munit.FunSuite:

  test("send routes a channel handle as a top-level Slack message"):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123"))

    port.send(sessionId, origin, "hello")

    assertEquals(bot.sent.toList, List(Sent("C123", "hello", None)))

  test("send routes a channel/thread handle as a Slack thread reply"):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    port.send(sessionId, origin, "hello")

    assertEquals(
      bot.sent.toList,
      List(Sent("C123", "hello", Some("123.456")))
    )

  test("send rejects direct session refs"):
    val port = SlackPort(FakeSlackApi())
    val sessionId = SessionId.random()
    val origin = Origin(
      port = SlackPort.Id,
      user = "U1",
      session = SessionRef.Direct(sessionId)
    )

    intercept[IllegalArgumentException]:
      port.send(sessionId, origin, "hello")

  test("send rejects handles for a different port kind"):
    val port = SlackPort(FakeSlackApi())
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(PortId("other"), "C123"))

    intercept[IllegalArgumentException]:
      port.send(sessionId, origin, "hello")

  test("send rejects malformed Slack thread handles"):
    val port = SlackPort(FakeSlackApi())
    val sessionId = SessionId.random()

    List("/123.456", "C123/").foreach: raw =>
      val origin = slackOrigin(SessionHandle(SlackPort.Id, raw))
      intercept[IllegalArgumentException]:
        port.send(sessionId, origin, "hello")

  test(
    "rejectInbound on a thread handle delivers the error to the originating Slack thread"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    port.rejectInbound(origin, "session resolution failed")

    assertEquals(
      bot.sent.toList,
      List(Sent("C123", "ERROR: session resolution failed", Some("123.456")))
    )

  test(
    "rejectInbound on a channel-only handle delivers the error to the channel without thread"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123"))

    port.rejectInbound(origin, "boom")

    assertEquals(bot.sent.toList, List(Sent("C123", "ERROR: boom", None)))

  test(
    "rejectInbound swallows bot failures so gateway is not destabilized"
  ):
    val bot = FailingSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    port.rejectInbound(origin, "boom") // must not throw

  test(
    "rejectInbound ignores non-slack external handles instead of mis-routing"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(PortId("other"), "C123"))

    port.rejectInbound(origin, "boom")

    assertEquals(bot.sent.toList, Nil)

  private def slackOrigin(handle: SessionHandle): Origin =
    Origin(
      port = SlackPort.Id,
      user = "U1",
      session = SessionRef.External(handle)
    )

private final case class Sent(
    channel: String,
    text: String,
    threadTs: Option[String]
)

private final class FakeSlackApi extends SlackApi:
  private val messages = UnboundedChannel[Message]()
  private val sentMessages = scala.collection.mutable.ListBuffer.empty[Sent]

  def sent: Seq[Sent] = sentMessages.toList

  def sendMessage(
      channel: String,
      text: String,
      threadTs: Option[String]
  ): String =
    sentMessages += Sent(channel, text, threadTs)
    "ts"

  def readHistory(channel: String, limit: Int): List[Message] = Nil

  def getChannel(id: String): Channel =
    Channel(id, id, "", "", isPrivate = false, isIm = false, isArchived = false)

  def getUser(id: String): User =
    User(id, id, id, id)

  def messageChannel: ReadableChannel[Message] = messages.asReadable

  def shutdown(): Unit = messages.close()

private final class FailingSlackApi extends SlackApi:
  private val messages = UnboundedChannel[Message]()

  def sendMessage(
      channel: String,
      text: String,
      threadTs: Option[String]
  ): String =
    throw new RuntimeException(s"slack api is down: $channel")

  def readHistory(channel: String, limit: Int): List[Message] = Nil

  def getChannel(id: String): Channel =
    Channel(id, id, "", "", isPrivate = false, isIm = false, isArchived = false)

  def getUser(id: String): User =
    User(id, id, id, id)

  def messageChannel: ReadableChannel[Message] = messages.asReadable

  def shutdown(): Unit = messages.close()
