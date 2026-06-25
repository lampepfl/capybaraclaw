package capybaraclaw.gateway.port.slack

import capybaraclaw.gateway.{
  Origin,
  PortId,
  SessionHandle,
  SessionId,
  SessionRef,
  UserId
}
import gears.async.{ReadableChannel, UnboundedChannel}
import scala.collection.mutable.ListBuffer

class SlackPortSuite extends munit.FunSuite:

  test("complete routes a channel handle as a top-level Slack message"):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123"))

    port.openReply(sessionId, origin).complete("hello")

    assertEquals(bot.sent.toList, List(Sent("C123", "hello", None)))

  test("complete routes a channel/thread handle as a Slack thread reply"):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    port.openReply(sessionId, origin).complete("hello")

    assertEquals(
      bot.sent.toList,
      List(Sent("C123", "hello", Some("123.456")))
    )

  test("complete rejects direct session refs"):
    val port = SlackPort(FakeSlackApi())
    val sessionId = SessionId.random()
    val origin = Origin(
      port = SlackPort.Id,
      user = UserId("U1"),
      session = SessionRef.Direct(sessionId)
    )

    intercept[IllegalArgumentException]:
      port.openReply(sessionId, origin).complete("hello")

  test("complete rejects malformed Slack thread handles"):
    val port = SlackPort(FakeSlackApi())
    val sessionId = SessionId.random()

    List("/123.456", "C123/").foreach: raw =>
      val origin = slackOrigin(SessionHandle(SlackPort.Id, raw))
      intercept[IllegalArgumentException]:
        port.openReply(sessionId, origin).complete("hello")

  test("abort sends an error-prefixed Slack message"):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123"))

    port.openReply(sessionId, origin).abort("timeout")

    assertEquals(bot.sent.toList, List(Sent("C123", "ERROR: timeout", None)))

  test("deltas on a thread handle stream natively, complete stops the stream"):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    port.openReply(sessionId, origin).delta("a").delta("b").complete("ab")

    assertEquals(
      bot.started.toList,
      List(Started("C123", "123.456", Some("U1"), "a"))
    )
    assertEquals(
      bot.appended.toList,
      List(Appended("C123", "ts", "b"))
    )
    assertEquals(bot.stopped.toList, List(Stopped("C123", "ts")))
    assertEquals(bot.sent.toList, Nil)

  test(
    "delta returns a reply stream that carries the opened Slack stream state"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    val openedReply = port.openReply(sessionId, origin).delta("a")
    openedReply.complete("a")

    assertEquals(
      bot.started.toList,
      List(Started("C123", "123.456", Some("U1"), "a"))
    )
    assertEquals(bot.stopped.toList, List(Stopped("C123", "ts")))
    assertEquals(bot.sent.toList, Nil)

  test(
    "a startStream failure falls back to a single post with no empty bubble"
  ):
    val bot = FakeSlackApi(failStart = true)
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    port.openReply(sessionId, origin).delta("Hello ").complete("Hello world")

    assertEquals(bot.started.toList, Nil)
    assertEquals(bot.stopped.toList, Nil)
    assertEquals(
      bot.sent.toList,
      List(Sent("C123", "Hello world", Some("123.456")))
    )

  test(
    "an append failure after the stream starts still delivers the full reply"
  ):
    val bot = FakeSlackApi(failAppend = true)
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    port
      .openReply(sessionId, origin)
      .delta("Hello ")
      .delta("world")
      .complete("Hello world")

    assertEquals(
      bot.started.toList,
      List(Started("C123", "123.456", Some("U1"), "Hello "))
    )
    assertEquals(bot.stopped.toList, List(Stopped("C123", "ts")))
    assertEquals(
      bot.sent.toList,
      List(Sent("C123", "Hello world", Some("123.456")))
    )

  test("complete closes an abandoned stream even when fallback post fails"):
    val bot = FakeSlackApi(failAppend = true, failSend = true)
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    port
      .openReply(sessionId, origin)
      .delta("Hello ")
      .delta("world")
      .complete("Hello world")

    assertEquals(bot.stopped.toList, List(Stopped("C123", "ts")))

  test("abort after a failed stream still surfaces the error to the user"):
    val bot = FakeSlackApi(failAppend = true)
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    port.openReply(sessionId, origin).delta("partial").abort("timeout")

    assertEquals(
      bot.sent.toList,
      List(Sent("C123", "ERROR: timeout", Some("123.456")))
    )

  test("abort closes an open stream even when appending the error fails"):
    val bot = FakeSlackApi(failAppend = true)
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    port.openReply(sessionId, origin).delta("partial").abort("timeout")

    assertEquals(bot.stopped.toList, List(Stopped("C123", "ts")))

  test("deltas on a top-level handle fall back to a single post on complete"):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val sessionId = SessionId.random()
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123"))

    port.openReply(sessionId, origin).delta("a").complete("a")

    assertEquals(bot.started.toList, Nil)
    assertEquals(bot.appended.toList, Nil)
    assertEquals(bot.sent.toList, List(Sent("C123", "a", None)))

  test(
    "rejectInbound on a thread handle delivers the error to the originating Slack thread"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/123.456"))

    port.rejectInbound(origin, "session resolution failed")

    assertEquals(
      bot.sent.toList,
      List(
        Sent(
          "C123",
          """:warning: *Capybara Claw could not process that message*
            |```session resolution failed```""".stripMargin,
          Some("123.456")
        )
      )
    )

  test(
    "rejectInbound on a channel-only handle delivers the error to the channel without thread"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123"))

    port.rejectInbound(origin, "boom")

    assertEquals(
      bot.sent.toList,
      List(
        Sent(
          "C123",
          """:warning: *Capybara Claw could not process that message*
            |```boom```""".stripMargin,
          None
        )
      )
    )

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

  test("handleValue: a thread ts wins over the message ts"):
    assertEquals(
      SlackPort.handleValue("C1", Some("100.5"), "200.9"),
      "C1/100.5"
    )

  test("handleValue: a top-level message anchors on its own ts"):
    assertEquals(SlackPort.handleValue("C1", None, "200.9"), "C1/200.9")

  test("handleValue: falls back to channel-only when ts is missing"):
    assertEquals(SlackPort.handleValue("C1", None, ""), "C1")

  private def slackOrigin(handle: SessionHandle): Origin =
    Origin(
      port = SlackPort.Id,
      user = UserId("U1"),
      session = SessionRef.External(handle)
    )

private final case class Sent(
    channel: String,
    text: String,
    threadTs: Option[String]
)

private final case class Started(
    channel: String,
    threadTs: String,
    recipientUserId: Option[String],
    markdown: String
)

private final case class Appended(channel: String, ts: String, markdown: String)

private final case class Stopped(channel: String, ts: String)

private final class FakeSlackApi(
    failAppend: Boolean = false,
    failStart: Boolean = false,
    failSend: Boolean = false
) extends SlackApi:
  private val messages = UnboundedChannel[Message]()
  private val sentMessages = ListBuffer.empty[Sent]
  private val startedStreams = ListBuffer.empty[Started]
  private val appendedChunks = ListBuffer.empty[Appended]
  private val stoppedStreams = ListBuffer.empty[Stopped]

  def sent: Seq[Sent] = sentMessages.toList
  def started: Seq[Started] = startedStreams.toList
  def appended: Seq[Appended] = appendedChunks.toList
  def stopped: Seq[Stopped] = stoppedStreams.toList

  def sendMessage(
      channel: String,
      text: String,
      threadTs: Option[String]
  ): String =
    if failSend then throw new RuntimeException("send boom")
    sentMessages += Sent(channel, text, threadTs)
    "ts"

  def startStream(
      channel: String,
      threadTs: String,
      recipientUserId: Option[String],
      markdown: String
  ): String =
    if failStart then throw new RuntimeException("start boom")
    startedStreams += Started(channel, threadTs, recipientUserId, markdown)
    "ts"

  def appendStream(channel: String, ts: String, markdown: String): Unit =
    if failAppend then throw new RuntimeException("append boom")
    appendedChunks += Appended(channel, ts, markdown)

  def stopStream(channel: String, ts: String): Unit =
    stoppedStreams += Stopped(channel, ts)

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

  def startStream(
      channel: String,
      threadTs: String,
      recipientUserId: Option[String],
      markdown: String
  ): String =
    throw new RuntimeException(s"slack api is down: $channel")

  def appendStream(channel: String, ts: String, markdown: String): Unit =
    throw new RuntimeException(s"slack api is down: $channel")

  def stopStream(channel: String, ts: String): Unit =
    throw new RuntimeException(s"slack api is down: $channel")

  def readHistory(channel: String, limit: Int): List[Message] = Nil

  def getChannel(id: String): Channel =
    Channel(id, id, "", "", isPrivate = false, isIm = false, isArchived = false)

  def getUser(id: String): User =
    User(id, id, id, id)

  def messageChannel: ReadableChannel[Message] = messages.asReadable

  def shutdown(): Unit = messages.close()
