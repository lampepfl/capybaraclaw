package capybaraclaw.gateway.port.slack

import capybaraclaw.gateway.{
  ApprovalDecision,
  ApprovalReply,
  ApprovalRequest,
  ApprovalResolution,
  Conversation,
  GatewayMessage,
  Permission,
  Origin,
  PortId,
  SessionHandle,
  SessionId,
  SessionRef,
  UserId
}
import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given
import scala.collection.mutable.ListBuffer

class SlackPortSuite extends munit.FunSuite:

  private def receive(bot: FakeSlackApi, port: SlackPort, msg: Message) =
    Async.blocking:
      port.start()
      bot.push(msg)
      val origin = port.incoming.read().toOption.get.origin
      bot.shutdown()
      origin

  test("a DM is a direct conversation"):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = receive(
      bot,
      port,
      Message("U1", "hi", "1.0", None, MessageOrigin.DirectMessage("D1"))
    )
    assertEquals(port.conversation(origin), Conversation.Direct)

  test("a channel or group DM is shared"):
    List(
      MessageOrigin.ChannelMessage("C1"),
      MessageOrigin.GroupMessage("C1")
    ).foreach: kind =>
      val bot = FakeSlackApi()
      val port = SlackPort(bot)
      val origin = receive(bot, port, Message("U1", "hi", "1.0", None, kind))
      assertEquals(port.conversation(origin), Conversation.Group("C1"))

  test("a channel never seen in a DM is shared"):
    val port = SlackPort(FakeSlackApi())
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "D9/1.0"))
    assertEquals(port.conversation(origin), Conversation.Group("D9"))

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

  private val sessionId = SessionId.random()

  test(
    "a message opening a channel thread gets a reaction, removed after the turn"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    Async.blocking:
      val _ = port.start()
      bot.push(
        Message("U1", "hi", "1.1", None, MessageOrigin.ChannelMessage("C123"))
      )
      val GatewayMessage(origin, _) =
        port.incoming.read().toOption.get: @unchecked

      val _ = port.openReply(sessionId, origin)
      port.onTurnFinished(sessionId, origin)

      assertEquals(
        bot.calls.toList,
        List(
          s"react C123 1.1 ${SlackPort.ThinkingReaction}",
          s"unreact C123 1.1 ${SlackPort.ThinkingReaction}"
        )
      )
      port.shutdown()

  test(
    "a message in an existing channel thread gets a reaction"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    Async.blocking:
      val _ = port.start()
      bot.push(
        Message(
          "U1",
          "hi",
          "2.2",
          Some("1.1"),
          MessageOrigin.ChannelMessage("C123")
        )
      )
      val GatewayMessage(origin, _) =
        port.incoming.read().toOption.get: @unchecked

      val _ = port.openReply(sessionId, origin)
      port.onTurnFinished(sessionId, origin)

      assertEquals(
        bot.calls.toList,
        List(
          s"react C123 2.2 ${SlackPort.ThinkingReaction}",
          s"unreact C123 2.2 ${SlackPort.ThinkingReaction}"
        )
      )
      port.shutdown()

  private val request = ApprovalRequest(
    7,
    sessionId,
    "slack:U1",
    Permission.Commands(Set("git")),
    "to show the log"
  )

  test(
    "a permission request is posted in the thread with its question and reason"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/1.1"))

    port.requestApproval(sessionId, origin, request)

    assertEquals(
      bot.prompts.toList,
      List(
        (
          "C123",
          Some("1.1"),
          ApprovalPrompt(
            7,
            Permission.Commands(Set("git")).question(),
            Some("capybara's reason: \"to show the log\"")
          )
        )
      )
    )

  test(
    "the requester's click answers the request; the gateway's verdict closes the prompt"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/1.1"))
    Async.blocking:
      val _ = port.start()
      port.requestApproval(sessionId, origin, request)
      bot.click(
        ApprovalClick(
          "U1",
          "C123",
          "prompt-7",
          Some("1.1"),
          7,
          ApprovalDecision.Approve
        )
      )

      assertEquals(
        port.incoming.read().toOption,
        Some(ApprovalReply(origin, Some(7), ApprovalDecision.Approve))
      )
      assertEquals(
        bot.closed.toList,
        Nil,
        "not closed before the gateway decides"
      )
      port.approvalResolved(
        sessionId,
        ApprovalResolution.Answered(request, ApprovalDecision.Approve),
        origin
      )
      assertEquals(
        bot.closed.toList,
        List(
          ("prompt-7", ":white_check_mark: Allowed for this session by <@U1>")
        )
      )
      port.shutdown()

  private def clickAndRefuse(
      stillPending: Boolean
  ): (FakeSlackApi, SlackPort, Origin) =
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/1.1"))
    Async.blocking:
      val _ = port.start()
      port.requestApproval(sessionId, origin, request)
      bot.click(
        ApprovalClick(
          "U1",
          "C123",
          "prompt-7",
          Some("1.1"),
          7,
          ApprovalDecision.Approve
        )
      )
      val _ = port.incoming.read()
      port.approvalRejected(
        sessionId,
        Some(7),
        origin,
        "not yours",
        stillPending
      )
    (bot, port, origin)

  test("a refused click on a request still pending offers the prompt again"):
    val (bot, port, origin) = clickAndRefuse(stillPending = true)
    assertEquals(bot.ephemerals.toList, List(("U1", "not yours")))
    assertEquals(bot.closed.toList, Nil)
    Async.blocking:
      val _ = port.start()
      bot.click(
        ApprovalClick(
          "U1",
          "C123",
          "prompt-7",
          Some("1.1"),
          7,
          ApprovalDecision.Deny
        )
      )
      assertEquals(
        port.incoming.read().toOption,
        Some(ApprovalReply(origin, Some(7), ApprovalDecision.Deny)),
        "the prompt answers again"
      )
      port.shutdown()

  test("a refused click on a request no longer pending closes the prompt"):
    val (bot, port, _) = clickAndRefuse(stillPending = false)
    assertEquals(bot.ephemerals.toList, List(("U1", "not yours")))
    assertEquals(
      bot.closed.toList,
      List(("prompt-7", ":no_entry: No longer pending"))
    )
    assertEquals(bot.sent.toList, Nil, "nothing is posted in the thread")
    port.shutdown()

  test("a withdrawn request closes its prompt as withdrawn"):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/1.1"))
    Async.blocking:
      val _ = port.start()
      port.requestApproval(sessionId, origin, request)
      bot.click(
        ApprovalClick(
          "U1",
          "C123",
          "prompt-7",
          Some("1.1"),
          7,
          ApprovalDecision.Approve
        )
      )
      val _ = port.incoming.read()
      port.approvalResolved(
        sessionId,
        ApprovalResolution
          .Withdrawn(request, "the roles of x no longer allow it"),
        origin
      )
      assertEquals(
        bot.closed.toList,
        List(
          (
            "prompt-7",
            ":no_entry: Withdrawn: the roles of x no longer allow it"
          )
        )
      )
      port.shutdown()

  test("someone else's click is refused privately and answers nothing"):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/1.1"))
    Async.blocking:
      val _ = port.start()
      port.requestApproval(sessionId, origin, request)
      bot.click(
        ApprovalClick(
          "U2",
          "C123",
          "prompt-7",
          Some("1.1"),
          7,
          ApprovalDecision.Approve
        )
      )
      bot.click(
        ApprovalClick(
          "U1",
          "C123",
          "prompt-7",
          Some("1.1"),
          7,
          ApprovalDecision.Deny
        )
      )

      assertEquals(
        port.incoming.read().toOption,
        Some(ApprovalReply(origin, Some(7), ApprovalDecision.Deny)),
        "only the requester's later click reaches the gateway"
      )
      assertEquals(
        bot.ephemerals.toList,
        List(("U2", "Only <@U1> can answer permission request #7."))
      )
      port.shutdown()

  test("a DM message gets a reaction"):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    Async.blocking:
      val _ = port.start()
      bot.push(
        Message("U1", "hi", "1.1", None, MessageOrigin.DirectMessage("D123"))
      )
      val GatewayMessage(origin, _) =
        port.incoming.read().toOption.get: @unchecked
      val _ = port.openReply(sessionId, origin)
      assertEquals(
        bot.calls.toList,
        List(s"react D123 1.1 ${SlackPort.ThinkingReaction}")
      )
      port.shutdown()

  test(
    "a click on an old prompt does not answer a newer request with the same id"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "C123/1.1"))
    Async.blocking:
      val _ = port.start()
      port.requestApproval(sessionId, origin, request)
      bot.click(
        ApprovalClick(
          "U1",
          "C999",
          "old-prompt",
          Some("1.1"),
          7,
          ApprovalDecision.Approve
        )
      )
      bot.click(
        ApprovalClick(
          "U1",
          "C123",
          "prompt-7",
          Some("1.1"),
          7,
          ApprovalDecision.Deny
        )
      )

      assertEquals(
        port.incoming.read().toOption,
        Some(ApprovalReply(origin, Some(7), ApprovalDecision.Deny))
      )
      assertEquals(
        bot.ephemerals.toList,
        List(("U1", "This permission request is no longer pending."))
      )
      port.shutdown()

  test(
    "the turn an answered prompt starts reacts to the prompt"
  ):
    val bot = FakeSlackApi()
    val port = SlackPort(bot)
    val origin = slackOrigin(SessionHandle(SlackPort.Id, "D123/1.1"))
    Async.blocking:
      val _ = port.start()
      port.requestApproval(sessionId, origin, request)
      bot.click(
        ApprovalClick(
          "U1",
          "D123",
          "prompt-7",
          Some("1.1"),
          7,
          ApprovalDecision.Approve
        )
      )
      val _ = port.incoming.read()
      val _ = port.openReply(sessionId, origin)
      assertEquals(
        bot.calls.toList,
        List(s"react D123 prompt-7 ${SlackPort.ThinkingReaction}")
      )
      port.shutdown()

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
  private val clicks = UnboundedChannel[ApprovalClick]()
  val calls: ListBuffer[String] = ListBuffer.empty
  val prompts: ListBuffer[(String, Option[String], ApprovalPrompt)] =
    ListBuffer.empty
  val closed: ListBuffer[(String, String)] = ListBuffer.empty
  val ephemerals: ListBuffer[(String, String)] = ListBuffer.empty

  def click(click: ApprovalClick): Unit = clicks.sendImmediately(click)
  def push(message: Message): Unit = messages.sendImmediately(message)

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

  def addReaction(channel: String, ts: String, name: String): Unit =
    calls += s"react $channel $ts $name"

  def removeReaction(channel: String, ts: String, name: String): Unit =
    calls += s"unreact $channel $ts $name"

  def postApprovalPrompt(
      channel: String,
      threadTs: Option[String],
      prompt: ApprovalPrompt
  ): String =
    prompts += ((channel, threadTs, prompt))
    s"prompt-${prompt.requestId}"

  def closeApprovalPrompt(
      channel: String,
      ts: String,
      prompt: ApprovalPrompt,
      outcome: String
  ): Unit =
    closed += ((ts, outcome))

  def postEphemeral(
      channel: String,
      threadTs: Option[String],
      userId: String,
      text: String
  ): Unit =
    ephemerals += ((userId, text))

  def messageChannel: ReadableChannel[Message] = messages.asReadable
  def approvalClicks: ReadableChannel[ApprovalClick] = clicks.asReadable

  def shutdown(): Unit =
    messages.close()
    clicks.close()

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

  def addReaction(channel: String, ts: String, name: String): Unit =
    throw new RuntimeException(s"slack api is down: $channel")

  def removeReaction(channel: String, ts: String, name: String): Unit =
    throw new RuntimeException(s"slack api is down: $channel")

  def postApprovalPrompt(
      channel: String,
      threadTs: Option[String],
      prompt: ApprovalPrompt
  ): String =
    throw new RuntimeException(s"slack api is down: $channel")

  def closeApprovalPrompt(
      channel: String,
      ts: String,
      prompt: ApprovalPrompt,
      outcome: String
  ): Unit =
    throw new RuntimeException(s"slack api is down: $channel")

  def postEphemeral(
      channel: String,
      threadTs: Option[String],
      userId: String,
      text: String
  ): Unit =
    throw new RuntimeException(s"slack api is down: $channel")

  def messageChannel: ReadableChannel[Message] = messages.asReadable
  private val clicks = UnboundedChannel[ApprovalClick]()
  def approvalClicks: ReadableChannel[ApprovalClick] = clicks.asReadable

  def shutdown(): Unit =
    messages.close()
    clicks.close()
