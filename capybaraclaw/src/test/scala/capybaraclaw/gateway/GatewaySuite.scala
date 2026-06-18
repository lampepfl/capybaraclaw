package capybaraclaw.gateway

import capybaraclaw.agent.ClawAgent
import capybaraclaw.gateway.port.Port
import capybaraclaw.gateway.port.cli.CliPort
import capybaraclaw.gateway.port.slack.SlackPort
import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given
import tacit.agents.llm.endpoint.{
  Endpoint,
  LLMConfig,
  LLMError,
  Message,
  ChatResponse,
  FinishReason,
  Role,
  StreamEvent
}
import tacit.agents.utils.Result
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.UUID
import java.util.concurrent.{
  ConcurrentLinkedQueue,
  LinkedBlockingQueue,
  TimeUnit
}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

// --- Test doubles ---

object FakePort:
  final case class Reply(
      sessionId: SessionId,
      handle: Option[SessionHandle],
      text: String
  )
  final case class Rejection(origin: Origin, text: String)

  private def replyHandle(origin: Origin): Option[SessionHandle] =
    origin.session match
      case SessionRef.External(handle) => Some(handle)
      case SessionRef.Direct(_)        => None

  /** Longer waits when `CAPYBARACLAW_CI` is `1` or `true` (e.g. GitHub Actions). */
  def defaultReplyTimeoutMs: Long =
    sys.env.get("CAPYBARACLAW_CI") match
      case Some(v) if v == "1" || v.equalsIgnoreCase("true") => 60_000L
      case _                                                 => 5_000L

/** Scripted LLM endpoint: on each `stream` call returns the next response from the
  * list as a single `StreamEvent.Done`. If responses are exhausted, returns an error.
  */
class StubEndpoint(responses: List[ChatResponse]) extends Endpoint:
  private var idx = 0

  def invoke(
      messages: List[Message],
      config: LLMConfig
  ): Result[ChatResponse, LLMError] =
    if idx < responses.length then
      val r = responses(idx); idx += 1; Right(r)
    else Left(LLMError("No more stub responses"))

  def stream(messages: List[Message], config: LLMConfig)(using
      Async.Spawn
  ): ReadableChannel[Result[StreamEvent, LLMError]] =
    val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
    if idx < responses.length then
      val r = responses(idx); idx += 1
      ch.sendImmediately(Right(StreamEvent.Done(r)))
    else ch.sendImmediately(Left(LLMError("No more stub responses")))
    ch.asReadable

class RejectingPort(override val id: PortId) extends FakePort(id):
  override def validateOriginForReply(origin: Origin): Unit =
    throw IllegalArgumentException("invalid reply origin")

/** In-memory Port that lets tests push inbound messages and capture outbound replies. */
class FakePort(override val id: PortId) extends Port:
  private val inCh = UnboundedChannel[GatewayMessage]()
  private val sentReplies = LinkedBlockingQueue[FakePort.Reply]()
  private val finishedTurns = LinkedBlockingQueue[SessionId]()
  private val rejectedInbound = LinkedBlockingQueue[FakePort.Rejection]()

  def incoming: ReadableChannel[GatewayMessage] = inCh.asReadable

  def send(sessionId: SessionId, origin: Origin, text: String): Unit =
    sentReplies.put(
      FakePort.Reply(sessionId, FakePort.replyHandle(origin), text)
    )

  override def onTurnFinished(sessionId: SessionId, origin: Origin): Unit =
    finishedTurns.put(sessionId)

  override def rejectInbound(origin: Origin, text: String): Unit =
    rejectedInbound.put(FakePort.Rejection(origin, text))
    origin.session match
      case SessionRef.Direct(sessionId) =>
        try sendError(sessionId, origin, text)
        finally onTurnFinished(sessionId, origin)
      case SessionRef.External(_) =>
        ()

  def shutdown(): Unit =
    try inCh.close()
    catch case _: Throwable => ()

  def push(msg: GatewayMessage): Unit =
    inCh.sendImmediately(msg)

  def nextReply(
      timeoutMs: Long = FakePort.defaultReplyTimeoutMs
  ): FakePort.Reply =
    val got = sentReplies.poll(timeoutMs, TimeUnit.MILLISECONDS)
    if got == null then
      throw new AssertionError(s"No reply within ${timeoutMs}ms")
    got

  def nextFinished(
      timeoutMs: Long = FakePort.defaultReplyTimeoutMs
  ): SessionId =
    val got = finishedTurns.poll(timeoutMs, TimeUnit.MILLISECONDS)
    if got == null then
      throw new AssertionError(s"No turn finish within ${timeoutMs}ms")
    got

  def nextRejection(
      timeoutMs: Long = FakePort.defaultReplyTimeoutMs
  ): FakePort.Rejection =
    val got = rejectedInbound.poll(timeoutMs, TimeUnit.MILLISECONDS)
    if got == null then
      throw new AssertionError(s"No rejection within ${timeoutMs}ms")
    got

/** In-memory `ContextProvider` for assertions on the persisted transcript order. */
class FakeContextProvider(
    seeds: Map[SessionId, List[Message]] = Map.empty
) extends ContextProvider:
  private val store =
    scala.collection.concurrent.TrieMap[SessionId, List[Message]]()
  private val sessions =
    scala.collection.concurrent.TrieMap[SessionId, SessionMetadata]()
  private val handles =
    scala.collection.concurrent.TrieMap[
      (String, SessionHandle),
      SessionId
    ]()
  private val appendLog = ConcurrentLinkedQueue[(SessionId, Message)]()
  private val lock = new Object
  seeds.foreach { case (k, v) =>
    store.update(k, v)
    sessions.update(k, metadata(k, "."))
  }

  def createSession(workdir: String): SessionId =
    lock.synchronized:
      val sessionId = SessionId.random()
      sessions.update(sessionId, metadata(sessionId, workdir))
      sessionId

  def verifyAndTouchSession(
      id: SessionId,
      expectedWorkdir: String
  ): Option[SessionMetadata] =
    lock.synchronized:
      sessions
        .get(id)
        .map: m =>
          if m.workdir == expectedWorkdir then
            sessions.update(id, m.copy(lastActivity = Instant.now))
          m

  def resolveOrCreateHandle(
      workdir: String,
      handle: SessionHandle
  ): SessionId =
    lock.synchronized:
      handles.get((workdir, handle)) match
        case Some(sessionId) =>
          sessions
            .get(sessionId)
            .foreach: m =>
              sessions.update(sessionId, m.copy(lastActivity = Instant.now))
          sessionId
        case None =>
          val sessionId = deterministicSessionId(workdir, handle)
          sessions.update(sessionId, metadata(sessionId, workdir))
          handles.update((workdir, handle), sessionId)
          sessionId

  def findSession(id: SessionId): Option[SessionMetadata] =
    lock.synchronized:
      sessions.get(id)

  def listSessions(): List[SessionMetadata] =
    lock.synchronized:
      sessions.values.toList.sortBy(_.lastActivity).reverse

  def load(sessionId: SessionId): List[Message] =
    store.getOrElse(sessionId, Nil)

  def append(sessionId: SessionId, msg: Message): Unit =
    store.updateWith(sessionId) {
      case Some(xs) => Some(xs :+ msg)
      case None     => Some(List(msg))
    }
    appendLog.offer((sessionId, msg))

  def log: List[(SessionId, Message)] = appendLog.iterator.asScala.toList

  private def metadata(
      sessionId: SessionId,
      workdir: String
  ): SessionMetadata =
    val now = Instant.now
    SessionMetadata(sessionId, workdir, now, now)

  private def deterministicSessionId(
      workdir: String,
      handle: SessionHandle
  ): SessionId =
    val raw = s"$workdir\u0000${handle.kind}\u0000${handle.value}"
    SessionId(
      UUID
        .nameUUIDFromBytes(raw.getBytes(StandardCharsets.UTF_8))
        .toString
    )

// --- Helpers ---

def textResponse(text: String): ChatResponse =
  ChatResponse(Message.assistant(text), FinishReason.Stop)

// --- Tests ---

class GatewaySuite extends munit.FunSuite:

  private def workDir: String = java.io.File(".").getCanonicalFile.getPath
  private def sid(
      localId: String,
      wd: String = workDir,
      port: String = "slack"
  ): SessionId =
    val raw = s"$wd\u0000$port\u0000$localId"
    SessionId(
      UUID
        .nameUUIDFromBytes(raw.getBytes(StandardCharsets.UTF_8))
        .toString
    )
  private def handle(localId: String): SessionHandle =
    SessionHandle(SlackPort.Id, localId)
  private def externalOrigin(
      port: PortId,
      user: String,
      localId: String
  ): Origin =
    Origin(port, UserId(user), SessionRef.External(handle(localId)))
  private def directOrigin(
      port: PortId,
      user: String,
      sessionId: SessionId
  ): Origin =
    Origin(port, UserId(user), SessionRef.Direct(sessionId))

  private def runGateway(
      ports: List[Port],
      cp: ContextProvider,
      endpointFactory: () => Endpoint,
      created: AtomicInteger,
      historySeen: ConcurrentLinkedQueue[List[Message]] =
        ConcurrentLinkedQueue(),
      gatewayWorkDir: String = workDir
  )(body: Async.Spawn ?=> Gateway => Unit): Unit =
    runGatewayWithResult(
      ports,
      cp,
      endpointFactory,
      created,
      historySeen,
      gatewayWorkDir
    )(body)

  private def runGatewayWithResult[R](
      ports: List[Port],
      cp: ContextProvider,
      endpointFactory: () => Endpoint,
      created: AtomicInteger,
      historySeen: ConcurrentLinkedQueue[List[Message]],
      gatewayWorkDir: String
  )(body: Async.Spawn ?=> Gateway => R): R =
    val factory: (String, SessionId, List[Message]) => ClawAgent =
      (wd, sid, hist) =>
        created.incrementAndGet()
        historySeen.offer(hist)
        ClawAgent(
          wd,
          sid,
          SessionSearch.empty,
          initialMessages = hist,
          endpointOverride = Some(endpointFactory())
        )

    Async.blocking:
      val gateway = Gateway(gatewayWorkDir, ports, cp, factory)
      val gwFut = Future(gateway.run())
      try body(gateway)
      finally
        gateway.shutdown()
        gwFut.awaitResult

  test("routes same Slack handle to one runner across multiple users"):
    val cp = FakeContextProvider()
    val port = FakePort(SlackPort.Id)
    val created = AtomicInteger(0)
    runGateway(
      List(port),
      cp,
      endpointFactory =
        () => StubEndpoint(List(textResponse("hi"), textResponse("yes"))),
      created = created
    ) { _ =>
      val origin1 = externalOrigin(SlackPort.Id, "U_alice", "C1")
      val origin2 = externalOrigin(SlackPort.Id, "U_bob", "C1")
      port.push(GatewayMessage(origin1, "ping"))
      val reply1 = port.nextReply()
      port.push(GatewayMessage(origin2, "pong"))
      val reply2 = port.nextReply()

      assertEquals(
        created.get,
        1,
        "Only one ClawAgent should be created for one session"
      )
      assertEquals(reply1.handle, Some(handle("C1")))
      assertEquals(reply1.text, "hi")
      assertEquals(reply2.handle, Some(handle("C1")))
      assertEquals(reply2.text, "yes")
      assertEquals(reply1.sessionId, reply2.sessionId)

      val persisted = cp.log.collect {
        case (sessionId, m) if sessionId == sid("C1") => m
      }
      assertEquals(persisted.size, 4)
      assertEquals(persisted(0).role, Role.User)
      assertEquals(persisted(0).text, "[U_alice] ping")
      assertEquals(persisted(1).role, Role.Assistant)
      assertEquals(persisted(1).text, "hi")
      assertEquals(persisted(2).role, Role.User)
      assertEquals(persisted(2).text, "[U_bob] pong")
      assertEquals(persisted(3).role, Role.Assistant)
      assertEquals(persisted(3).text, "yes")
    }

  test("distinct Slack handles spawn distinct runners"):
    val cp = FakeContextProvider()
    val port = FakePort(SlackPort.Id)
    val created = AtomicInteger(0)
    runGateway(
      List(port),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("a"))),
      created = created
    ) { _ =>
      port.push(
        GatewayMessage(externalOrigin(SlackPort.Id, "U1", "C1"), "m1")
      )
      port.nextReply()
      port.push(
        GatewayMessage(externalOrigin(SlackPort.Id, "U1", "C2"), "m2")
      )
      port.nextReply()

      assertEquals(created.get, 2, "One runner per session")
    }

  test("same Slack handle in different workdirs maps to different UUIDs"):
    val cp = FakeContextProvider()
    val firstPort = FakePort(SlackPort.Id)
    val firstCreated = AtomicInteger(0)
    val firstSessionId = runGatewayWithResult(
      List(firstPort),
      cp,
      () => StubEndpoint(List(textResponse("a"))),
      firstCreated,
      ConcurrentLinkedQueue[List[Message]](),
      "/tmp/claw-one"
    ) { _ =>
      firstPort.push(
        GatewayMessage(
          externalOrigin(SlackPort.Id, "U1", "shared"),
          "m1"
        )
      )
      firstPort.nextReply().sessionId
    }

    val secondPort = FakePort(SlackPort.Id)
    val secondCreated = AtomicInteger(0)
    runGateway(
      List(secondPort),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("b"))),
      created = secondCreated,
      gatewayWorkDir = "/tmp/claw-two"
    ) { _ =>
      secondPort.push(
        GatewayMessage(
          externalOrigin(SlackPort.Id, "U1", "shared"),
          "m2"
        )
      )
      val secondSessionId = secondPort.nextReply().sessionId

      assertNotEquals(secondSessionId, firstSessionId)
    }

  test("cold start rehydrates prior history into ClawAgent"):
    val seed = List(
      Message.user("[U_old] what was yesterday?"),
      Message.assistant("Tuesday.")
    )
    val cp = FakeContextProvider(seeds = Map(sid("C1") -> seed))
    val port = FakePort(SlackPort.Id)
    val created = AtomicInteger(0)
    val seenHistory = ConcurrentLinkedQueue[List[Message]]()
    runGateway(
      List(port),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("ok"))),
      created = created,
      historySeen = seenHistory
    ) { _ =>
      port.push(
        GatewayMessage(
          externalOrigin(SlackPort.Id, "U1", "C1"),
          "and today?"
        )
      )
      port.nextReply()

      assertEquals(created.get, 1)
      val hist = seenHistory.iterator.asScala.toList.head
      assertEquals(
        hist.map(_.text),
        List("[U_old] what was yesterday?", "Tuesday.")
      )
    }

  test("validates external origins before creating a session handle"):
    val resolveAttempts = AtomicInteger(0)
    val cp = new FakeContextProvider():
      override def resolveOrCreateHandle(
          workdir: String,
          handle: SessionHandle
      ): SessionId =
        resolveAttempts.incrementAndGet()
        super.resolveOrCreateHandle(workdir, handle)

    val port = RejectingPort(SlackPort.Id)
    val created = AtomicInteger(0)
    runGateway(
      List(port),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("unused"))),
      created = created
    ) { _ =>
      port.push(
        GatewayMessage(externalOrigin(SlackPort.Id, "U1", "invalid"), "hello")
      )
      val rejection = port.nextRejection()

      assertEquals(rejection.origin.user, UserId("U1"))
      assert(rejection.text.contains("invalid reply origin"))
      assertEquals(resolveAttempts.get, 0)
      assertEquals(created.get, 0)
    }

  test("rejects messages whose origin.port does not match the sending port id"):
    val cp = FakeContextProvider()
    val port = FakePort(SlackPort.Id)
    val created = AtomicInteger(0)
    runGateway(
      List(port),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("ok"))),
      created = created
    ) { _ =>
      port.push(
        GatewayMessage(
          externalOrigin(PortId("other"), "U1", "T1"),
          "bogus"
        )
      )
      val rejection = port.nextRejection()

      assert(rejection.text.contains("does not match receiving port"))
      assertEquals(created.get, 0)
    }

  test(
    "rejects external messages whose handle kind does not match origin port"
  ):
    val resolveAttempts = AtomicInteger(0)
    val cp = new FakeContextProvider():
      override def resolveOrCreateHandle(
          workdir: String,
          handle: SessionHandle
      ): SessionId =
        resolveAttempts.incrementAndGet()
        super.resolveOrCreateHandle(workdir, handle)

    val port = FakePort(SlackPort.Id)
    val created = AtomicInteger(0)
    runGateway(
      List(port),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("unused"))),
      created = created
    ) { _ =>
      val mismatched = Origin(
        port = SlackPort.Id,
        user = UserId("U1"),
        session = SessionRef.External(SessionHandle(PortId("other"), "C1"))
      )
      port.push(GatewayMessage(mismatched, "hello"))
      val rejection = port.nextRejection()

      assert(rejection.text.contains("does not match origin port"))
      assertEquals(resolveAttempts.get, 0)
      assertEquals(created.get, 0)
    }

  test("recovers reader loop after session resolution failure"):
    val resolveAttempts = AtomicInteger(0)
    val cp = new FakeContextProvider():
      override def resolveOrCreateHandle(
          workdir: String,
          handle: SessionHandle
      ): SessionId =
        if resolveAttempts.incrementAndGet() == 1 then
          throw RuntimeException("temporary sqlite failure")
        super.resolveOrCreateHandle(workdir, handle)

    val port = FakePort(SlackPort.Id)
    val created = AtomicInteger(0)
    runGateway(
      List(port),
      cp,
      endpointFactory =
        () => StubEndpoint(List(textResponse("after recovery"))),
      created = created
    ) { _ =>
      port.push(
        GatewayMessage(externalOrigin(SlackPort.Id, "U1", "C1"), "first")
      )
      val rejection = port.nextRejection()
      assert(rejection.text.contains("temporary sqlite failure"))
      assertEquals(created.get, 0)

      port.push(
        GatewayMessage(externalOrigin(SlackPort.Id, "U1", "C1"), "second")
      )
      val reply = port.nextReply()

      assertEquals(reply.text, "after recovery")
      assertEquals(created.get, 1)
    }

  test(
    "rejects direct sessions from a different workdir and finalizes the turn"
  ):
    val cp = FakeContextProvider()
    val sessionId = cp.createSession("/tmp/other-workdir")
    val port = FakePort(CliPort.Id)
    val created = AtomicInteger(0)

    runGateway(
      List(port),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("unused"))),
      created = created,
      gatewayWorkDir = "/tmp/current-workdir"
    ) { _ =>
      port.push(
        GatewayMessage(directOrigin(CliPort.Id, "tester", sessionId), "hello")
      )

      val error = port.nextReply()
      val finished = port.nextFinished()

      assert(error.text.contains("belongs to workdir"))
      assertEquals(error.sessionId, sessionId)
      assertEquals(finished, sessionId)
      assertEquals(created.get, 0)
    }

  test("rejects missing direct sessions and finalizes the turn"):
    val cp = FakeContextProvider()
    val sessionId = SessionId.random()
    val port = FakePort(CliPort.Id)
    val created = AtomicInteger(0)

    runGateway(
      List(port),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("unused"))),
      created = created
    ) { _ =>
      port.push(
        GatewayMessage(directOrigin(CliPort.Id, "tester", sessionId), "hello")
      )

      val error = port.nextReply()
      val finished = port.nextFinished()

      assert(error.text.contains("session not found"))
      assertEquals(error.sessionId, sessionId)
      assertEquals(finished, sessionId)
      assertEquals(created.get, 0)
    }
