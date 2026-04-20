package capybaraclaw.gateway

import capybaraclaw.agent.ClawAgent
import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given
import tacit.agents.llm.endpoint.{
  Endpoint,
  LLMConfig,
  LLMError,
  Message,
  Content,
  ChatResponse,
  FinishReason,
  Role,
  StreamEvent,
}
import tacit.agents.utils.Result
import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

// --- Test doubles ---

/** Scripted LLM endpoint: on each `stream` call returns the next response from the
  * list as a single `StreamEvent.Done`. If responses are exhausted, returns an error.
  */
class StubEndpoint(responses: List[ChatResponse]) extends Endpoint:
  private var idx = 0

  def invoke(messages: List[Message], config: LLMConfig): Result[ChatResponse, LLMError] =
    if idx < responses.length then
      val r = responses(idx); idx += 1; Right(r)
    else Left(LLMError("No more stub responses"))

  def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
    val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
    if idx < responses.length then
      val r = responses(idx); idx += 1
      ch.sendImmediately(Right(StreamEvent.Done(r)))
    else
      ch.sendImmediately(Left(LLMError("No more stub responses")))
    ch.asReadable

/** In-memory Port that lets tests push inbound messages and capture outbound replies. */
class FakePort(override val id: String) extends Port:
  private val inCh = UnboundedChannel[GatewayMessage]()
  private val sentReplies = LinkedBlockingQueue[(ContextKey, String)]()

  def incoming: ReadableChannel[GatewayMessage] = inCh.asReadable

  def send(key: ContextKey, text: String): Unit =
    sentReplies.put((key, text))

  def shutdown(): Unit =
    try inCh.close() catch case _: Throwable => ()

  def push(msg: GatewayMessage): Unit =
    inCh.sendImmediately(msg)

  def nextReply(timeoutMs: Long = 3000): (ContextKey, String) =
    val got = sentReplies.poll(timeoutMs, TimeUnit.MILLISECONDS)
    if got == null then throw new AssertionError(s"No reply within ${timeoutMs}ms")
    got

/** In-memory `ContextProvider` for assertions on the persisted transcript order. */
class FakeContextProvider(
  seeds: Map[ContextKey, List[Message]] = Map.empty,
) extends ContextProvider:
  private val store = scala.collection.concurrent.TrieMap[ContextKey, List[Message]]()
  private val appendLog = ConcurrentLinkedQueue[(ContextKey, Message)]()
  seeds.foreach { case (k, v) => store.update(k, v) }

  def load(key: ContextKey): List[Message] = store.getOrElse(key, Nil)

  def append(key: ContextKey, msg: Message): Unit =
    store.updateWith(key) {
      case Some(xs) => Some(xs :+ msg)
      case None     => Some(List(msg))
    }
    appendLog.offer((key, msg))

  def log: List[(ContextKey, Message)] = appendLog.iterator.asScala.toList

// --- Helpers ---

def textResponse(text: String): ChatResponse =
  ChatResponse(Message.assistant(text), FinishReason.Stop)

// --- Tests ---

class GatewaySuite extends munit.FunSuite:

  private def workDir: String = java.io.File(".").getCanonicalFile.getPath

  private def runGateway(
    ports: List[Port],
    cp: ContextProvider,
    endpointFactory: () => Endpoint,
    created: AtomicInteger,
    historySeen: ConcurrentLinkedQueue[List[Message]] = ConcurrentLinkedQueue(),
  )(body: Async.Spawn ?=> Gateway => Unit): Unit =
    val factory: (String, List[Message]) => ClawAgent = (wd, hist) =>
      created.incrementAndGet()
      historySeen.offer(hist)
      ClawAgent(wd, initialMessages = hist, endpointOverride = Some(endpointFactory()))

    Async.blocking:
      val gateway = Gateway(workDir, ports, cp, factory)
      val gwFut = Future(gateway.run())
      try body(gateway)
      finally
        gateway.shutdown()
        gwFut.awaitResult

  test("routes same (port, thread) to one runner across multiple users"):
    val cp = FakeContextProvider()
    val port = FakePort("slack")
    val created = AtomicInteger(0)
    runGateway(
      List(port),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("hi"), textResponse("yes"))),
      created = created,
    ) { _ =>
      val origin1 = Origin("slack", "C1", "U_alice")
      val origin2 = Origin("slack", "C1", "U_bob")
      port.push(GatewayMessage(origin1, "ping"))
      val reply1 = port.nextReply()
      port.push(GatewayMessage(origin2, "pong"))
      val reply2 = port.nextReply()

      assertEquals(created.get, 1, "Only one ClawAgent should be created for one thread")
      assertEquals(reply1, (ContextKey("slack", "C1"), "hi"))
      assertEquals(reply2, (ContextKey("slack", "C1"), "yes"))

      val key = ContextKey("slack", "C1")
      val persisted = cp.log.collect { case (k, m) if k == key => m }
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

  test("distinct threads spawn distinct runners"):
    val cp = FakeContextProvider()
    val port = FakePort("slack")
    val created = AtomicInteger(0)
    runGateway(
      List(port),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("a"))),
      created = created,
    ) { _ =>
      port.push(GatewayMessage(Origin("slack", "C1", "U1"), "m1"))
      port.nextReply()
      port.push(GatewayMessage(Origin("slack", "C2", "U1"), "m2"))
      port.nextReply()

      assertEquals(created.get, 2, "One runner per thread")
    }

  test("cold start rehydrates prior history into ClawAgent"):
    val priorKey = ContextKey("slack", "C1")
    val seed = List(
      Message.user("[U_old] what was yesterday?"),
      Message.assistant("Tuesday."),
    )
    val cp = FakeContextProvider(seeds = Map(priorKey -> seed))
    val port = FakePort("slack")
    val created = AtomicInteger(0)
    val seenHistory = ConcurrentLinkedQueue[List[Message]]()
    runGateway(
      List(port),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("ok"))),
      created = created,
      historySeen = seenHistory,
    ) { _ =>
      port.push(GatewayMessage(Origin("slack", "C1", "U1"), "and today?"))
      port.nextReply()

      assertEquals(created.get, 1)
      val hist = seenHistory.iterator.asScala.toList.head
      assertEquals(hist.map(_.text), List("[U_old] what was yesterday?", "Tuesday."))
    }

  test("rejects messages whose origin.port does not match the sending port id"):
    val cp = FakeContextProvider()
    val port = FakePort("slack")
    val created = AtomicInteger(0)
    runGateway(
      List(port),
      cp,
      endpointFactory = () => StubEndpoint(List(textResponse("ok"))),
      created = created,
    ) { _ =>
      // Push a GatewayMessage whose origin claims a different port; Gateway should
      // drop it instead of creating a runner.
      port.push(GatewayMessage(Origin("other", "T1", "U1"), "bogus"))
      // Give the reader a moment; no runner should be spawned, no reply sent.
      Thread.sleep(200)
      assertEquals(created.get, 0)
    }
