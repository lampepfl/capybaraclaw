package tacit.library.privatechannel

import java.net.http.HttpRequest
import java.nio.ByteBuffer
import java.util.concurrent.Flow
import java.util.concurrent.atomic.AtomicReference

import io.circe.parser.parse

import tacit.library.{ClassifiedImpl, InterfaceImpl}
import PrivateChannel.*

/** The host's permission oracle, as capybaraclaw's would answer. One per JVM. */
object TestOracle:
  val answer = AtomicReference("""{"allow":true}""")
  val lastRequest = AtomicReference("")
  InterfaceImpl.installPermissionOracle: request =>
    lastRequest.set(request)
    answer.get

class PrivateChannelSuite extends munit.FunSuite:

  private val oracle = TestOracle

  private val config = Config.fromEnv(
    Map(
      "SLACK_BOT_TOKEN" -> "xoxb-test",
      "CLAW_PRIVATE_SLACK_CHANNEL" -> "C123",
      "CLAW_PRIVATE_SLACK_CHANNEL_NAME" -> "#hr-private",
      "CLAW_PRIVATE_LLM_MODEL" -> "private-model",
      "CLAW_PRIVATE_LLM_API_KEY" -> "sk-test"
    ).get
  )

  /** The body a request would send. */
  private def body(request: HttpRequest): String =
    val chunks = scala.collection.mutable.ListBuffer.empty[Array[Byte]]
    request
      .bodyPublisher()
      .get()
      .subscribe(new Flow.Subscriber[ByteBuffer]:
        def onSubscribe(s: Flow.Subscription): Unit = s.request(Long.MaxValue)
        def onNext(b: ByteBuffer): Unit =
          val bytes = new Array[Byte](b.remaining)
          b.get(bytes)
          chunks += bytes
        def onError(t: Throwable): Unit = throw t
        def onComplete(): Unit = ())
    String(chunks.flatten.toArray, "UTF-8")

  /** Fake Slack and model: records requests, answers as configured. */
  private final class Fake(
      model: (Int, String) = modelOk,
      slack: (Int, String) = slackOk
  ):
    val sent = scala.collection.mutable.ListBuffer.empty[(String, String)]
    val http: Http = request =>
      val url = request.uri.toString
      sent += url -> body(request)
      if url.endsWith("/chat/completions") then model else slack
    def slackBody = sent.collectFirst {
      case (url, b) if url.endsWith("chat.postMessage") =>
        parse(b).toOption.get.hcursor
    }.get
    def slackText: String = slackBody.get[String]("text").toOption.get

  private val modelOk =
    200 -> """{"choices":[{"message":{"content":"Bob <@U1> is underpaid & see https://x.example"}}]}"""
  private val slackOk = 200 -> """{"ok":true}"""

  /** Delivers synchronously, so the fake has seen everything when it returns. */
  private def send(request: Request, fake: Fake, cfg: Config = config): String =
    deliver(cfg, request, fake.http, run = f => f())

  /** Agent-defined data whose `toString` throws. */
  private final class Hostile(e: Throwable):
    override def toString: String = throw e

  test(
    "ask: the model gets question and data, the channel gets the answer in a code block"
  ):
    val fake = Fake()
    val reply = send(
      Request.Ask("Who is underpaid?", ClassifiedImpl.wrap("Bob,90000")),
      fake
    )
    assert(reply.startsWith("Sent to #hr-private"), reply)
    val (modelUrl, modelBody) = fake.sent.head
    assertEquals(modelUrl, "http://localhost:11434/v1/chat/completions")
    assert(
      modelBody.contains("private-model") && modelBody.contains(
        "Bob,90000"
      ) && modelBody.contains("Who is underpaid?")
    )
    assertEquals(
      fake.slackText,
      ":lock: *Confidential answer from capybara*\n*Question:*\n```\nWho is underpaid?\n```\n*Answer:*\n```\nBob &lt;@U1&gt; is underpaid &amp; see https://x.example\n```"
    )
    assertEquals(fake.slackBody.get[String]("channel").toOption, Some("C123"))
    assertEquals(
      fake.slackBody.get[Boolean]("unfurl_links").toOption,
      Some(false)
    )
    assertEquals(
      fake.slackBody.get[Boolean]("unfurl_media").toOption,
      Some(false)
    )

  test("the user is asked for the plugin permission, naming the channel"):
    send(Request.Post(ClassifiedImpl.wrap("x")), Fake())
    val request = parse(oracle.lastRequest.get).toOption.get.hcursor
    assertEquals(request.get[String]("plugin").toOption, Some(PluginId))
    assertEquals(request.get[String]("permission").toOption, Some(Permission))
    assertEquals(
      request.get[List[String]]("items").toOption,
      Some(List("#hr-private"))
    )

  test("until the user agrees, nothing is sent"):
    oracle.answer.set(
      """{"allow":false,"message":"permission request #1 is waiting"}"""
    )
    try
      val fake = Fake()
      val e = intercept[SecurityException](
        send(Request.Post(ClassifiedImpl.wrap("x")), fake)
      )
      assert(e.getMessage.contains("#1"))
      assert(fake.sent.isEmpty)
    finally oracle.answer.set("""{"allow":true}""")

  test(
    "the reply to the agent does not depend on the data, whatever the data does"
  ):
    def replies(
        data: Any,
        model: (Int, String) = modelOk,
        slack: (Int, String) = slackOk
    ) =
      val c = data match
        case e: Throwable => ClassifiedImpl.fromTry(scala.util.Failure(e))
        case v            => ClassifiedImpl.wrap(v)
      List(
        send(Request.Ask("q", c), Fake(model, slack)),
        send(Request.Post(c), Fake(model, slack))
      )
    val all = List(
      replies("secret-a"),
      replies("secret-b"),
      replies(RuntimeException("secret-c")),
      replies("secret-d", model = 500 -> "boom secret-d"),
      replies(
        "secret-e",
        slack = 200 -> """{"ok":false,"error":"msg_too_long"}"""
      ),
      replies(Hostile(InterruptedException("secret-f"))),
      replies(Hostile(StackOverflowError("secret-g"))),
      replies(new RuntimeException:
        override def getMessage: String =
          throw InterruptedException("secret-h"))
    ).flatten.toSet
    assertEquals(all.size, 1, all)
    assert(!all.head.contains("secret"))

  test("failures are reported in the channel instead"):
    val fake = Fake(model = 500 -> "model down")
    send(Request.Ask("q", ClassifiedImpl.wrap("x")), fake)
    assert(
      fake.slackText.contains("failed") && fake.slackText.contains("HTTP 500"),
      fake.slackText
    )

  test("post sends the value as it is, without calling a model"):
    val fake = Fake()
    send(Request.Post(ClassifiedImpl.wrap("a & b")), fake)
    assertEquals(fake.sent.size, 1)
    assertEquals(
      fake.slackText,
      ":lock: *Confidential result from capybara*\n```\na &amp; b\n```"
    )

  test("no run of backticks in the data can close the block"):
    List(3, 5, 6, 9).foreach: n =>
      val fake = Fake()
      send(
        Request.Post(ClassifiedImpl.wrap("x" + "`" * n + "<https://evil|y>")),
        fake
      )
      assertEquals(fake.slackText.split("```", -1).length, 3, fake.slackText)

  test("content is cut after escaping, without breaking an entity"):
    val fake = Fake()
    send(Request.Post(ClassifiedImpl.wrap("&" * 100000)), fake)
    val text = fake.slackText
    assert(text.length < 40000, text.length)
    assert(text.contains("&amp;\n... (truncated)"), text.takeRight(40))

  test("a blank setting counts as unset"):
    val blankName = Config.fromEnv(
      Map(
        "SLACK_BOT_TOKEN" -> "xoxb-test",
        "CLAW_PRIVATE_SLACK_CHANNEL" -> "C123",
        "CLAW_PRIVATE_SLACK_CHANNEL_NAME" -> "",
        "CLAW_PRIVATE_LLM_MODEL" -> " "
      ).get
    )
    assertEquals(blankName.channelLabel, "C123")
    assertEquals(blankName.llm, None)

  test("missing settings fail before the user is asked"):
    intercept[IllegalStateException](
      Config.fromEnv(Map.empty[String, String].get)
    )
    oracle.lastRequest.set("")
    val e = intercept[IllegalStateException](
      send(
        Request.Ask("q", ClassifiedImpl.wrap("x")),
        Fake(),
        config.copy(llm = None)
      )
    )
    assert(e.getMessage.contains("CLAW_PRIVATE_LLM_MODEL"))
    assertEquals(oracle.lastRequest.get, "")
