// The package sits under `tacit.library` so the plugin can read `Classified`
// values with the library's own `ClassifiedImpl`, which is `private[library]`.
package tacit.library.privatechannel

import language.experimental.captureChecking
import caps.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import scala.util.{Failure, Success, Try}

import io.circe.Json
import io.circe.parser.parse

import tacit.library.{
  Classified,
  ClassifiedImpl,
  IOCapability,
  PluginPermissions
}

/** Ask the private model `question` about `data` and post its answer to the
  * private channel. Neither the data nor the answer reaches the agent: it only
  * learns that the answer is on its way.
  */
@assumeSafe
def askPrivately(question: String, data: Classified[Any])(using
    IOCapability
): String =
  PrivateChannel.deliver(
    PrivateChannel.Config.fromEnv(),
    PrivateChannel.Request.Ask(question, data)
  )

/** Post `message` to the private channel as it is. */
@assumeSafe
def postPrivately(message: Classified[Any])(using IOCapability): String =
  PrivateChannel.deliver(
    PrivateChannel.Config.fromEnv(),
    PrivateChannel.Request.Post(message)
  )

/** A sink for `Classified` data that only the people in one Slack channel can
  * read. The operator picks the channel and the model through the gateway's
  * environment; agent code cannot change either.
  *
  * Nothing the agent observes may depend on the data, or it would learn a bit
  * of a secret per call: not the reply, not an exception, not how long the
  * call takes. So the call only checks the settings and the user's permission
  * (neither depends on the data) and returns; the message is built and posted
  * in the background, and whatever goes wrong there is reported in the channel
  * or not at all.
  */
private[privatechannel] object PrivateChannel:

  val PluginId = "claw.private-channel"

  /** Shown to the user. Once granted, every Classified value of the session may
    * go to the channel, so the permission says so.
    */
  val Permission =
    "post any confidential data capybara holds to the private channel"

  /** Characters per part of a message, after escaping; Slack truncates
    * messages over 40,000.
    */
  val MaxQuestion = 2000
  val MaxContent = 30000

  enum Request:
    case Ask(question: String, data: Classified[Any])
    case Post(message: Classified[Any])

  final case class Config(
      slackApi: String,
      slackToken: String,
      channel: String,
      channelLabel: String,
      llm: Option[LlmConfig]
  )

  final case class LlmConfig(
      baseUrl: String,
      model: String,
      apiKey: Option[String]
  )

  object Config:
    def fromEnv(
        environment: String => Option[String] = name =>
          Option(System.getenv(name))
    ): Config =
      // A blank value counts as unset: .env parsers turn e.g. an unquoted
      // `#hr-private` into "" (`#` starts a comment).
      def env(name: String) = environment(name).map(_.trim).filter(_.nonEmpty)
      def required(name: String) =
        env(name)
          .getOrElse(
            throw IllegalStateException(
              s"The private channel is not configured: set $name for the gateway."
            )
          )
      val channel = required("CLAW_PRIVATE_SLACK_CHANNEL")
      Config(
        slackApi = env("CLAW_PRIVATE_SLACK_API_URL")
          .getOrElse("https://slack.com/api")
          .stripSuffix("/"),
        slackToken = required("SLACK_BOT_TOKEN"),
        channel = channel,
        channelLabel =
          env("CLAW_PRIVATE_SLACK_CHANNEL_NAME").getOrElse(channel),
        llm = env("CLAW_PRIVATE_LLM_MODEL")
          .map: model =>
            LlmConfig(
              baseUrl = env("CLAW_PRIVATE_LLM_BASE_URL")
                .getOrElse("http://localhost:11434/v1")
                .stripSuffix("/"),
              model = model,
              apiKey = env("CLAW_PRIVATE_LLM_API_KEY")
            )
      )

  /** Sends a request and returns its status code and body. Java I/O is not
    * tracked by capture checking, hence a pure function type.
    */
  type Http = HttpRequest -> (Int, String)

  private lazy val client =
    HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build()

  val realHttp: Http = request =>
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    (response.statusCode, response.body)

  /** Runs the delivery off the agent's thread. Not a daemon, so a gateway that
    * shuts down right after the call still delivers. Nothing may escape it:
    * an uncaught error would be printed to the shared stderr.
    */
  def background(delivery: () -> Unit): Unit =
    val _ = Thread
      .ofPlatform()
      .daemon(false)
      .name("claw-private-channel")
      .start: () =>
        try delivery()
        catch case _: Throwable => ()

  def deliver(
      config: Config,
      request: Request,
      http: Http = realHttp,
      run: (() -> Unit) -> Unit = background
  ): String =
    val compose: () -> String = request match
      case Request.Post(data)          => () => postMessage(data)
      case Request.Ask(question, data) =>
        val llm = config.llm.getOrElse(
          throw IllegalStateException(
            "No private model is configured: set CLAW_PRIVATE_LLM_MODEL (and CLAW_PRIVATE_LLM_BASE_URL) for the gateway."
          )
        )
        () => askMessage(llm, question, data, http)
    PluginPermissions.require(PluginId, Permission, Set(config.channelLabel))
    run(() => postToSlack(config, guarded(compose()), http))
    s"Sent to ${config.channelLabel}. Its content is confidential and you cannot see it: tell the user to read it there. " +
      "If nothing appears there within a few minutes, the private channel is misconfigured."

  /** Agent code runs while a message is built: `toString` of the data and
    * `getMessage` of exceptions it threw. Whatever it throws, fatal errors
    * included, ends here.
    */
  private def guarded(text: => String): String =
    try text
    catch
      case _: Throwable =>
        ":lock: capybara sent confidential data, but it could not be shown."

  private def postMessage(data: Classified[Any]): String =
    s":lock: *Confidential result from capybara*\n${block(ClassifiedImpl.unwrap(data).map(_.toString))}"

  private def askMessage(
      llm: LlmConfig,
      question: String,
      data: Classified[Any],
      http: Http
  ): String =
    val answer = ClassifiedImpl
      .unwrap(data)
      .flatMap: d =>
        try Success(complete(llm, question, d.toString, http))
        catch case e: Throwable => Failure(e)
    s":lock: *Confidential answer from capybara*\n*Question:*\n${block(Success(question), MaxQuestion)}\n*Answer:*\n${block(answer)}"

  /** A Slack code block, where URLs are not links, so data or a model answer
    * cannot turn into something to click. `&`, `<` and `>` are escaped as
    * Slack requires everywhere, and every backtick gets a zero-width space
    * after it, so no run of them can close the block.
    */
  def block(text: Try[String], max: Int = MaxContent): String =
    val raw = text match
      case Success(t) => t
      case Failure(e) =>
        s"(failed: ${e.getClass.getName}: ${Option(e.getMessage).getOrElse("")})"
    val escaped = raw
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("`", "`\u200b")
    val shown =
      if escaped.length <= max then escaped
      else escaped.take(max).replaceFirst("&[a-z]*$", "") + "\n... (truncated)"
    s"```\n$shown\n```"

  /** An OpenAI-compatible chat completion (Ollama, OpenRouter, OpenAI, ...). */
  def complete(
      llm: LlmConfig,
      question: String,
      data: String,
      http: Http
  ): String =
    val body = Json.obj(
      "model" -> Json.fromString(llm.model),
      "messages" -> Json.arr(
        Json.obj(
          "role" -> Json.fromString("system"),
          "content" -> Json.fromString(
            "You answer questions about confidential data for the people allowed to see it. " +
              "Use only the data given. Be concise. Do not include links."
          )
        ),
        Json.obj(
          "role" -> Json.fromString("user"),
          "content" -> Json.fromString(s"Question: $question\n\nData:\n$data")
        )
      )
    )
    val request = HttpRequest
      .newBuilder(URI.create(s"${llm.baseUrl}/chat/completions"))
      .timeout(Duration.ofMinutes(3))
      .header("Content-Type", "application/json")
      .headers(
        llm.apiKey.toList.flatMap(k => List("Authorization", s"Bearer $k"))*
      )
      .POST(HttpRequest.BodyPublishers.ofString(body.noSpaces))
      .build()
    val (status, response) = http(request)
    if status / 100 != 2 then
      throw RuntimeException(
        s"the private model answered HTTP $status: ${response.take(300)}"
      )
    parse(response).toOption
      .flatMap(
        _.hcursor
          .downField("choices")
          .downN(0)
          .downField("message")
          .get[String]("content")
          .toOption
      )
      .getOrElse(
        throw RuntimeException(
          s"unexpected answer from the private model: ${response.take(300)}"
        )
      )

  /** Posts `text`; failures are dropped, as nobody who may see them is waiting. */
  def postToSlack(config: Config, text: String, http: Http): Unit =
    val body = Json.obj(
      "channel" -> Json.fromString(config.channel),
      "text" -> Json.fromString(text),
      "unfurl_links" -> Json.False,
      "unfurl_media" -> Json.False
    )
    val request = HttpRequest
      .newBuilder(URI.create(s"${config.slackApi}/chat.postMessage"))
      .timeout(Duration.ofSeconds(30))
      .header("Content-Type", "application/json; charset=utf-8")
      .header("Authorization", s"Bearer ${config.slackToken}")
      .POST(HttpRequest.BodyPublishers.ofString(body.noSpaces))
      .build()
    val _ = Try(http(request))
