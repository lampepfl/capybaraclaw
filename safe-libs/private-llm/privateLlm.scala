package safemode.lib

import language.experimental.captureChecking
import scala.caps.assumeSafe

// ─── Private LLM Bridge (trusted secondary channel) ───────────────────────
//
// `privateGenerate` performs unrestricted HTTP. It is `@assumeSafe`, which
// tells the capture checker not to track the network capability — meaning the
// SAFETY ARGUMENT does NOT come from the type system here. It comes from the
// host: the URL is read from JVM system properties (`safemode.private.*`)
// that the host process — capybaraclaw, in our case — sets at startup before
// the REPL is initialized. Agent code cannot mutate those properties because
// `CodeValidator` blocks `System.setProperty`, and cannot replace this binding
// because `safemode.lib` is loaded from a sealed library JAR.
//
// In other words: if the host configured a trusted private endpoint, this
// stays a private channel; if the host got tricked into pointing it at an
// attacker's endpoint, the model never had a chance regardless.

/** Send a prompt to the configured private LLM and return its response text.
 *  Routed via JVM sysprops set by the host:
 *    - safemode.private.provider  (default "ollama")
 *    - safemode.private.baseUrl   (default $OLLAMA_URL, then http://localhost:11434)
 *    - safemode.private.model     (default $OLLAMA_MODEL, then qwen3.5:latest)
 *  Lives in the trusted harness layer — never exposed to agent code directly. */
@assumeSafe
private def privateGenerate(prompt: String): String =
  val provider = System.getProperty("safemode.private.provider", "ollama")
  if provider != "ollama" then
    throw IllegalStateException(
      s"Unsupported private LLM provider '$provider'; only 'ollama' is supported"
    )

  import java.net.http.HttpClient
  import java.net.http.HttpRequest
  import java.net.http.HttpRequest.BodyPublishers
  import java.net.http.HttpResponse.BodyHandlers
  import java.net.URI

  val baseUrl = System
    .getProperty(
      "safemode.private.baseUrl",
      sys.env.getOrElse("OLLAMA_URL", "http://localhost:11434"),
    )
    .stripSuffix("/")
  val model = System.getProperty(
    "safemode.private.model",
    sys.env.getOrElse("OLLAMA_MODEL", "qwen3.5:latest"),
  )
  val body = ujson.Obj(
    "model"  -> model,
    "prompt" -> prompt,
    "stream" -> false,
  ).render()
  val request = HttpRequest.newBuilder()
    .uri(URI.create(s"$baseUrl/api/generate"))
    .header("Content-Type", "application/json")
    .timeout(java.time.Duration.ofMinutes(5))
    .POST(BodyPublishers.ofString(body))
    .build()
  val response = HttpClient.newHttpClient().send(request, BodyHandlers.ofString())
  if response.statusCode() < 200 || response.statusCode() >= 300 then
    throw RuntimeException(
      s"Private Ollama call failed (${response.statusCode()}): ${response.body()}"
    )
  ujson.read(response.body()).obj.get("response").map(_.str)
    .getOrElse("[private LLM: missing response field]")

// ─── Public API exposed to the agent ──────────────────────────────────────

/** Chat with the private LLM. Plain String in, plain String out. */
@assumeSafe
def chat(message: String): String =
  privateGenerate(message)

/** Chat with the private LLM over a classified value. The value never leaves
 *  the Classified wrapper — `map` runs `privateGenerate` inside the sealed
 *  envelope, and the response stays classified. */
@assumeSafe
def chat(message: Classified[String]): Classified[String] =
  message.map(privateGenerate)

/** Send a classified prompt through the trusted private model and append the
 *  private model's response to the configured confidential output sink.
 *
 *  This is the double-LLM channel: the public agent never sees the
 *  confidential value or the private model's response — only the append-only
 *  sink changes.
 *
 *  Returns a value-independent confirmation string so the REPL prints a
 *  visible success line for the agent. Treat that line as a STOP signal:
 *  repeating the call duplicates the answer in the sink. */
@assumeSafe
def writePrivateAnswer[P <: Permission](
    message: Classified[String]
)(using fs: FileSystem[P]): String =
  writeConfidential(message.map(privateGenerate))
  "Wrote answer to the classified output sink. STOP — do not call any more tools; just acknowledge to the user."

/** Compress a classified value into a bounded-length classified summary.
 *  Useful when the agent needs to keep working with the gist of a confidential
 *  document but its full size would be unwieldy or risky to handle. The result
 *  is still classified — there is no escape from containment here. */
@assumeSafe
def summarize(text: Classified[String], maxWords: Int): Classified[String] =
  text.map: content =>
    privateGenerate(
      s"Summarize the following text in at most $maxWords words. " +
      s"Respond with only the summary, no preamble:\n\n$content"
    )

/** Produce a redacted description of a classified value via the private
 *  model. The result is still classified — but its content is a policy-bounded
 *  *description* (distributions, shapes, ranges), never specific values or
 *  identifiers. To actually surface the redaction to the user, route it
 *  through `writeConfidential` or `writePrivateAnswer`. */
@assumeSafe
def redact(text: Classified[String]): Classified[String] =
  text.map: content =>
    privateGenerate(
      "Produce a public-safe redacted description of the following confidential " +
      "content. Describe shapes, distributions, and high-level structure — DO " +
      "NOT include any specific values, names, identifiers, or details that " +
      "could re-identify a record. Respond with only the description:\n\n" +
      content
    )

/** Categorize a classified value into one of the supplied buckets. Returns a
 *  classified bucket label — the bucket is clamped to the supplied list so the
 *  agent can't reason about an unknown response. */
@assumeSafe
def categorize(text: Classified[String], buckets: List[String]): Classified[String] =
  text.map: content =>
    val raw = privateGenerate(
      s"Categorize the following confidential content into exactly one of: " +
      s"${buckets.mkString(", ")}. Respond with only the category name, " +
      s"no explanation:\n\n$content"
    ).trim
    buckets.find(b => b.equalsIgnoreCase(raw))
      .orElse(buckets.find(b => raw.toLowerCase.contains(b.toLowerCase)))
      .getOrElse(buckets.headOption.getOrElse("(unknown)"))

/** Extract the value of a named field from a classified value. The private
 *  model decides what counts as the field — useful for semi-structured text.
 *  The result is still classified. */
@assumeSafe
def extractField(text: Classified[String], field: String): Classified[String] =
  text.map: content =>
    privateGenerate(
      s"Extract the value of the field '$field' from the following content. " +
      s"Respond with only the value, no preamble or formatting:\n\n$content"
    ).trim
