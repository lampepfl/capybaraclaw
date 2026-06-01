# Safemode Private LLM

Bridges every operation that needs LLM reasoning over confidential data to a
**private** model endpoint (default: a local Ollama instance). Every primitive
that takes a classified input returns classified output — there is no
agent-visible path that unwraps `Classified[String]` into a plain `String`.

The host (capybaraclaw) configures the endpoint by setting these JVM system
properties before the REPL boots; the agent has no path to mutate them:

```
safemode.private.provider   default: "ollama"
safemode.private.baseUrl    default: $OLLAMA_URL  or  http://localhost:11434
safemode.private.model      default: $OLLAMA_MODEL or  qwen3.5:latest
```

## Chat

```scala
@assumeSafe def chat(message: String): String
@assumeSafe def chat(message: Classified[String]): Classified[String]
```

Send free-form prompts to the private LLM. The classified overload keeps the
prompt and response inside the `Classified` wrapper end to end.

## Double-LLM answer sink

```scala
@assumeSafe def writePrivateAnswer[P <: Permission](
    message: Classified[String]
)(using FileSystem[P]): String
```

Routes a classified prompt through the private model and appends the response
to the configured classified output sink. The public agent never observes
either the input or the response — only the sink is mutated. This is the
canonical way to surface a confidential answer to the user.

**Returns a fixed STOP confirmation string** — value-independent, contains no
classified content. When the REPL prints
`"Wrote answer to the classified output sink. STOP — ..."` your write
succeeded; **do not call writePrivateAnswer (or any other tool) again** in the
same turn. The sink is append-only, so repeating the call duplicates the
answer. Just reply to the user with a brief acknowledgement.

## Reasoning primitives over `Classified[T]`

All four functions keep their result classified. To actually surface anything
to the user, chain through `writePrivateAnswer` (or `writeConfidential`).

```scala
@assumeSafe def summarize(text: Classified[String], maxWords: Int): Classified[String]
```
Bounded-length summary of a classified value. Result stays classified.

```scala
@assumeSafe def redact(text: Classified[String]): Classified[String]
```
Policy-bounded *description* of a classified value (shapes, distributions,
ranges). The private model is instructed to omit specific values, names, or
identifiers. The result is still classified — you must route it through
`writePrivateAnswer` or `writeConfidential` to release it.

```scala
@assumeSafe def categorize(text: Classified[String], buckets: List[String]): Classified[String]
```
Clamp a classified value to one of `buckets`. The private model picks; the
implementation clamps the response to ensure no out-of-bucket leakage.

```scala
@assumeSafe def extractField(text: Classified[String], field: String): Classified[String]
```
Pull the value of a named field from a classified document. Useful for
semi-structured text. Result stays classified.

## Idiomatic pattern

```scala
val payroll = loadCompReview("payroll.xlsx")
val outliers = payroll.outliers(zScore = 2.0)
val summary = summarize(outliers.map(_.toString), maxWords = 80)
writePrivateAnswer(summary)
```

The agent never sees `outliers` or `summary`. The sink — visible to the user
through the host's double-LLM bridge — receives the private model's
description of the outliers.
