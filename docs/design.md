# Design of Capybara Claw

Yichen Xu, 20 Apr 2026

## Scope of CapybaraClaw

It is an always-running LLM agent that offers both power and safety, by leveraging [TACIT](https://github.com/lampepfl/tacit).

TACIT is an agent harness. What makes TACIT different from traditional harnesses is that it requires LLMs to express their intentions in capability-safe Scala code. It is a known issue that the behaviour of LLMs is difficult to reason about. TACIT provides a clever solution: LLMs must express their intentions in a formal language (which is Scala). Then, all the type level analysis mechanics that have been there for decades come to help reasoning about the **safety** of LLM actions. Apparently unsafe LLM actions (like leaking private information to the public) should be rejected at the type level. 

Among all the typing mechanisms, capture checking, a recent Scala 3 experimental feature, is of greatest significance. It provides a capability framework, and tracks capabilities in types. The entire TACIT is built on top of these notions.

So, CapybaraClaw is an always-running LLM agent connected to TACIT as its harness for provably-safe behaviours. It also provides a wide variety of communication channels, like CLI, Slack, WhatsApp, etc.

Goals of CapybaraClaw:
- Safety: It provably prevents a well-defined set of unsafe behaviours. There should be concrete examples of what it can prevent.
- Usability: Its harness should be able to express a wide variety of computer tasks (coding, file management, information research, office tasks, designing, messaging, task management, and more and more and more).
- Extensibility: Its architecture must be easy to extend. Users should be able to connect CapybaraClaw to new communication channels, extend the TACIT harness to support more tasks, et cetera.

## Architecture

### Core Concepts

- TACIT. The agent harness. It provides the API using which the agent interacts with the computer.
- Agent. The LLM agent connected to TACIT.
- Message gateway. An aggregate of all message channels.
- Context manager. It manages the context fed into the agent. Session management, memory, skills, etc., all fit in here.

### Layers

#### Completion API

Path: `agents/src/main/scala/tacit/agents/llm/endpoint/` (package `tacit.agents.llm.endpoint`).

A provider-agnostic abstraction over streaming LLM APIs. The central trait is:

```scala
trait Endpoint:
  def invoke(messages: List[Message], config: LLMConfig): Result[ChatResponse, LLMError]
  def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]]
```

#### Tool-Calling Agent

Path: `agents/src/main/scala/tacit/agents/llm/agentic/` (package `tacit.agents.llm.agentic`).

The agentic loop on top of the Completion API. It:
- installs tools,
- runs the tool calling loop until an output.

```scala
abstract class Agent:
  type State <: AgentState
  val state: State

  def handle[A: IsToolArg](name: String, desc: String)(handler: (A, State) => String): this.type

  def ask(message: String)(using Endpoint): Result[ChatResponse, AgentError]
  def streamAsk(message: String)(using Endpoint, Async.Spawn): AgentRun
```

`ask` is synchronous, blocking until completion. `streamAsk` is async, returning a live handle:

```scala
class AgentRun:
  val events: ReadableChannel[Result[AgentStreamEvent, AgentError]]
  def steer(text: String): SteerOutcome   // Accepted | RejectedRunEnded
  def isActive: Boolean
```

`steer` enqueues a user message that lands at the next post-tool-result boundary. The event channel streams live completion events (message delta, tool call, etc.).

#### ClawAgent

Path: `capybaraclaw/src/main/scala/capybaraclaw/agent/` (package `capybaraclaw.agent`).

`Agent` + TACIT backend. It:
- loads `AgentConfig` from `claw.json` and configures endpoint, thinking mode, and system prompt,
- owns a TACIT `ReplSession` and exposes it to the LLM as an `evaluate_scala` tool,
- seeds prior history via `initialMessages`.

```scala
class ClawAgent(val workDir: String, initialMessages: List[Message] = Nil):
  def ask(message: String): Result[ChatResponse, AgentError]
  def streamAsk(message: String)(using Async.Spawn): AgentRun
```

`AgentConfig.load(workDir)` also builds the system prompt from the bundled `Interface.scala` and the project's optional `CLAW.md`.

### Gateway

Path: `capybaraclaw/src/main/scala/capybaraclaw/gateway/` (package `capybaraclaw.gateway`).

Multiplexes inbound messages from N `Port`s into per-thread `ClawAgent` instances. It:
- fans in each `Port`'s `incoming` channel,
- keys each conversation by `ContextKey(port, thread)` — one agent + one REPL + one transcript per thread, shared across users,
- rehydrates prior history from a `ContextProvider` on first touch, and appends new user/assistant messages as the turn proceeds,
- routes replies back to the originating port.

```scala
case class Origin(port: String, thread: String, user: String)
case class ContextKey(port: String, thread: String)
case class GatewayMessage(origin: Origin, text: String)

trait Port:
  def id: String
  def incoming: ReadableChannel[GatewayMessage]
  def send(key: ContextKey, text: String): Unit
  def shutdown(): Unit

trait ContextProvider:
  def load(key: ContextKey): List[Message]
  def append(key: ContextKey, msg: Message): Unit

class Gateway(workDir: String, ports: List[Port], contextProvider: ContextProvider):
  def run()(using Async.Spawn): Unit
  def shutdown(): Unit
```

Mid-turn messages are forwarded to the active `AgentRun` via `steer`. `JsonlContextProvider` persists transcripts to `.claw/history/{port}/{thread}.jsonl`. 

Current ports: `SlackPort` (in `port.slack`) and `CliPort` (in `port.cli`).

