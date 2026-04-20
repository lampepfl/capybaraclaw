# Roadmap for CapybaraClaw

## Work to be done

### Gateway

Currently supported interfaces:
- Slack
- TUI

#### More Connectors

Support more messenger connectors in the gateway. Candidates:
- Whatsapp
- Matrix
- Telegram
- Email
- Discord
- Signal
- WeChat
- ... and more

#### Improve the TUI Chat

The TUI chat right now is quite rudimentary. We want to make it full fledged with:
- Nice TUI rendering
- Session management
- More slash commands

#### Context Management

Now, we have only a jsonl-file-based simple context persistence. Things to consider:
- Efficient persistence solutions (databases).
- Auto-compaction when context reaches LLM's limit.
- Auto-memory (dreaming). Organise and summarise message histories into memories periodically.

#### Streaming

Support streaming a response from the agent to the messenger.

Current `Port` surface (`gateway/port/Port.scala`) — one-shot, final text only:

```scala
trait Port:
  def id: String
  def incoming: ReadableChannel[GatewayMessage]
  def send(key: ContextKey, text: String): Unit
  def shutdown(): Unit
```

Proposed: replace the one-shot `send` with a streaming handle so the runner can forward each delta as it arrives.

```scala
trait Port:
  ...
  def openReply(key: ContextKey): ReplyStream

trait ReplyStream:
  def delta(text: String): Unit
  def complete(finalText: String): Unit
  def abort(reason: String): Unit
```

- `CliPort`: `delta` prints each token as it arrives; `complete` newline-terminates and re-prompts.
- `SlackPort`: streaming API is supported by Slack.
- Email / batch-style ports: ignore `delta`, render only on `complete`.

### Harness Expressiveness

#### Real World Tools

Add more tools into TACIT.
- Email management.
- Office works.
- Web search.
- Browser/computer control.

#### Subagent Orchestration

Extend tools for subagent orchestration: forking subagents, creating agent teams, communication channels between agents.

#### Memory and Skills

Extend TACIT with tools for reading memories and loading skills.

### Harness Safety

We want to support finer-grained permission system. An agent of a different channel when handling a message from a different user should get different capabilities.

### User Experience

We want a smooth user experience for:
- Installing, running and configuring CapybaraClaw.
- Extending the TACIT harness with custom libraries. (A hub for libraries will be highly desirable. Users should be able to do something like `claw install plugin-name` to install a TACIT extension.)

### Devops

#### Testing

Expand the scope of testing, and tighten the testing framework.

Right now, the test suite consist of two parts:
(1) Simple functionality tests without network access.
(2) Tests that actually talks to API endpoint to test behaviours like function calling, streaming, etc.

The second part needs API keys in environment variables to run, and it takes a long time.

#### Packaging

Currently, this project depends a locally-published tacit. We have to standardize the publishing pipeline.

