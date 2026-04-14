# capybaraclaw

An always-running agent, trustworthy and secure.

`capybaraclaw` is a long-lived LLM agent that listens on Slack and drives a
capability-safe Scala 3 REPL. User code is executed inside a sandboxed REPL
whose access to files, processes, and the network is gated by the
[TACIT](https://github.com/lampepfl/tacit) capability library, so the agent
can evaluate arbitrary Scala without stepping outside its allowlist.

## Subprojects

- **`agents/`** — `tacit-agents`, the LLM client library (OpenAI, Anthropic,
  OpenRouter, Ollama endpoints; agent + tool abstractions). Self-contained.
- **`capybaraclaw/`** — the Slack bot, `ClawAgent`, and configuration loading.
  Wires together `agents` and `tacit.{core,executor}` to run the REPL.

## Prerequisites

- JDK 17+
- sbt 1.12+
- A local clone of [`lampepfl/tacit`](https://github.com/lampepfl/tacit) with
  its artifacts published to the local Ivy cache:

  ```sh
  cd path/to/tacit && sbt publishLocal
  ```

  This publishes `lampepfl::tacit` and `lampepfl::tacit-library` (a fat JAR)
  to `~/.ivy2/local/lampepfl/`, which this repo consumes as managed
  dependencies.

## Quickstart

1. Publish tacit locally (see above).
2. Export provider and Slack credentials in your shell:

   ```sh
   export SLACK_APP_TOKEN=xapp-...
   export SLACK_BOT_TOKEN=xoxb-...
   export OPENROUTER_API_KEY=sk-or-...   # or OPENAI_API_KEY / ANTHROPIC_API_KEY
   ```

3. Configure `claw.json` (provider, model, classified paths) and optionally
   `CLAW.md` (project-specific instructions) in the working directory. An
   example lives at `capybaraclaw/examples/default/`.
4. Run the bot:

   ```sh
   sbt claw
   ```

   The `claw` alias sets the working directory to
   `capybaraclaw/examples/default/` and launches the agent.

## Layout

```
.
├── agents/         tacit-agents (stable Scala 3)
├── capybaraclaw/   Slack bot + ClawAgent (Scala 3 nightly)
└── build.sbt       both subprojects + aggregation root
```

## License

MIT. See [LICENSE](LICENSE).
