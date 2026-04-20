# Capybara Claw

An always-running agent, yet trustworthy and secure.

> ⚠️ **Highly experimental and under active development.** Expect breaking changes, rough edges, and the occasional missing feature. Not yet suitable for production use.

## Building the Project

The project depends on the `agent-framework` branch of [`tacit`](https://github.com/lampepfl/tacit), which has to be locally published. So the first step for building will be:
``` shell
git clone git@github.com:lampepfl/tacit.git
cd tacit
git checkout agent-framework
sbt publishLocal
```

Then, inside `capybaraclaw/`, run `sbt test` for a sanity-check. It should build successfully.

## Running CapybaraClaw

Do `sbt claw`. This requires a valid `OPENROUTER_API_KEY` in the environment.

