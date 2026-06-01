# Compensation Review demo — setup

This workdir drives a Capybara Claw agent that analyses `payroll.xlsx` through
three composing TACIT plugins:

- **`safemode.capabilities`** — foundation: `Classified[T]`, `FileSystem`,
  capability brokers.
- **`safemode.private-llm`** — bridge to a local LLM for confidential
  reasoning: `writePrivateAnswer`, `summarize`, `redact`, `categorize`,
  `extractField`.
- **`safemode.compreview`** — domain plugin: `loadCompReview`, salary /
  comp-ratio / outliers / band-violations analytics.

The compreview and private-llm plugins both declare `requires:
["safemode.capabilities"]`, so TACIT will refuse to start if any of the three
is missing from `plugins/`.

## One-time setup

```bash
# 1. Build the safe-libs (from repo root)
./safe-libs/build-jars.sh

# 2. Link the three plugin JARs into this workdir's plugins/ folder
cd capybaraclaw/examples/comp-review-demo
mkdir -p plugins
ln -sf ../../../../safe-libs/capabilities/target/capabilities-assembly.jar plugins/
ln -sf ../../../../safe-libs/private-llm/target/private-llm-assembly.jar plugins/
ln -sf ../../../../safe-libs/compreview/target/compreview-assembly.jar plugins/
```

Rebuilding any safe-lib overwrites its JAR in place — symlinks pick the new
version up automatically. After touching `safe-libs/capabilities/` you should
re-run `./safe-libs/build-jars.sh` for **all three** plugins so the bundled
`safemode.lib.*` classes stay byte-identical across JARs.

## Running the agent

From the repo root:

```bash
sbt "runMain capybaraclaw.main capybaraclaw/examples/comp-review-demo"
```

The startup banner should list all three plugins:

```
plugins  : Safemode Capabilities 0.1.0 (replace-core),
           Safemode Private LLM 0.1.0 (replace-core),
           Safemode Compensation Review 0.1.0 (replace-core)
```

If the plugins line is missing entries or says `(none in …/plugins)`, the
symlinks are wrong or one of the safe-libs has not been built. If TACIT refuses
to start with an error mentioning `requires`, the dependency contract is
violated — check that `capabilities-assembly.jar` is present and unbroken.
