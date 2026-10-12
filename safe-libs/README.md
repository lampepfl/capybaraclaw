# safe-libs: TACIT plugins for capybaraclaw

Two `extend-core` plugins on top of tacit-library (the capability API, the
permission oracle and `Classified`):

- `sheets` (`claw.sheets`): loads `.xlsx` files through the agent's
  `FileSystem`. Columns whose header starts with `*` are confidential: their
  values only leave the table inside `Classified` aggregates. The file must be
  under a `classified_paths` entry of `claw.json`, e.g.
  `"classified_paths": ["$workdir/payroll.xlsx"]`; otherwise agent code could
  read its bytes directly, and `loadSheet` refuses it.
- `private-channel` (`claw.private-channel`): delivers `Classified` data to
  one private Slack channel, optionally through a private model. The agent
  only learns that it was sent: the call returns at once and the message is
  built and posted in the background, so neither the reply nor the time it
  takes depends on the data. The first delivery in a session asks the user for
  the plugin permission "post any confidential data capybara holds to the
  private channel": once granted, any `Classified` value of that session can
  go there (and, with `askPrivately`, to the private model).

## Build

Needs tacit-library `0.2.2-SNAPSHOT` in `~/.ivy2/local`, from the TACIT commit
capybaraclaw pins (`sbt publishLocal` in that checkout), and scala-cli.

```sh
safe-libs/build.sh [workdir]   # JARs into <workdir>/plugins, default capybaraclaw/examples/default
scala-cli run safe-libs/sample-data -- capybaraclaw/examples/default/payroll.xlsx
scala-cli test safe-libs/sheets
scala-cli test safe-libs/private-channel
```

## Configure the private channel

Environment of the gateway (agent code cannot read or change it):

| Variable | |
|---|---|
| `SLACK_BOT_TOKEN` | the bot capybara already uses; invite it to the channel |
| `CLAW_PRIVATE_SLACK_CHANNEL` | channel ID, e.g. `C0123ABCD` |
| `CLAW_PRIVATE_SLACK_CHANNEL_NAME` | optional, shown to users, e.g. `#hr-private` |
| `CLAW_PRIVATE_LLM_MODEL` | model for `askPrivately`; without it only `postPrivately` works |
| `CLAW_PRIVATE_LLM_BASE_URL` | OpenAI-compatible API, default `http://localhost:11434/v1` (Ollama) |
| `CLAW_PRIVATE_LLM_API_KEY` | optional bearer token |
| `CLAW_PRIVATE_SLACK_API_URL` | for tests only: where the bot token is sent, default `https://slack.com/api` |

Deliveries run on a background thread that the gateway waits for when it
shuts down. If a delivery never shows up, the bot is most likely not in the channel
(`/invite @capybara` there) or the channel ID is wrong: failures cannot be
reported to the agent without telling it something about the data.

## Notes for sheet authors

- Only the first sheet is read; the `*` markers are the only thing that keeps
  a column hidden, and a sheet without any is refused.
- Files that are valid UTF-8 text are refused: that is all agent code can write
  under a classified path (`writeClassified`), so it cannot build a workbook
  from Classified data and read its "public" columns back.
- A formula in a public column is shown evaluated, so a public
  `=IF(F2>150000,"high","low")` reveals what it computes from `*Salary`.
- Rows without any public value are ignored (a filled-in confidential cell in
  an otherwise empty row would otherwise show through `rowCount`).

The plugins call Slack and the model directly from trusted library code, not
through the agent's `requestNetwork`: the destination is the operator's
choice, not the agent's.
