# Capybara Claw

An always-running agent, yet trustworthy and secure.

> ⚠️ **Highly experimental and under active development.** Expect breaking changes, rough edges, and the occasional missing feature. Not yet suitable for production use.

## Building the Project

The project depends on [`tacit`](https://github.com/lampepfl/tacit), which has to be locally published. So the first step for building will be:
``` shell
git clone git@github.com:lampepfl/tacit.git
cd tacit
sbt publishLocal
```

Then, inside `capybaraclaw/`, run `sbt test` for a sanity-check. It should build successfully.

## Running CapybaraClaw

Do `sbt claw`. This requires a valid `OPENROUTER_API_KEY` in the environment.

## Connecting to Slack

Pass `--enable-slack`; capybara then also needs `SLACK_BOT_TOKEN` (`xoxb-…`)
and `SLACK_APP_TOKEN` (`xapp-…`) in the environment. Create the Slack app as
Slack's documentation describes; what capybara relies on:

- [Socket Mode](https://docs.slack.dev/apis/events-api/using-socket-mode)
  enabled. The app-level token is `SLACK_APP_TOKEN`.
- [Interactivity](https://docs.slack.dev/interactivity/handling-user-interaction)
  enabled, or the buttons on permission requests do nothing: the request stays
  pending and the agent keeps waiting.
- The `message` events
  ([`message.channels`](https://docs.slack.dev/reference/events/message.channels),
  [`message.groups`](https://docs.slack.dev/reference/events/message.groups),
  [`message.im`](https://docs.slack.dev/reference/events/message.im),
  [`message.mpim`](https://docs.slack.dev/reference/events/message.mpim))
  for the conversations capybara should answer in.
- Bot token scopes for the Web API methods it calls; each method's page lists
  the scopes it needs:
  [`auth.test`](https://docs.slack.dev/reference/methods/auth.test),
  [`chat.postMessage`](https://docs.slack.dev/reference/methods/chat.postMessage),
  [`chat.update`](https://docs.slack.dev/reference/methods/chat.update),
  [`chat.postEphemeral`](https://docs.slack.dev/reference/methods/chat.postEphemeral),
  [`chat.startStream`](https://docs.slack.dev/reference/methods/chat.startStream),
  [`chat.appendStream`](https://docs.slack.dev/reference/methods/chat.appendStream),
  [`chat.stopStream`](https://docs.slack.dev/reference/methods/chat.stopStream),
  [`reactions.add`](https://docs.slack.dev/reference/methods/reactions.add),
  [`reactions.remove`](https://docs.slack.dev/reference/methods/reactions.remove),
  [`conversations.history`](https://docs.slack.dev/reference/methods/conversations.history),
  [`conversations.info`](https://docs.slack.dev/reference/methods/conversations.info),
  [`users.info`](https://docs.slack.dev/reference/methods/users.info).
  A missing scope shows up as a `missing_scope` error in
  `~/.claw/logs/capybara.log`.

## People and roles

Who may be asked for which permissions is set in `~/.claw/identities.json`,
written by the operator. Capybara re-reads it whenever it changes, so roles
apply without a restart; a change that does not load is logged and the
previous version stays in force.

```json
{
  "roles": {
    "admin": { "may": ["*"] },
    "dev": {
      "may": [
        "network:fetch",
        { "permission": "exec", "items": ["git", "sbt"] },
        { "permission": "files:read", "items": ["~/src"] }
      ]
    },
    "finance": { "may": ["plugin:sheets/read payroll"] }
  },
  "default_roles": [],
  "people": {
    "lukasz": { "ids": ["cli:lbialy", "slack:U0123"], "roles": ["admin"] },
    "ann": { "ids": ["slack:U0456"], "roles": ["dev", "finance"] }
  }
}
```

- `ids` link a person's accounts across ports (`<port>:<user>`, e.g. a Slack
  member id). Someone not listed, and a person without `roles`, gets
  `default_roles`.
- A role's `may` lists the permissions its holders may be asked for:
  `files`, `files:read`, `files:write`, `exec`, `network`, `network:fetch`,
  `network:send`, `plugin`, `plugin:<id>`, `plugin:<id>/<permission>`, or `*`
  for everything. A pattern covers everything below it, and `*` may stand for
  one whole part (`plugin:*`, `plugin:<id>/*`). `files:write` covers reading,
  `network:send` fetching.
- `items` limits a rule to those command names (`exec`), host names
  (`network`), plugin items (`plugin:…`), or directories and everything below
  them (`files`, absolute or starting with `~/`).

When agent code needs a permission during someone's turn, they are asked for
it only if their roles allow it; otherwise it is denied without asking.
Only the person who asked can answer, and what they approve is granted to
them in that session only: in a shared Slack thread, someone else's turn
does not use it. A grant stops counting as soon as its holder's roles no
longer allow it. While a turn runs, only its sender's messages join it;
everyone else's wait for a turn of their own.

The terminal user who started capybara (`cli:$USER`) is the operator, who
may be asked for anything whatever the file says. Without the file, nobody
else may be asked for anything.
