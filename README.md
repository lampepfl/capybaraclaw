# Capybara Claw

An always-running agent, yet trustworthy and secure.

## Building the Project

The project depends on [`tacit`](https://github.com/lampepfl/tacit), which has to be locally published. So the first step for building will be:
``` shell
git clone git@github.com:lampepfl/tacit.git
cd tacit
sbt publishLocal
```

Then, inside `capybaraclaw/`, run `sbt test` for a sanity-check. It should build successfully.

