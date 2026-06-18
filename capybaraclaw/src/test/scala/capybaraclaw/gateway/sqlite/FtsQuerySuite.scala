package capybaraclaw.gateway.sqlite

import capybaraclaw.gateway.SearchTerms

class FtsQuerySuite extends munit.FunSuite:

  test("single allOf term is a literal phrase"):
    assertEquals(
      FtsQuery.compile(SearchTerms(allOf = List("docker"))),
      Some("\"docker\"")
    )

  test("multiple allOf terms are ANDed"):
    assertEquals(
      FtsQuery.compile(SearchTerms(allOf = List("docker", "compose"))),
      Some("\"docker\" AND \"compose\"")
    )

  test("multi-word term stays a single phrase"):
    assertEquals(
      FtsQuery.compile(SearchTerms(allOf = List("docker compose"))),
      Some("\"docker compose\"")
    )

  test("anyOf terms are ORed inside a group"):
    assertEquals(
      FtsQuery.compile(SearchTerms(anyOf = List("docker", "podman"))),
      Some("(\"docker\" OR \"podman\")")
    )

  test("allOf and anyOf combine with the OR group parenthesized"):
    assertEquals(
      FtsQuery.compile(
        SearchTerms(allOf = List("k8s"), anyOf = List("a", "b"))
      ),
      Some("\"k8s\" AND (\"a\" OR \"b\")")
    )

  test("noneOf appends NOT to a single positive term"):
    assertEquals(
      FtsQuery.compile(
        SearchTerms(allOf = List("docker"), noneOf = List("swarm"))
      ),
      Some("\"docker\" NOT \"swarm\"")
    )

  test("noneOf wraps a multi-term positive so NOT applies to the whole"):
    assertEquals(
      FtsQuery.compile(
        SearchTerms(allOf = List("docker", "compose"), noneOf = List("swarm"))
      ),
      Some("(\"docker\" AND \"compose\") NOT \"swarm\"")
    )

  test("multiple noneOf terms each get their own NOT"):
    assertEquals(
      FtsQuery.compile(SearchTerms(allOf = List("a"), noneOf = List("b", "c"))),
      Some("\"a\" NOT \"b\" NOT \"c\"")
    )

  test("pure negation has no positive term and yields None"):
    assertEquals(FtsQuery.compile(SearchTerms(noneOf = List("swarm"))), None)

  test("empty terms yield None"):
    assertEquals(FtsQuery.compile(SearchTerms()), None)

  test("prefix appends '*' to positive terms but leaves exclusions exact"):
    assertEquals(
      FtsQuery.compile(
        SearchTerms(
          allOf = List("deploy"),
          noneOf = List("test"),
          prefix = true
        )
      ),
      Some("\"deploy\"* NOT \"test\"")
    )

  test("prefix on a multi-word term targets its final token"):
    assertEquals(
      FtsQuery.compile(
        SearchTerms(allOf = List("docker compose"), prefix = true)
      ),
      Some("\"docker compose\"*")
    )

  test("embedded double-quote is doubled, never breaking the literal"):
    assertEquals(
      FtsQuery.compile(SearchTerms(allOf = List("say \"hi\""))),
      Some("\"say \"\"hi\"\"\"")
    )

  test("a term with FTS metacharacters is matched literally, not parsed"):
    assertEquals(
      FtsQuery.compile(SearchTerms(allOf = List("error:"))),
      Some("\"error:\"")
    )
    assertEquals(
      FtsQuery.compile(SearchTerms(allOf = List("my-app.config.ts"))),
      Some("\"my-app.config.ts\"")
    )

  test("blank and punctuation-only terms are dropped"):
    assertEquals(
      FtsQuery.compile(SearchTerms(allOf = List("docker", "   ", "---"))),
      Some("\"docker\"")
    )

  test("a list of only droppable terms yields None"):
    assertEquals(FtsQuery.compile(SearchTerms(allOf = List("---", "  "))), None)
