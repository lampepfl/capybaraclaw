package capybaraclaw.gateway.port.cli

import munit.FunSuite

class CommandMatchingSuite extends FunSuite:

  import CommandMatching.*

  test("topMatches: returns Nil for non-slash input"):
    assertEquals(topMatches("hello", HintLimit).matches, Nil)
    assertEquals(topMatches("", HintLimit).matches, Nil)

  test(
    "topMatches: bare slash returns prefix matches alphabetical"
  ):
    val r = topMatches("/", HintLimit)
    val slashCommands = CliCommands.All.filter(_.startsWith("/"))
    assertEquals(r.matches.toSet, slashCommands)
    assertEquals(r.matches, r.matches.sorted)

  test("topMatches: hard cap honoured, truncated flag set"):
    val r = topMatches("/", limit = 2)
    assertEquals(r.matches.size, 2)
    assertEquals(r.truncated, true)

  test("topMatches: /ses narrows to /sessions"):
    assertEquals(topMatches("/ses", HintLimit).matches, List("/sessions"))

  test("topMatches: no prefix match"):
    val r = topMatches("/zzz", HintLimit)
    assert(r.matches.nonEmpty)
    assert(r.matches.size <= HintLimit)
    assert(r.matches.forall(CliCommands.All.contains))

  test("topMatches: pathologically long needle returns Nil"):
    val longNeedle = "/" + "a" * (MaxHintInputLength + 1)
    assertEquals(topMatches(longNeedle, HintLimit).matches, Nil)

  test("formatHints: empty result yields empty string"):
    assertEquals(formatHints(HintResult(Nil, false)), "")

  test("formatHints: not truncated"):
    assertEquals(
      formatHints(HintResult(List("/a", "/b"), truncated = false)),
      "↳ /a   /b"
    )

  test("formatHints: truncated"):
    assertEquals(
      formatHints(HintResult(List("/a", "/b"), truncated = true)),
      "↳ /a   /b   …"
    )

  test("suggestCommand: short typo against long command still suggests"):
    assertEquals(suggestCommand("/seions"), Some("/sessions"))

  test("suggestCommand: garbage with no close match returns None"):
    assertEquals(suggestCommand("/zzzzz"), None)

  test("suggestCommand: empty input returns None"):
    assertEquals(suggestCommand(""), None)
