package capybaraclaw.gateway.sqlite

import Fragments.{Close, Open}

class FragmentsSuite extends munit.FunSuite:
  /** 100 five-character words: "w001 w002 … w100". */
  private val words = (1 to 100).map(i => f"w$i%03d").mkString(" ")

  private def withMatch(text: String, word: String): List[(Int, Int)] =
    val at = text.indexOf(word)
    List(at -> (at + word.length))

  test("ranges reads match positions from highlight() output"):
    assertEquals(
      Fragments.ranges("a foo b foo", s"a ${Open}foo$Close b ${Open}foo$Close"),
      Some(List(2 -> 5, 8 -> 11))
    )

  test("ranges gives up when the text contains the markers itself"):
    assertEquals(Fragments.ranges(s"a${Open}b", s"a$Open${Open}b$Close"), None)

  test("ranges gives up when highlight() output does not match the text"):
    assertEquals(Fragments.ranges("abc", s"${Open}abd$Close"), None)

  test("a short message is one fragment without ellipses"):
    assertEquals(
      Fragments.render("we use docker here", List(7 -> 13), 200, 40),
      List("we use «docker» here")
    )

  test("radius counts from the match edges, then extends to a word boundary"):
    val text = words.replace("w050", "needle")
    val List(fragment) =
      Fragments.render(text, withMatch(text, "needle"), 200, 40): @unchecked
    assert(fragment.startsWith("…") && fragment.endsWith("…"), fragment)
    val before = fragment.drop(1).takeWhile(_ != '«')
    val after = fragment.dropRight(1).reverse.takeWhile(_ != '»').reverse
    assert(before.length >= 200 && before.length <= 240, before.length)
    assert(after.length >= 200 && after.length <= 240, after.length)
    assert(before.startsWith("w"), before)
    assert(after.endsWith(" ") || after.last.isDigit, after)
    assert(text.contains(before + "needle" + after))
    val start = text.indexOf(before + "needle")
    assert(text(start - 1).isWhitespace, "starts at a word boundary")

  test("without whitespace nearby, edges are cut at exactly the radius"):
    val text = "a" * 500 + "needle" + "b" * 500
    val List(fragment) =
      Fragments.render(text, withMatch(text, "needle"), 200, 40): @unchecked
    assertEquals(fragment, "…" + "a" * 200 + "«needle»" + "b" * 200 + "…")

  test("a hard cut never splits a surrogate pair"):
    val emoji = "😀"
    val text = emoji * 300 + "needle" + emoji * 300
    val List(fragment) =
      Fragments.render(text, withMatch(text, "needle"), 201, 0): @unchecked
    // An odd radius lands both hard cuts inside an emoji; each edge takes
    // the whole emoji instead of half of it.
    assertEquals(fragment, "…" + emoji * 101 + "«needle»" + emoji * 101 + "…")

  test("nearby matches merge into one fragment with both marked"):
    val text = words.replace("w050", "alpha").replace("w052", "omega")
    val matches = withMatch(text, "alpha") ++ withMatch(text, "omega")
    val fragments = Fragments.render(text, matches, 200, 40)
    assertEquals(fragments.size, 1)
    assert(fragments.head.contains("«alpha» w051 «omega»"), fragments.head)

  test("distant matches give separate fragments in text order"):
    val text = words.replace("w005", "alpha").replace("w095", "omega")
    val matches = withMatch(text, "omega") ++ withMatch(text, "alpha")
    val fragments = Fragments.render(text, matches, 100, 10)
    assertEquals(fragments.size, 2)
    assert(fragments(0).contains("«alpha»") && !fragments(0).startsWith("…"))
    assert(fragments(1).contains("«omega»") && !fragments(1).endsWith("…"))
