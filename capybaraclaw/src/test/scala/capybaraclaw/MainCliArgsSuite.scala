package capybaraclaw

class MainCliArgsSuite extends munit.FunSuite:
  test("parses workdir when flag comes first"):
    val parsed =
      MainCliArgs.parse(List("--enable-slack", "capybaraclaw/examples/default"))
    assert(parsed.isRight)
    val args = parsed.toOption.getOrElse(fail("expected successful parse"))
    assertEquals(args.enableSlack, true)
    assertEquals(
      args.workDirFile.getPath.endsWith("capybaraclaw/examples/default"),
      true
    )

  test("parses workdir when flag comes last"):
    val parsed =
      MainCliArgs.parse(List("capybaraclaw/examples/default", "--enable-slack"))
    assert(parsed.isRight)
    val args = parsed.toOption.getOrElse(fail("expected successful parse"))
    assertEquals(args.enableSlack, true)
    assertEquals(
      args.workDirFile.getPath.endsWith("capybaraclaw/examples/default"),
      true
    )

  test("rejects more than one workdir"):
    val parsed = MainCliArgs.parse(List("one", "two"))
    assert(parsed.isLeft)
    val message = parsed.left.toOption.getOrElse("")
    assert(message.contains("at most one workdir"))
