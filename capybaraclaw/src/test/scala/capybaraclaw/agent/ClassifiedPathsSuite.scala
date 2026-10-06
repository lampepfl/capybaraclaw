package capybaraclaw.agent

import java.nio.file.{Files, Path}

class ClassifiedPathsSuite extends munit.FunSuite:
  // The REPL compiles each snippet, which is slow on a cold JVM.
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private val home = System.getProperty("user.home")

  // tacit confines the agent to roots under the JVM's working directory by
  // default, so the workdir must live there rather than in the system tmp.
  private val workDir = FunFixture[Path](
    setup = _ =>
      val base = Files.createDirectories(Path.of("target").toAbsolutePath)
      val dir = Files.createTempDirectory(base, "claw-classified").toRealPath()
      Files.writeString(dir.resolve(".env"), "ENV_SECRET")
      Files.createDirectories(dir.resolve("sub/.ssh"))
      Files.writeString(dir.resolve("sub/.ssh/id_rsa"), "SSH_KEY")
      Files.createDirectories(dir.resolve("keys"))
      Files.writeString(dir.resolve("keys/prod.pem"), "PEM_SECRET")
      Files.createDirectories(dir.resolve("a/b"))
      Files.writeString(dir.resolve("a/b/token"), "NESTED_TOKEN")
      Files.writeString(dir.resolve("public.txt"), "PUBLIC")
      dir
    ,
    teardown = deleteRecursively
  )

  test("defaults are kept when no classified_paths are configured"):
    val patterns = ReplEnvironment.classifiedPatterns("/w", Nil)
    assert(patterns.contains(".ssh"), patterns)
    assert(patterns.contains(".env"), patterns)

  test("each entry kind maps to the matching tacit pattern"):
    val patterns = ReplEnvironment.classifiedPatterns(
      "/w",
      List(
        ".ssh",
        "emails/",
        "/abs/keys",
        "~",
        "~/.config/gh",
        "$workdir",
        "$workdir/emails/",
        "$workdir/../x",
        "$workdirs",
        "a/b",
        "./c",
        "../d"
      )
    )
    assertEquals(
      patterns.filterNot(ReplEnvironment.classifiedPatterns("/w", Nil).toSet),
      List(
        "emails/",
        "/abs/keys",
        home,
        s"$home/.config/gh",
        "/w",
        "/w/emails",
        "/x",
        "$workdirs",
        "/w/a/b",
        "/w/c",
        "/d"
      )
    )

  private def read(paths: List[String], dir: Path, rel: String): String =
    val env = ReplEnvironment(dir.toString, paths)
    env.repl
      .execute(s"""requestFileSystem("$dir") { access("$dir/$rel").read() }""")
      .output

  private def assertBlocked(paths: List[String], dir: Path, rel: String) =
    val out = read(paths, dir, rel)
    assert(out.contains("classified path"), out)

  workDir.test("tacit defaults protect files without classified_paths"): dir =>
    assertBlocked(Nil, dir, ".env")
    assertBlocked(Nil, dir, "sub/.ssh/id_rsa")

  workDir.test("a bare name is classified at any depth"): dir =>
    assertBlocked(List("token"), dir, "a/b/token")

  workDir.test("an absolute path is classified as-is"): dir =>
    assertBlocked(List(s"$dir/keys"), dir, "keys/prod.pem")

  workDir.test("$workdir/<name> classifies only the top-level entry"): dir =>
    Files.createDirectories(dir.resolve("a/keys"))
    Files.writeString(dir.resolve("a/keys/nested.pem"), "NOT_CLASSIFIED")
    assertBlocked(List("$workdir/keys"), dir, "keys/prod.pem")
    val out = read(List("$workdir/keys"), dir, "a/keys/nested.pem")
    assert(out.contains("\"NOT_CLASSIFIED\""), out)

  workDir.test("a relative path is resolved against the workdir"): dir =>
    assertBlocked(List("a/b"), dir, "a/b/token")

  workDir.test("unclassified files stay readable"): dir =>
    val out = read(List("keys"), dir, "public.txt")
    assert(out.contains("\"PUBLIC\""), out)

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
