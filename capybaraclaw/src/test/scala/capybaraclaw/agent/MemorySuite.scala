package capybaraclaw.agent

import java.util.concurrent.{Callable, CountDownLatch, Executors}

import capybaraclaw.agent.tools.MemoryTool

class MemorySuite extends munit.FunSuite:
  private val tmpRoot: os.Path = os.temp.dir(prefix = "claw-memory-tests")

  override def afterAll(): Unit = os.remove.all(tmpRoot)

  private def freshStore(): MemoryStore =
    MemoryStore(os.temp.dir(tmpRoot, prefix = "store-").toIO)

  private def base(store: MemoryStore): os.Path = os.Path(store.baseDir)

  extension (r: MemoryResult)
    private def apply(key: String): ujson.Value = MemoryResult.toJson(r)(key)
    private def has(key: String): Boolean =
      MemoryResult.toJson(r).obj.contains(key)
    private def render(): String = MemoryResult.toJson(r).render()

  private def assertSuccess(result: MemoryResult): Unit =
    assert(MemoryResult.toJson(result)("success").bool, result.render())

  private def assertFailure(result: MemoryResult): Unit =
    assert(!MemoryResult.toJson(result)("success").bool, result.render())

  private def entries(result: MemoryResult): List[String] =
    MemoryResult.toJson(result)("entries").arr.toList.map(_.str)

  private def writeRaw(
      store: MemoryStore,
      f: MemoryFile,
      content: String
  ): Unit =
    os.write.over(base(store) / f.fileName, content)

  private def driftFile(store: MemoryStore, content: String): Unit =
    writeRaw(store, MemoryFile.Memory, content)

  private def memBackups(store: MemoryStore): List[String] =
    val dir = base(store)
    val prefix = s"${MemoryFile.Memory.fileName}.bak."
    if os.exists(dir) then
      os.list(dir).map(_.last).filter(_.startsWith(prefix)).toList
    else Nil

  private def runTool(store: MemoryStore, args: MemoryTool.Args): ujson.Value =
    ujson.read(MemoryTool.run(store, args))

  private def concurrently[A](n: Int, threads: Int)(task: Int => A): List[A] =
    val pool = Executors.newFixedThreadPool(threads)
    try
      val gate = CountDownLatch(1)
      val futures = (1 to n).toList.map: i =>
        val job: Callable[A] = () =>
          gate.await()
          task(i)
        pool.submit(job)
      gate.countDown()
      futures.map(_.get())
    finally pool.shutdownNow()

  private def assertAllPresent(content: String, n: Int, prefix: String): Unit =
    val present = (1 to n).count(i => content.contains(s"$prefix$i"))
    assertEquals(
      present,
      n,
      s"expected all $n entries, found $present in:\n$content"
    )

  test("add on empty file writes the entry verbatim"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.Memory, "first note"))
    assertEquals(store.read(MemoryFile.Memory), "first note")

  test("add appends entries with the section separator"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.Memory, "first"))
    assertSuccess(store.add(MemoryFile.Memory, "second"))
    assertEquals(store.read(MemoryFile.Memory), "first\n§\nsecond")

  test("add returns success without persisting an exact duplicate"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.Memory, "same"))
    val result = store.add(MemoryFile.Memory, "same")
    assertSuccess(result)
    assert(result("message").str.contains("no duplicate"), result.render())
    assertEquals(entries(result), List("same"))

  test("add refuses to exceed cap and returns live entries"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.User, "existing"))
    val result = store.add(MemoryFile.User, "x" * MemoryFile.User.capacity)
    assertFailure(result)
    assert(result("error").str.contains("exceed the limit ("), result.render())
    assert(
      result("error").str.contains(s"/${MemoryFile.User.capacity})"),
      result.render()
    )
    assertEquals(
      result("current_entries").arr.toList.map(_.str),
      List("existing")
    )

  test("replace replaces the complete entry identified by a substring"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.Memory, "alpha beta gamma"))
    assertSuccess(store.replace(MemoryFile.Memory, "beta", "replacement"))
    assertEquals(store.read(MemoryFile.Memory), "replacement")

  test("replace refuses a substring matching multiple entries"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.Memory, "foo first"))
    assertSuccess(store.add(MemoryFile.Memory, "foo second"))
    val result = store.replace(MemoryFile.Memory, "foo", "replacement")
    assertFailure(result)
    assert(result("error").str.contains("Multiple entries"), result.render())

  test("remove deletes the complete entry identified by a substring"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.Memory, "one"))
    assertSuccess(store.add(MemoryFile.Memory, "keep no fragment of two"))
    assertSuccess(store.add(MemoryFile.Memory, "three"))
    assertSuccess(store.remove(MemoryFile.Memory, "fragment of two"))
    assertEquals(store.read(MemoryFile.Memory), "one\n§\nthree")

  test("snapshot normalizes and deduplicates entries"):
    val store = freshStore()
    driftFile(store, "one\n§\none")
    val snapshot = store.snapshot()
    assertEquals(snapshot.memory, "one")
    assertEquals(snapshot.userPct, 0)

  test("snapshot reports usage percentages"):
    val store = freshStore()
    assertSuccess(
      store.add(MemoryFile.Memory, "x" * (MemoryFile.Memory.capacity / 2))
    )
    val snapshot = store.snapshot()
    assert(
      snapshot.memoryPct >= 49 && snapshot.memoryPct <= 51,
      s"got ${snapshot.memoryPct}%"
    )

  test(
    "mutation refuses non-roundtripping external drift and creates a backup"
  ):
    val store = freshStore()
    driftFile(store, "manually edited \n§\nkept")
    val result = store.add(MemoryFile.Memory, "new")
    assertFailure(result)
    assert(result("error").str.contains("Refusing to write"), result.render())
    assert(
      result("drift_backup").str.contains("MEMORY.md.bak."),
      result.render()
    )
    assertEquals(memBackups(store).size, 1)
    assertEquals(store.read(MemoryFile.Memory), "manually edited \n§\nkept")

  test("repeated mutations against unchanged drift reuse one backup"):
    val store = freshStore()
    driftFile(store, "manual \n§\ncontent")
    val first = store.add(MemoryFile.Memory, "first")
    val second = store.add(MemoryFile.Memory, "second")
    assertEquals(first("drift_backup").str, second("drift_backup").str)
    assertEquals(memBackups(store).size, 1)

  test("a successful write deletes backups whose content is now subsumed"):
    val store = freshStore()
    driftFile(store, "manually edited \n§\nkept")
    assertFailure(store.add(MemoryFile.Memory, "new"))
    driftFile(store, "manually edited\n§\nkept")
    assertSuccess(store.add(MemoryFile.Memory, "new"))
    assertEquals(memBackups(store), Nil)

  test("a successful write keeps backups whose content is not subsumed"):
    val store = freshStore()
    driftFile(store, "manually edited \n§\nkept")
    assertFailure(store.add(MemoryFile.Memory, "new"))
    driftFile(store, "totally different")
    assertSuccess(store.add(MemoryFile.Memory, "new"))
    assertEquals(memBackups(store).size, 1)

  test("reading a missing store does not create its base directory"):
    val absent = tmpRoot / s"missing-${java.util.UUID.randomUUID()}"
    val store = MemoryStore(absent.toIO)
    assertEquals(store.snapshot(), MemorySnapshot.empty)
    assert(
      !os.exists(absent),
      "read-only snapshot should not create the directory"
    )
    assertSuccess(store.add(MemoryFile.Memory, "created by mutation"))
    assert(os.exists(absent), "a mutation should create the store directory")

  test("MemoryTool.run dispatches add and returns live JSON state"):
    val result = runTool(
      freshStore(),
      MemoryTool.Args("add", "memory", content = Some("via tool"))
    )
    assert(result("success").bool, result.render())
    assertEquals(result("entries").arr.toList.map(_.str), List("via tool"))

  test("MemoryTool.run rejects unknown action"):
    val result = runTool(
      freshStore(),
      MemoryTool.Args("wipe", "memory", content = Some("x"))
    )
    assert(!result("success").bool, result.render())

  test("concurrent adds on the same file all land"):
    val store = freshStore()
    val results =
      concurrently(32, 8)(i => store.add(MemoryFile.Memory, s"entry-$i"))
    assert(
      results.forall(_("success").bool),
      results.map(_.render()).mkString("\n")
    )
    assertAllPresent(store.read(MemoryFile.Memory), 32, "entry-")

  test("two stores on the same directory do not lose updates"):
    val dir = os.temp.dir(tmpRoot, prefix = "shared-").toIO
    val storeA = MemoryStore(dir)
    val storeB = MemoryStore(dir)
    val results = concurrently(40, 8): i =>
      (if i % 2 == 0 then storeA else storeB)
        .add(MemoryFile.Memory, s"shared-$i")
    assert(
      results.forall(_("success").bool),
      results.map(_.render()).mkString("\n")
    )
    assertAllPresent(MemoryStore(dir).read(MemoryFile.Memory), 40, "shared-")

  test("write creates and releases the lock file"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.Memory, "first"))
    val lockFile = base(store) / s"${MemoryFile.Memory.fileName}.lock"
    assert(os.exists(lockFile), "expected MEMORY.md.lock to be created")
    val userLock = base(store) / s"${MemoryFile.User.fileName}.lock"
    assert(
      !os.exists(userLock),
      "USER.md.lock should not exist before a user-store mutation"
    )
    val channel = java.nio.channels.FileChannel.open(
      lockFile.toNIO,
      java.nio.file.StandardOpenOption.CREATE,
      java.nio.file.StandardOpenOption.READ,
      java.nio.file.StandardOpenOption.WRITE
    )
    try
      val osLock = channel.tryLock()
      assert(osLock != null, "expected OS lock to have been released")
      osLock.release()
    finally channel.close()

  test("concurrent writes leave no temporary files in the base directory"):
    val store = freshStore()
    concurrently(16, 4)(i => store.add(MemoryFile.User, s"u-$i"))
      .foreach(assertSuccess)
    val leftovers =
      os.list(base(store)).map(_.last).filter(_.endsWith(".tmp")).toList
    assertEquals(leftovers, Nil, s"unexpected tmp files: $leftovers")

  test("add rejects content containing a line that is only §"):
    val store = freshStore()
    val result = store.add(MemoryFile.Memory, "before\n§\nafter")
    assertFailure(result)
    assert(result("error").str.contains("separator"), result.render())
    assertEquals(store.read(MemoryFile.Memory), "")

  test("add allows § inline within a line"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.Memory, "costs 5§ per unit"))
    assertEquals(store.read(MemoryFile.Memory), "costs 5§ per unit")

  test("add stores angle brackets and reserved tags verbatim (no guard)"):
    val store = freshStore()
    assertSuccess(
      store.add(MemoryFile.Memory, "increased memory usage when a < b")
    )
    assertSuccess(
      store.add(MemoryFile.Memory, "note </memory> and <user_profile>")
    )
    assertEquals(
      store.read(MemoryFile.Memory),
      "increased memory usage when a < b\n§\nnote </memory> and <user_profile>"
    )

  test("read returns content without locking (no lock file created)"):
    val store = freshStore()
    driftFile(store, "unlocked read")
    assertEquals(store.read(MemoryFile.Memory), "unlocked read")
    val lockFile = base(store) / s"${MemoryFile.Memory.fileName}.lock"
    assert(!os.exists(lockFile), "a read must not create the lock file")

  test("replace refuses when the replacement would exceed the cap"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.User, "short"))
    val result =
      store.replace(
        MemoryFile.User,
        "short",
        "x" * (MemoryFile.User.capacity + 1)
      )
    assertFailure(result)
    assert(result("error").str.contains("would put memory at"), result.render())
    assertEquals(result("current_entries").arr.toList.map(_.str), List("short"))
    assert(result.has("usage"), result.render())
    assertEquals(store.read(MemoryFile.User), "short")

  test(
    "an I/O failure during a mutation returns a structured error, not an exception"
  ):
    val store = freshStore()
    os.makeDir.all(base(store) / MemoryFile.Memory.fileName)
    val result = store.add(MemoryFile.Memory, "data")
    assertFailure(result)
    assert(
      result("error").str.contains(MemoryFile.Memory.fileName),
      result.render()
    )

  test(
    "an oversize on-disk entry is recoverable: remove works, add is blocked"
  ):
    val store = freshStore()
    writeRaw(store, MemoryFile.User, "x" * (MemoryFile.User.capacity + 50))
    val added = store.add(MemoryFile.User, "tiny")
    assertFailure(added)
    assert(!added.has("drift_backup"), added.render())
    assertSuccess(store.remove(MemoryFile.User, "xxx"))
    assertEquals(store.read(MemoryFile.User), "")

  test(
    "replace into content equal to another entry collapses instead of wedging the store"
  ):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.Memory, "alpha"))
    assertSuccess(store.add(MemoryFile.Memory, "beta"))
    assertSuccess(store.replace(MemoryFile.Memory, "alpha", "beta"))
    assertEquals(store.read(MemoryFile.Memory), "beta")
    assertSuccess(store.add(MemoryFile.Memory, "gamma"))
    assertEquals(store.read(MemoryFile.Memory), "beta\n§\ngamma")
    assertEquals(memBackups(store), Nil)

  test("snapshot on an unreadable store degrades to empty instead of throwing"):
    val store = freshStore()
    os.makeDir.all(base(store) / MemoryFile.Memory.fileName)
    assertEquals(store.snapshot(), MemorySnapshot.empty)

  test("read returns raw, parsed entries, and no drift for a clean file"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.Memory, "alpha"))
    assertSuccess(store.add(MemoryFile.Memory, "beta"))
    val result = store.inspect(MemoryFile.Memory)
    assertSuccess(result)
    assert(!result("drift").bool, result.render())
    assertEquals(entries(result), List("alpha", "beta"))
    assertEquals(result("raw").str, "alpha\n§\nbeta")
    assertEquals(result("backups").arr.toList, Nil)

  test("read on a drifted file flags drift and surfaces the backup inline"):
    val store = freshStore()
    driftFile(store, "manually edited \n§\nkept")
    val result = store.inspect(MemoryFile.Memory)
    assertSuccess(result)
    assert(result("drift").bool, result.render())
    val backups = result("backups").arr.toList
    assertEquals(backups.size, 1, result.render())
    assertEquals(backups.head("content").str, "manually edited \n§\nkept")
    assertEquals(memBackups(store).size, 1)

  test("reconcile refuses a file that is not in drift"):
    val store = freshStore()
    assertSuccess(store.add(MemoryFile.Memory, "alpha"))
    val result = store.reconcile(MemoryFile.Memory, "alpha\n§\nbeta")
    assertFailure(result)
    assert(result("error").str.contains("No drift"), result.render())
    assertEquals(store.read(MemoryFile.Memory), "alpha")

  test("reconcile repairs a drifted file and un-wedges the store"):
    val store = freshStore()
    driftFile(store, "manually edited \n§\nkept")
    val result =
      store.reconcile(MemoryFile.Memory, "manually edited\n§\nkept\n§\nnew")
    assertSuccess(result)
    assertEquals(result("message").str, "Reconciled.")
    assertEquals(
      store.read(MemoryFile.Memory),
      "manually edited\n§\nkept\n§\nnew"
    )
    assertSuccess(store.add(MemoryFile.Memory, "after"))

  test("reconcile refuses content that would exceed the cap"):
    val store = freshStore()
    writeRaw(store, MemoryFile.User, "drifted \n§\nentry")
    val result =
      store.reconcile(MemoryFile.User, "x" * (MemoryFile.User.capacity + 1))
    assertFailure(result)
    assert(result.has("usage"), result.render())
    assert(result.has("current_entries"), result.render())
    assertEquals(store.read(MemoryFile.User), "drifted \n§\nentry")

  test("reconcile drops a backup whose entries it integrated"):
    val store = freshStore()
    driftFile(store, "manually edited \n§\nkept")
    store.inspect(MemoryFile.Memory)
    assertSuccess(
      store.reconcile(MemoryFile.Memory, "manually edited\n§\nkept")
    )
    assertEquals(memBackups(store), Nil)

  test("reconcile keeps a backup whose entries it did not integrate"):
    val store = freshStore()
    driftFile(store, "manually edited \n§\nkept")
    store.inspect(MemoryFile.Memory)
    assertSuccess(store.reconcile(MemoryFile.Memory, "totally different"))
    assertEquals(memBackups(store).size, 1)

  test("reconcile with empty content clears the file but keeps the backup"):
    val store = freshStore()
    driftFile(store, "manually edited \n§\nkept")
    store.inspect(MemoryFile.Memory)
    assertSuccess(store.reconcile(MemoryFile.Memory, ""))
    assertEquals(store.read(MemoryFile.Memory), "")
    assertEquals(memBackups(store).size, 1)

  test("MemoryTool.run drives the full drift recovery: add → read → reconcile"):
    val store = freshStore()
    driftFile(store, "manually edited \n§\nkept")

    val blocked =
      runTool(store, MemoryTool.Args("add", "memory", content = Some("new")))
    assert(!blocked("success").bool, blocked.render())
    assert(blocked.obj.contains("drift_backup"), blocked.render())

    val read = runTool(store, MemoryTool.Args("read", "memory"))
    assert(read("success").bool, read.render())
    assert(read("drift").bool, read.render())
    assert(read("backups").arr.nonEmpty, read.render())

    val repaired = runTool(
      store,
      MemoryTool.Args(
        "reconcile",
        "memory",
        content = Some("manually edited\n§\nkept")
      )
    )
    assert(repaired("success").bool, repaired.render())

    val added =
      runTool(store, MemoryTool.Args("add", "memory", content = Some("new")))
    assert(added("success").bool, added.render())

  test("MemoryTool.run reconcile requires the content field"):
    val result = runTool(freshStore(), MemoryTool.Args("reconcile", "memory"))
    assert(!result("success").bool, result.render())
