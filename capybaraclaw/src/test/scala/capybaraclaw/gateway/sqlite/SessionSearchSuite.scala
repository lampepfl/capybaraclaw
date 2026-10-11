package capybaraclaw.gateway.sqlite

import capybaraclaw.gateway.*
import tacit.agents.llm.endpoint.Message

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicLong

/** Session search against a real SQLite database. Message ids follow append
  * order from 1, and every write advances the clock by one millisecond.
  */
class SessionSearchSuite extends munit.FunSuite:
  private val WD = "/work"
  private val Me: SessionScope.Direct =
    SessionScope.Direct(PortId("cli"), UserId("me"))

  /** What a private session of `Me` in `WD` may see. */
  private def mine(current: SessionId = SessionId.random()): SearchScope =
    SearchScope(current, WD, linked("cli:me"))

  /** Private sessions of these `port:user` accounts, as in
    * `identities.json`.
    */
  private def linked(accounts: String*): Visibility =
    Visibility.DirectWith(accounts.toSet.map: a =>
      val i = a.indexOf(':')
      PortId(a.take(i)) -> UserId(a.drop(i + 1)))

  private class Fixture(val provider: SqliteContextProvider):
    private val nextId = AtomicLong(0)

    /** A private session of `Me` with alternating user and assistant
      * messages; returns its id and the ids of its messages.
      */
    def session(texts: String*): (SessionId, List[Long]) =
      sessionIn(Me)(texts*)

    def sessionIn(scope: SessionScope, workdir: String = WD)(
        texts: String*
    ): (SessionId, List[Long]) =
      val sid = provider.createSession(workdir)
      provider.recordScope(sid, scope)
      (sid, appendAll(sid, texts))

    /** A session recorded before scopes existed. */
    def unscoped(texts: String*): (SessionId, List[Long]) =
      val sid = provider.createSession(WD)
      (sid, appendAll(sid, texts))

    private def appendAll(sid: SessionId, texts: Seq[String]): List[Long] =
      val ids = texts.toList.zipWithIndex.map: (text, i) =>
        provider.append(
          sid,
          if i % 2 == 0 then Message.user(text) else Message.assistant(text)
        )
        nextId.incrementAndGet()
      ids

    def discover(
        terms: SearchTerms,
        limit: Int = 10,
        offset: Int = 0,
        window: Int = 2,
        sort: SearchSort = SearchSort.Rank,
        scope: SearchScope = mine()
    ): Discovery =
      provider.discover(terms, limit, offset, window, sort, scope)

  private val fixture = FunFixture[Fixture](
    setup = _ =>
      val ticks = AtomicLong(0)
      Fixture(
        SqliteContextProvider(
          Files.createTempDirectory("claw-session-search"),
          () => ticks.incrementAndGet()
        )
      )
    ,
    teardown = _.provider.close()
  )

  private def all(terms: String*) = SearchTerms(allOf = terms.toList)

  fixture.test(
    "discover gives one hit per session, anchored on the best match"
  ): f =>
    val (a, aIds) = f.session(
      "[u] tell me about docker",
      "docker is a container tool, docker docker docker",
      "[u] thanks"
    )
    val (b, _) = f.session("[u] nothing relevant", "still nothing")
    val (c, _) = f.session("[u] docker once", "ok")
    val found = f.discover(all("docker"))
    assertEquals(found.totalSessions, 2)
    assertEquals(found.hits.map(_.session.sessionId).toSet, Set(a, c))
    val hit = found.hits.find(_.session.sessionId == a).get
    assertEquals(hit.matchedMessages, 2)
    assertEquals(hit.matches.map(_.message.id), aIds.take(2))
    assertEquals(hit.matches(1).matchCount, 4)
    assertEquals(hit.session.title, "tell me about docker")
    assert(!found.hits.exists(_.session.sessionId == b))

  fixture.test("discover excludes the current session"): f =>
    val (a, _) = f.session("[u] docker here")
    val (b, _) = f.session("[u] docker there")
    val found = f.discover(all("docker"), scope = mine(a))
    assertEquals(found.hits.map(_.session.sessionId), List(b))
    assertEquals(found.totalSessions, 1)

  fixture.test("discover pages with limit and offset over the total"): f =>
    val sessions = (1 to 5).map(i => f.session(s"[u] kubernetes note $i")._1)
    val first =
      f.discover(all("kubernetes"), limit = 2, sort = SearchSort.Oldest)
    val third = f.discover(
      all("kubernetes"),
      limit = 2,
      offset = 4,
      sort = SearchSort.Oldest
    )
    assertEquals(first.totalSessions, 5)
    assertEquals(first.hits.map(_.session.sessionId), sessions.take(2).toList)
    assertEquals(third.hits.map(_.session.sessionId), List(sessions(4)))
    assertEquals(third.totalSessions, 5)

  fixture.test(
    "newest and oldest sort by a session's latest and earliest match"
  ): f =>
    val (early, _) = f.session("[u] redis early")
    val (later, _) = f.session("[u] redis later")
    f.provider.append(later, Message.user("[u] redis latest of all"))
    val newest = f.discover(all("redis"), sort = SearchSort.Newest)
    val oldest = f.discover(all("redis"), sort = SearchSort.Oldest)
    assertEquals(newest.hits.map(_.session.sessionId), List(later, early))
    assertEquals(oldest.hits.map(_.session.sessionId), List(early, later))

  fixture.test("prefix matching and exclusions"): f =>
    val (deploy, _) = f.session("[u] the deployment failed")
    val (both, _) = f.session("[u] deployment of staging")
    val exact = f.discover(all("deploy"))
    val prefixed =
      f.discover(SearchTerms(allOf = List("deploy"), prefix = true))
    val excluded = f.discover(
      SearchTerms(
        allOf = List("deploy"),
        noneOf = List("staging"),
        prefix = true
      )
    )
    assertEquals(exact.hits, Nil)
    assertEquals(
      prefixed.hits.map(_.session.sessionId).toSet,
      Set(deploy, both)
    )
    assertEquals(excluded.hits.map(_.session.sessionId), List(deploy))

  fixture.test("a long message is shown as fragments with its full length"):
    f =>
      val filler = (1 to 400).map(i => s"word$i").mkString(" ")
      val long = s"$filler needle $filler"
      f.session("[u] question", long)
      val m = f.discover(all("needle")).hits.head.matches.head
      assertEquals(m.message.length, long.length)
      assertEquals(m.matchCount, 1)
      assertEquals(m.fragments.size, 1)
      assert(m.fragments.head.contains("«needle»"), m.fragments.head)
      assert(m.fragments.head.length < 600, m.fragments.head.length)

  fixture.test("fragments beyond the per-message limit are counted"): f =>
    val filler = (1 to 100).map(i => s"pad$i").mkString(" ")
    val text = (1 to 8).map(_ => s"needle $filler").mkString(" ")
    f.session(s"[u] $text")
    val m = f.discover(all("needle")).hits.head.matches.head
    assertEquals(m.matchCount, 8)
    assertEquals(m.fragments.size, SessionSearch.FragmentsPerMessage)
    assertEquals(m.moreFragments, 8 - SessionSearch.FragmentsPerMessage)

  fixture.test(
    "context lists messages around the best match, in its session only"
  ): f =>
    val (_, before) = f.session("[u] other session a", "b")
    val (_, ids) =
      f.session("[u] m1", "m2", "[u] m3 needle", "m4", "[u] m5", "m6")
    val (_, after) = f.session("[u] other session c")
    val hit = f.discover(all("needle"), window = 1).hits.head
    assertEquals(hit.context.map(_.id), ids.slice(1, 4))
    assertEquals(
      hit.context.map(_.role),
      List("assistant", "user", "assistant")
    )
    assertEquals(hit.context.map(_.length), List(2, 13, 2))
    val edge = f.discover(all("needle"), window = 5).hits.head
    assertEquals(edge.context.map(_.id), ids)
    assert(before.nonEmpty && after.nonEmpty)

  fixture.test("scroll returns metadata by default and text when asked"): f =>
    val (sid, ids) = f.session("[u] one", "two", "[u] three", "four")
    val meta = f.provider.scroll(sid, ids(1), 1, fullText = false, mine()).get
    assertEquals(meta.messages.map(_.message.id), ids.take(3))
    assert(meta.messages.forall(_.text.isEmpty))
    assertEquals(meta.messages.map(_.message.length), List(7, 3, 9))
    val full = f.provider.scroll(sid, ids(1), 1, fullText = true, mine()).get
    assertEquals(
      full.messages.flatMap(_.text),
      List("[u] one", "two", "[u] three")
    )
    assertEquals(full.session.title, "one")

  fixture.test("scroll refuses a message id from another session"): f =>
    val (a, _) = f.session("[u] in a")
    val (_, bIds) = f.session("[u] in b")
    assertEquals(
      f.provider.scroll(a, bIds.head, 5, fullText = true, mine()),
      None
    )
    assertEquals(f.provider.scroll(a, 999L, 5, fullText = true, mine()), None)
    assertEquals(
      f.provider
        .scroll(SessionId.random(), bIds.head, 5, fullText = true, mine()),
      None
    )

  fixture.test(
    "get returns whole messages in id order, skipping the current session"
  ): f =>
    val (a, aIds) = f.session("[u] alpha", "beta")
    val (b, bIds) = f.session("[u] gamma")
    val got = f.provider.get(List(bIds.head, aIds(1), 999L, aIds.head), mine(a))
    assertEquals(got.map(_.message.id), bIds)
    assertEquals(got.map(_.text), List("[u] gamma"))
    assertEquals(got.map(_.sessionId), List(b))
    val all = f.provider.get(List(bIds.head, aIds(1)), mine())
    assertEquals(all.map(_.text), List("beta", "[u] gamma"))

  fixture.test("browse orders by last activity, excluding the current session"):
    f =>
      val (a, _) = f.session("[u] first", "reply")
      val (b, _) = f.session()
      val (c, _) = f.session("[u] third")
      f.provider.verifyAndTouchSession(a, WD)
      val sessions = f.provider.browse(10, 0, mine(c))
      assertEquals(sessions.map(_.sessionId), List(a, b))
      assertEquals(sessions.map(_.messageCount), List(2, 0))
      assertEquals(sessions.map(_.title), List("first", "(untitled session)"))

  /** Whether `scope` finds `target`, whose messages include "needle", in each
    * mode.
    */
  private def findsIn(
      f: Fixture,
      scope: SearchScope,
      target: SessionId,
      ids: List[Long]
  ): Map[String, Boolean] =
    val discovered = f.discover(all("needle"), scope = scope)
    Map(
      "discover" -> discovered.hits.exists(_.session.sessionId == target),
      "count" -> (discovered.totalSessions > 0),
      "browse" -> f.provider
        .browse(25, 0, scope)
        .exists(_.sessionId == target),
      "scroll" -> f.provider.scroll(target, ids.head, 5, true, scope).isDefined,
      "get" -> f.provider.get(ids, scope).nonEmpty
    )

  private def assertFinds(found: Map[String, Boolean], expected: Boolean)(using
      munit.Location
  ): Unit =
    assertEquals(found, found.map((mode, _) => mode -> expected))

  fixture.test(
    "a private session is found only by its person, in every mode"
  ): f =>
    val alice = SessionScope.Direct(PortId("slack"), UserId("U_alice"))
    val (target, ids) = f.sessionIn(alice)("[u] the needle salary", "ok")
    def as(accounts: String*) =
      SearchScope(SessionId.random(), WD, linked(accounts*))
    assertFinds(
      findsIn(f, as("slack:U_alice", "cli:alice"), target, ids),
      true
    )
    assertFinds(findsIn(f, as("cli:alice"), target, ids), false)
    assertFinds(findsIn(f, as("slack:U_bob"), target, ids), false)
    assertFinds(findsIn(f, as(), target, ids), false)
    val channel = SearchScope(
      SessionId.random(),
      WD,
      Visibility.InChannel(PortId("slack"), "C1")
    )
    assertFinds(findsIn(f, channel, target, ids), false)

  fixture.test("a shared session is found only from its channel"): f =>
    val (target, ids) =
      f.sessionIn(SessionScope.Group(PortId("slack"), "C1"))("[u] needle")
    def in(port: String, channel: String) = SearchScope(
      SessionId.random(),
      WD,
      Visibility.InChannel(PortId(port), channel)
    )
    assertFinds(findsIn(f, in("slack", "C1"), target, ids), true)
    assertFinds(findsIn(f, in("slack", "C2"), target, ids), false)
    assertFinds(findsIn(f, in("discord", "C1"), target, ids), false)
    val direct = SearchScope(
      SessionId.random(),
      WD,
      linked("slack:C1", "slack:U_alice")
    )
    assertFinds(findsIn(f, direct, target, ids), false)

  fixture.test("a session in another workdir is never found"): f =>
    val (target, ids) = f.sessionIn(Me, workdir = "/other")("[u] needle")
    assertFinds(findsIn(f, mine(), target, ids), false)
    val there =
      SearchScope(
        SessionId.random(),
        "/other",
        linked("cli:me")
      )
    assertFinds(findsIn(f, there, target, ids), true)

  fixture.test("a session without a recorded scope is found by nobody"): f =>
    val (target, ids) = f.unscoped("[u] needle")
    assertFinds(findsIn(f, mine(), target, ids), false)
    val (own, ownIds) = f.session("[u] needle")
    assertFinds(findsIn(f, mine(), own, ownIds), true)
    val nobody = SearchScope(SessionId.random(), WD, Visibility.NoSessions)
    assertFinds(findsIn(f, nobody, own, ownIds), false)

  fixture.test("the first recorded scope stays"): f =>
    val sid = f.provider.createSession(WD)
    f.provider.recordScope(sid, SessionScope.Group(PortId("slack"), "C1"))
    f.provider.recordScope(sid, Me)
    assertEquals(
      f.provider.scopeOf(sid),
      Some(SessionScope.Group(PortId("slack"), "C1"))
    )
    assertEquals(f.provider.scopeOf(f.provider.createSession(WD)), None)
    assertEquals(f.provider.scopeOf(SessionId.random()), None)

  fixture.test(
    "unscoped sessions without a handle go to the operator, the rest stay unscoped"
  ): f =>
    val cli = f.provider.createSession(WD)
    val slack = f.provider.resolveOrCreateHandle(
      WD,
      SessionHandle(PortId("slack"), "C1/1")
    )
    val (scoped, _) =
      f.sessionIn(SessionScope.Group(PortId("slack"), "C2"))()
    assertEquals(f.provider.adoptUnscopedCliSessions(Me), 1)
    assertEquals(f.provider.scopeOf(cli), Some(Me))
    assertEquals(f.provider.scopeOf(slack), None)
    assertEquals(
      f.provider.scopeOf(scoped),
      Some(SessionScope.Group(PortId("slack"), "C2"))
    )
    assertEquals(f.provider.adoptUnscopedCliSessions(Me), 0)

  fixture.test(
    "only a session's own person, or anyone for a new session, may attach"
  ): f =>
    val bob: SessionScope.Direct =
      SessionScope.Direct(PortId("cli"), UserId("bob"))
    val fresh = f.provider.createSession(WD)
    val (own, _) = f.session("[u] mine")
    val (channel, _) =
      f.sessionIn(SessionScope.Group(PortId("slack"), "C1"))("[u] hi")
    val handled = f.provider.resolveOrCreateHandle(
      WD,
      SessionHandle(PortId("slack"), "C2/1")
    )
    assert(f.provider.canAttach(fresh, Me))
    assert(f.provider.canAttach(own, Me))
    assert(!f.provider.canAttach(own, bob))
    assert(!f.provider.canAttach(channel, Me))
    assert(!f.provider.canAttach(handled, Me))
    assert(!f.provider.canAttach(SessionId.random(), Me))
