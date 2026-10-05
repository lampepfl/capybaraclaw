package capybaraclaw.agent.tools

import capybaraclaw.gateway.SessionId
import capybaraclaw.gateway.sqlite.SqliteContextProvider
import tacit.agents.llm.endpoint.Message
import tacit.agents.llm.utils.IsToolArg

import java.nio.file.Files

/** The tool from the JSON the model sends to the JSON it gets back. */
class SessionSearchToolSuite extends munit.FunSuite:
  private val provider = FunFixture[SqliteContextProvider](
    setup =
      _ => SqliteContextProvider(Files.createTempDirectory("claw-search-tool")),
    teardown = _.close()
  )

  private def call(
      p: SqliteContextProvider,
      current: SessionId,
      json: String
  ): ujson.Value =
    val args = summon[IsToolArg[SessionSearchTool.Args]]
      .parse(json)
      .fold(e => fail(s"args did not parse: $e"), identity)
    ujson.read(SessionSearchTool.run(p, current, args))

  /** A session with one message per text; returns its id. */
  private def session(p: SqliteContextProvider, texts: String*): SessionId =
    val sid = p.createSession("/work")
    texts.foreach(t => p.append(sid, Message.user(t)))
    sid

  private def error(result: ujson.Value): String =
    assertEquals(result("success").bool, false, result.render())
    result("error").str

  test("the schema names every parameter in snake_case"):
    val names = summon[IsToolArg[SessionSearchTool.Args]].schema.properties
    assertEquals(
      names.keySet,
      Set(
        "all_of",
        "any_of",
        "none_of",
        "prefix",
        "session_id",
        "around_message_id",
        "full_text",
        "message_ids",
        "limit",
        "offset",
        "window",
        "sort"
      )
    )
    assertEquals(names("around_message_id").`type`, "integer")
    assertEquals(names("message_ids").items.map(_.`type`), Some("integer"))
    assert(
      SessionSearchTool.description.contains("around_message_id") &&
        !SessionSearchTool.description.contains("aroundMessageId")
    )

  provider.test("discover returns fragments and lengths, never whole messages"):
    p =>
      val filler = (1 to 300).map(i => s"word$i").mkString(" ")
      val long = s"$filler needle $filler"
      val past = session(p, long)
      val current = session(p, "[u] now")
      val result = call(p, current, """{"all_of": ["needle"]}""")
      assert(result("success").bool, result.render())
      assertEquals(result("total_sessions").num, 1d)
      val hit = result("results")(0)
      assertEquals(hit("session_id").str, past.toString)
      val m = hit("matches")(0)
      assertEquals(m("length").num, long.length.toDouble)
      assert(m("fragments")(0).str.contains("«needle»"))
      assert(!result.render().contains(long), "the whole message leaked")
      assert(!hit("context")(0).obj.contains("text"))
      assert(!result.obj.contains("next_offset"))

  provider.test("discover reports the next offset while results remain"): p =>
    (1 to 3).foreach(i => session(p, s"[u] topic $i"))
    val current = session(p, "[u] now")
    val result = call(p, current, """{"all_of": ["topic"], "limit": 2}""")
    assertEquals(result("total_sessions").num, 3d)
    assertEquals(result("count").num, 2d)
    assertEquals(result("next_offset").num, 2d)

  provider.test("get fetches whole messages by id"): p =>
    val past = session(p, "[u] the full story")
    val current = session(p, "[u] now")
    val result = call(p, current, """{"message_ids": [1, 2, 77]}""")
    assertEquals(
      result("messages").arr.map(_("text").str).toList,
      List("[u] the full story")
    )
    assertEquals(result("messages")(0)("session_id").str, past.toString)
    assertEquals(result("not_found").arr.map(_.num).toList, List(2d, 77d))

  provider.test("get refuses too many ids"): p =>
    val current = session(p, "[u] now")
    val ids = (1 to 21).mkString("[", ", ", "]")
    assert(
      error(call(p, current, s"""{"message_ids": $ids}"""))
        .contains("at most 20")
    )

  provider.test("scroll takes session_id and around_message_id"): p =>
    val past = session(p, "[u] one", "[u] two", "[u] three")
    val current = session(p, "[u] now")
    val meta = call(
      p,
      current,
      s"""{"session_id": "$past", "around_message_id": 2, "window": 1}"""
    )
    assertEquals(meta("messages").arr.map(_("id").num).toList, List(1d, 2d, 3d))
    assert(meta("messages").arr.forall(!_.obj.contains("text")))
    assertEquals(
      meta("messages").arr.map(_("anchor").bool).toList,
      List(false, true, false)
    )
    val full = call(
      p,
      current,
      s"""{"session_id": "$past", "around_message_id": 2, "window": 1, "full_text": true}"""
    )
    assertEquals(
      full("messages").arr.map(_("text").str).toList,
      List("[u] one", "[u] two", "[u] three")
    )

  provider.test("half of the scroll arguments is an error naming the other"):
    p =>
      val current = session(p, "[u] now")
      assert(
        error(call(p, current, s"""{"session_id": "$current"}"""))
          .contains("around_message_id")
      )
      assert(
        error(call(p, current, """{"around_message_id": 3}""")).contains(
          "session_id"
        )
      )

  provider.test("an unknown sort is an error, not a silent fallback"): p =>
    val current = session(p, "[u] now")
    val message =
      error(call(p, current, """{"all_of": ["x"], "sort": "date"}"""))
    assert(message.contains("'date'") && message.contains("newest"), message)

  provider.test("mixing modes is an error"): p =>
    val current = session(p, "[u] now")
    assert(
      error(call(p, current, """{"all_of": ["x"], "message_ids": [1]}"""))
        .contains("one mode at a time")
    )
