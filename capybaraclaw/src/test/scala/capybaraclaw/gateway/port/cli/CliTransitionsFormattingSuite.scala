package capybaraclaw.gateway.port.cli

import capybaraclaw.Throwables
import munit.FunSuite

class CliTransitionsFormattingSuite extends FunSuite:

  /** formatDuration */

  test("formatDuration: under a minute → seconds only"):
    assertEquals(CliTransitions.formatDuration(0), "0s")
    assertEquals(CliTransitions.formatDuration(1), "1s")
    assertEquals(CliTransitions.formatDuration(45), "45s")
    assertEquals(CliTransitions.formatDuration(59), "59s")

  test("formatDuration: minutes range → 'Xm Ys' with both pieces"):
    assertEquals(CliTransitions.formatDuration(60), "1m 0s")
    assertEquals(CliTransitions.formatDuration(61), "1m 1s")
    assertEquals(CliTransitions.formatDuration(150), "2m 30s")
    assertEquals(CliTransitions.formatDuration(3599), "59m 59s")

  test("formatDuration: hours range → 'Xh Ym', no seconds"):
    assertEquals(CliTransitions.formatDuration(3600), "1h 0m")
    assertEquals(CliTransitions.formatDuration(7320), "2h 2m")
    assertEquals(CliTransitions.formatDuration(86400), "24h 0m")

  /** selectThinkingWord */

  test("selectThinkingWord: at t=0 returns the start word"):
    assertEquals(
      CliTransitions.selectThinkingWord(0, 0),
      CliTransitions.ThinkingWords(0)
    )
    assertEquals(
      CliTransitions.selectThinkingWord(5, 0),
      CliTransitions.ThinkingWords(5)
    )

  test("selectThinkingWord: rotates every ThinkingWordRotateMs"):
    val rotateMs = CliTransitions.ThinkingWordRotateMs
    assertEquals(
      CliTransitions.selectThinkingWord(0, rotateMs - 1),
      CliTransitions.ThinkingWords(0)
    )
    assertEquals(
      CliTransitions.selectThinkingWord(0, rotateMs),
      CliTransitions.ThinkingWords(1)
    )
    assertEquals(
      CliTransitions.selectThinkingWord(0, 2 * rotateMs),
      CliTransitions.ThinkingWords(2)
    )

  test("selectThinkingWord: wraps modulo word count"):
    val rotateMs = CliTransitions.ThinkingWordRotateMs
    val wordCount = CliTransitions.ThinkingWords.size
    assertEquals(
      CliTransitions.selectThinkingWord(0, rotateMs * wordCount),
      CliTransitions.ThinkingWords(0)
    )
    assertEquals(
      CliTransitions.selectThinkingWord(wordCount - 1, rotateMs),
      CliTransitions.ThinkingWords(0)
    )

  /** spinnerFrameAt */

  test("spinnerFrameAt: most ticks render frame 0"):
    assertEquals(
      CliTransitions.spinnerFrameAt(0),
      CliTransitions.SpinnerFrames(0)
    )
    assertEquals(
      CliTransitions.spinnerFrameAt(1),
      CliTransitions.SpinnerFrames(0)
    )
    assertEquals(
      CliTransitions.spinnerFrameAt(12),
      CliTransitions.SpinnerFrames(0)
    )

  test("spinnerFrameAt: blink frame on every BlinkEvery-th tick"):
    val blink = CliTransitions.SpinnerBlinkEveryTicks
    assertEquals(
      CliTransitions.spinnerFrameAt(blink - 1),
      CliTransitions.SpinnerFrames(1)
    )
    assertEquals(
      CliTransitions.spinnerFrameAt(2 * blink - 1),
      CliTransitions.SpinnerFrames(1)
    )
    assertEquals(
      CliTransitions.spinnerFrameAt(blink),
      CliTransitions.SpinnerFrames(0)
    )

  /** errorMessage */

  test("errorMessage: uses the throwable's message when present"):
    assertEquals(Throwables.errorMessage(RuntimeException("boom")), "boom")
    assertEquals(
      Throwables.errorMessage(IllegalArgumentException("bad arg")),
      "bad arg"
    )

  test(
    "errorMessage: falls back to class simple name when message is null/empty"
  ):
    assertEquals(
      Throwables.errorMessage(RuntimeException()),
      "RuntimeException"
    )
    assertEquals(
      Throwables.errorMessage(IllegalStateException("")),
      "IllegalStateException"
    )

  /** prepareEntryLines */

  test("prepareEntryLines: filters blank lines"):
    assertEquals(
      CliTransitions.prepareEntryLines("a\n\nb\nc"),
      List("a", "b", "c")
    )
    assertEquals(
      CliTransitions.prepareEntryLines("\nhello\n\nworld\n"),
      List("hello", "world")
    )

  test("prepareEntryLines: empty input becomes single empty line"):
    assertEquals(CliTransitions.prepareEntryLines(""), List(""))
    assertEquals(CliTransitions.prepareEntryLines("\n\n\n"), List(""))

  test("prepareEntryLines: preserves a single non-empty line"):
    assertEquals(CliTransitions.prepareEntryLines("solo"), List("solo"))

  /** compactArgs / formatToolCall */

  test("compactArgs: collapses whitespace and newlines to single spaces"):
    assertEquals(
      CliTransitions.compactArgs("{\n  \"a\": 1,\n  \"b\": 2\n}"),
      "{ \"a\": 1, \"b\": 2 }"
    )

  test("compactArgs: truncates overly long args with an ellipsis"):
    val long = "x" * 200
    val out = CliTransitions.compactArgs(long)
    assertEquals(out.length, CliTransitions.ToolArgsMaxLen)
    assert(out.endsWith("…"))

  test("formatToolCall: renders a single name(args) line"):
    assertEquals(
      CliTransitions.formatToolCall("mem", "{\"q\":1}"),
      "mem({\"q\":1})"
    )

  test("formatToolCall: truncates long args with an ellipsis"):
    val out = CliTransitions.formatToolCall("t", "y" * 500)
    assert(out.startsWith("t("))
    assert(out.endsWith("…)"))
