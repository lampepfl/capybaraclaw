package capybaraclaw.gateway.port.cli

import capybaraclaw.gateway.{
  GatewayMessage,
  Origin,
  SessionId,
  SessionRef,
  UserId
}
import munit.FunSuite

class CliTransitionsSuite extends FunSuite:

  import CliTransitions.*
  import CliTransitions.CliEffect.*

  private val ctx = TransitionContext(
    now = 1_000_000L,
    newSpinnerWordIdx = 7,
    shouldRenderSpinner = true,
    origin = Origin(
      CliPort.Id,
      UserId("tester"),
      SessionRef.Direct(SessionId.random())
    )
  )

  private val idle: State = State.initial

  private val midTurn: State = State.initial.copy(
    turnInFlight = true,
    spinner = Some(
      SpinnerState(startedAtMillis = 500_000L, wordStartIdx = 3, frameTick = 5)
    ),
    turnCount = 1
  )

  /** UserInput */

  test("UserInput empty/whitespace: no-op"):
    val r = transition(idle, UserInput("   "), ctx)
    assertEquals(r.state, idle)
    assertEquals(r.effects, Nil)

  test("UserInput quit: marks running=false, no further effects"):
    val r = transition(idle, UserInput("quit"), ctx)
    assertEquals(r.state.running, false)
    assertEquals(r.effects, Nil)

  test("UserInput /exit: also a quit"):
    val r = transition(idle, UserInput("/exit"), ctx)
    assertEquals(r.state.running, false)

  test("UserInput /sessions: emits RenderSessionsList, state unchanged"):
    val r = transition(idle, UserInput("/sessions"), ctx)
    assertEquals(r.state, idle)
    assertEquals(r.effects, List(RenderSessionsList))

  test("UserInput /current: emits RenderCurrentInfo, state unchanged"):
    val r = transition(idle, UserInput("/current"), ctx)
    assertEquals(r.state, idle)
    assertEquals(r.effects, List(RenderCurrentInfo))

  test("UserInput /sessions during turn-in-flight: still listed"):
    val r = transition(midTurn, UserInput("/sessions"), ctx)
    assertEquals(r.state, midTurn)
    assertEquals(r.effects, List(RenderSessionsList))

  test("UserInput /garbage: emits Unknown command error"):
    val r = transition(idle, UserInput("/garbage"), ctx)
    assertEquals(r.state, idle)
    assertEquals(
      r.effects,
      List(Render(Role.Error, "Unknown command: /garbage"))
    )

  test("UserInput while turn-in-flight: renders error, state unchanged"):
    val r = transition(midTurn, UserInput("hi"), ctx)
    assertEquals(r.state, midTurn)
    assertEquals(
      r.effects,
      List(Render(Role.Error, "Turn already in progress. Please wait."))
    )

  test("UserInput from idle: kicks off a turn"):
    val r = transition(idle, UserInput("hello"), ctx)
    assertEquals(r.state.turnInFlight, true)
    assertEquals(r.state.turnCount, 1)
    assert(r.state.spinner.isDefined)
    val sp = r.state.spinner.get
    assertEquals(sp.startedAtMillis, ctx.now)
    assertEquals(sp.wordStartIdx, ctx.newSpinnerWordIdx)
    assertEquals(sp.frameTick, 0)

  test(
    "UserInput from idle: emits Send, RenderSpinner, SetEcho(false), StartSpinner"
  ):
    val r = transition(idle, UserInput("hello"), ctx)
    val sp = r.state.spinner.get
    assertEquals(
      r.effects,
      List(
        SendOutbound(GatewayMessage(ctx.origin, "hello")),
        RenderSpinner(sp, ctx.now),
        SetEcho(false),
        StartSpinnerFiber
      )
    )

  test("UserInput from idle: increments turnCount"):
    val started = idle.copy(turnCount = 5)
    val r = transition(started, UserInput("hi"), ctx)
    assertEquals(r.state.turnCount, 6)

  /** AssistantText / ErrorText */

  test("AssistantText while running: emits Render(Assistant)"):
    val r = transition(midTurn, AssistantText("answer"), ctx)
    assertEquals(r.state, midTurn)
    assertEquals(r.effects, List(Render(Role.Assistant, "answer")))

  test("ErrorText while running: emits Render(Error)"):
    val r = transition(midTurn, ErrorText("oops"), ctx)
    assertEquals(r.state, midTurn)
    assertEquals(r.effects, List(Render(Role.Error, "oops")))

  test("Assistant/Error text after running=false: no effects"):
    val stopped = idle.copy(running = false)
    assertEquals(transition(stopped, AssistantText("x"), ctx).effects, Nil)
    assertEquals(transition(stopped, ErrorText("x"), ctx).effects, Nil)

  /** TurnFinished */

  test(
    "TurnFinished with active spinner: cancels fiber, restores echo, stops spinner"
  ):
    val r = transition(midTurn, TurnFinished, ctx)
    assertEquals(r.state.spinner, None)
    assertEquals(r.state.turnInFlight, false)
    assertEquals(r.state.turnCount, midTurn.turnCount) // unchanged
    assertEquals(
      r.effects,
      List(CancelSpinnerFiber, SetEcho(true), StopSpinner)
    )

  test(
    "TurnFinished without spinner: still restores echo and stops (idempotent)"
  ):
    val noSpinner = midTurn.copy(spinner = None)
    val r = transition(noSpinner, TurnFinished, ctx)
    assertEquals(r.state.turnInFlight, false)
    assertEquals(r.effects, List(SetEcho(true), StopSpinner))

  /** SpinnerTick */

  test(
    "SpinnerTick when spinner active and may render: increments tick, emits RenderSpinner"
  ):
    val r = transition(midTurn, SpinnerTick(2_000_000L), ctx)
    val before = midTurn.spinner.get
    val after = r.state.spinner.get
    assertEquals(after.frameTick, before.frameTick + 1)
    assertEquals(r.effects, List(RenderSpinner(before, 2_000_000L)))

  test(
    "SpinnerTick when shouldRenderSpinner=false: still increments tick, no Render"
  ):
    val ctxNoRender = ctx.copy(shouldRenderSpinner = false)
    val r = transition(midTurn, SpinnerTick(2_000_000L), ctxNoRender)
    val after = r.state.spinner.get
    assertEquals(after.frameTick, midTurn.spinner.get.frameTick + 1)
    assertEquals(r.effects, Nil)

  test("SpinnerTick without active spinner: no-op"):
    val r = transition(idle, SpinnerTick(ctx.now), ctx)
    assertEquals(r.state, idle)
    assertEquals(r.effects, Nil)

  /** InputReadFailed */

  test("InputReadFailed while running: emits error render, state unchanged"):
    val err = RuntimeException("disk on fire")
    val r = transition(idle, InputReadFailed(err), ctx)
    assertEquals(r.state, idle)
    assertEquals(
      r.effects,
      List(Render(Role.Error, "Input reader failed: disk on fire"))
    )

  test("InputReadFailed after stop: no effects"):
    val stopped = idle.copy(running = false)
    val r = transition(stopped, InputReadFailed(RuntimeException("x")), ctx)
    assertEquals(r.effects, Nil)

  /** InputClosed / ShutdownRequested */

  test("InputClosed: stops the loop and cancels active spinner"):
    val r = transition(midTurn, InputClosed, ctx)
    assertEquals(r.state.running, false)
    assertEquals(r.state.spinner, None)
    assertEquals(r.effects, List(CancelSpinnerFiber))

  test("ShutdownRequested: same shape as InputClosed"):
    val r = transition(midTurn, ShutdownRequested, ctx)
    assertEquals(r.state.running, false)
    assertEquals(r.state.spinner, None)
    assertEquals(r.effects, List(CancelSpinnerFiber))

  test("InputClosed when idle: no spinner cancel emitted"):
    val r = transition(idle, InputClosed, ctx)
    assertEquals(r.state.running, false)
    assertEquals(r.effects, Nil)
