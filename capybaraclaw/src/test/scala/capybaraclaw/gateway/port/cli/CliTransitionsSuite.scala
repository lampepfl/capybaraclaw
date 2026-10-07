package capybaraclaw.gateway.port.cli

import capybaraclaw.gateway.{
  Permission,
  ApprovalDecision,
  ApprovalReply,
  ApprovalRequest,
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

  /** Permission requests */

  test(
    "UserInput /approve: sends an approval for the latest request and starts a turn"
  ):
    val r = transition(idle, UserInput("/approve"), ctx)
    assert(r.state.turnInFlight)
    assertEquals(r.state.turnCount, idle.turnCount)
    assertEquals(
      r.effects.head,
      SendOutbound(ApprovalReply(ctx.origin, None, ApprovalDecision.Approve))
    )

  test("UserInput /deny #2: sends a denial of request 2"):
    val r = transition(idle, UserInput("/deny #2"), ctx)
    assertEquals(
      r.effects.head,
      SendOutbound(ApprovalReply(ctx.origin, Some(2), ApprovalDecision.Deny))
    )

  test("UserInput /approve with a bad id: usage error, state unchanged"):
    val r = transition(idle, UserInput("/approve soon"), ctx)
    assertEquals(r.state, idle)
    assertEquals(
      r.effects,
      List(
        Render(Role.Error, "Usage: /approve [request number], e.g. /approve 3.")
      )
    )

  private val request =
    ApprovalRequest(
      3,
      SessionId.random(),
      Permission.Files("/data", Permission.FileAccess.ReadWrite)
    )

  test("ApprovalRequested while running: queues the request for the menu"):
    val r = transition(midTurn, ApprovalRequested(request), ctx)
    assertEquals(r.state, midTurn.copy(pendingApprovals = List(request)))
    assertEquals(r.effects, Nil)

  test("ApprovalRequested without menus: tells how to answer"):
    val r = transition(
      midTurn,
      ApprovalRequested(request),
      ctx.copy(menusSupported = false)
    )
    assertEquals(r.effects, List(RenderApprovalRequest(request)))

  test("ApprovalRequested after running=false: no effects"):
    val r =
      transition(idle.copy(running = false), ApprovalRequested(request), ctx)
    assertEquals(r.effects, Nil)

  test("nextInput: a pending request is asked before the next line"):
    val pending = idle.copy(pendingApprovals = List(request))
    assertEquals(
      nextInput(pending, menusSupported = true),
      Some(InputRequest.AskApproval(request))
    )
    assertEquals(
      nextInput(pending, menusSupported = false),
      Some(InputRequest.ReadLine)
    )
    assertEquals(
      nextInput(idle, menusSupported = true),
      Some(InputRequest.ReadLine)
    )
    assertEquals(nextInput(midTurn, menusSupported = true), None)

  test("ApprovalChosen: sends the decision and waits for the agent"):
    val pending = idle.copy(pendingApprovals = List(request))
    val r =
      transition(pending, ApprovalChosen(request, ApprovalDecision.Deny), ctx)
    assert(r.state.turnInFlight)
    assertEquals(r.state.pendingApprovals, Nil)
    assertEquals(r.state.turnCount, idle.turnCount)
    assertEquals(
      r.effects.head,
      SendOutbound(ApprovalReply(ctx.origin, Some(3), ApprovalDecision.Deny))
    )

  test(
    "ApprovalPostponed: drops it from the queue and says how to answer later"
  ):
    val pending = idle.copy(pendingApprovals = List(request))
    val r = transition(pending, ApprovalPostponed(request), ctx)
    assertEquals(r.state, idle)
    assert(
      r.effects.exists:
        case Render(Role.Permission, text) => text.contains("/approve 3")
        case _                             => false
    )

  test("UserInput /approve 3: answers request 3 and clears it from the queue"):
    val other = request.copy(id = 4)
    val pending = idle.copy(pendingApprovals = List(request, other))
    val r = transition(pending, UserInput("/approve 3"), ctx)
    assertEquals(r.state.pendingApprovals, List(other))

  /** UserInput */

  test("UserInput empty/whitespace: no-op"):
    val r = transition(idle, UserInput("   "), ctx)
    assertEquals(r.state, idle)
    assertEquals(r.effects, Nil)

  test("UserInput /quit: marks running=false, no further effects"):
    val r = transition(idle, UserInput("/quit"), ctx)
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
      List(Render(Role.Error, "Unknown command: /garbage."))
    )

  test("UserInput /sesions: Unknown command + 'Did you mean' suggestion"):
    val r = transition(idle, UserInput("/sesions"), ctx)
    assertEquals(r.state, idle)
    assertEquals(r.effects.size, 1)
    val text = r.effects.head match
      case Render(Role.Error, t) => t
      case other                 => fail(s"unexpected: $other")
    assert(text.startsWith("Unknown command: /sesions."))
    assert(text.contains("Did you mean: /sessions"))

  test("UserInput unknown /command while turn-in-flight: still blocked"):
    val r = transition(midTurn, UserInput("/foobar"), ctx)
    assertEquals(r.state, midTurn)
    assertEquals(r.effects.size, 1)
    assert:
      r.effects.head match
        case Render(Role.Error, t) => t.startsWith("Unknown command")
        case _                     => false

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

  /** AssistantTextDelta / AssistantTextComplete / ErrorText */

  test("AssistantTextDelta while running: emits RenderAssistantDelta"):
    val r = transition(midTurn, AssistantTextDelta("ans"), ctx)
    assertEquals(r.state, midTurn)
    assertEquals(r.effects, List(RenderAssistantDelta("ans")))

  test("AssistantTextComplete while running: emits RenderAssistantComplete"):
    val r = transition(midTurn, AssistantTextComplete("answer"), ctx)
    assertEquals(r.state, midTurn)
    assertEquals(r.effects, List(RenderAssistantComplete))

  test("ErrorText while running: flushes stream then emits Render(Error)"):
    val r = transition(midTurn, ErrorText("oops"), ctx)
    assertEquals(r.state, midTurn)
    assertEquals(
      r.effects,
      List(RenderAssistantComplete, Render(Role.Error, "oops"))
    )

  test("Assistant/Error text after running=false: no effects"):
    val stopped = idle.copy(running = false)
    assertEquals(transition(stopped, AssistantTextDelta("x"), ctx).effects, Nil)
    assertEquals(
      transition(stopped, AssistantTextComplete("x"), ctx).effects,
      Nil
    )
    assertEquals(transition(stopped, ErrorText("x"), ctx).effects, Nil)

  /** ToolCall */

  test(
    "ToolCall while running: flushes streamed text, then renders name(args)"
  ):
    val r = transition(midTurn, ToolCall("eval_scala", "{\"code\":\"1\"}"), ctx)
    assertEquals(r.state, midTurn)
    assertEquals(
      r.effects,
      List(
        RenderAssistantComplete,
        Render(Role.Tool, "eval_scala({\"code\":\"1\"})")
      )
    )

  test("ToolCall after running=false: no effects"):
    val stopped = idle.copy(running = false)
    assertEquals(transition(stopped, ToolCall("t", "{}"), ctx).effects, Nil)

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
      List(CancelSpinnerFiber, SetEcho(true), StopSpinner, ClearAssistantBuffer)
    )

  test(
    "TurnFinished without spinner: still restores echo and stops (idempotent)"
  ):
    val noSpinner = midTurn.copy(spinner = None)
    val r = transition(noSpinner, TurnFinished, ctx)
    assertEquals(r.state.turnInFlight, false)
    assertEquals(
      r.effects,
      List(SetEcho(true), StopSpinner, ClearAssistantBuffer)
    )

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

  /** HintTick */

  test("HintTick with /se buffer: RenderHintStatus contains /sessions"):
    val r = transition(idle, HintTick("/se"), ctx)
    assertEquals(r.state, idle)
    assertEquals(r.effects.size, 1)
    val text = r.effects.head match
      case RenderHintStatus(t) => t
      case other               => fail(s"unexpected: $other")
    assert(text.startsWith("↳ "))
    assert(text.contains("/sessions"))

  test("HintTick with empty buffer: RenderHintStatus(\"\") clears the line"):
    val r = transition(idle, HintTick(""), ctx)
    assertEquals(r.effects, List(RenderHintStatus("")))

  test("HintTick with non-slash text: RenderHintStatus(\"\")"):
    val r = transition(idle, HintTick("hello world"), ctx)
    assertEquals(r.effects, List(RenderHintStatus("")))

  test("HintTick during turn-in-flight: no effects (spinner owns status)"):
    val r = transition(midTurn, HintTick("/se"), ctx)
    assertEquals(r.state, midTurn)
    assertEquals(r.effects, Nil)
