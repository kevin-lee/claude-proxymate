package claudeproxymate.renderer.state

import hedgehog.*
import hedgehog.runner.*

/** Tests for the *pure state semantics* of presenter mode.
  *
  * [[PresenterMode.toggle]] also re-renders the active capture's
  * detail view and updates the toolbar button + chip; those side
  * effects touch `dom.document` and can't be exercised in a Node
  * test env without a JS DOM shim. The state-mutation surface is
  * exercised here to lock the click semantics (per the design doc):
  *
  *   1. Each toggle flips `presenterMaskAll`.
  *   2. Each toggle clears `maskOverrides` so the new baseline
  *      takes hold without dissent from prior per-span clicks.
  *
  * The truth-table for [[AppState.isRevealed]] lives in
  * [[AppStateSpec]]; these tests focus on the transitions.
  */
object PresenterModeSpec extends Properties {

  override def tests: List[Test] = List(
    example("toggle from mask-all to reveal-all flips the flag", testToggleFlipsFlag),
    example("toggle clears existing overrides (reveal-all → mask-all)", testToggleClearsOverridesA),
    example("toggle clears existing overrides (mask-all → reveal-all)", testToggleClearsOverridesB),
    example("double-toggle returns the flag to its starting value", testDoubleToggleReturns),
    example("double-toggle clears overrides each pass", testDoubleToggleClearsOverrides),
    example("user example 1: revealAll → mask one span → maskAll → all masked", testUserExample1),
    example("user example 2: revealAll → mask one span → revealAll → all revealed", testUserExample2),
  )

  private def reset(): Unit = {
    AppState.maskOverrides.clear()
    AppState.presenterMaskAll = true
  }

  /** Mirror of [[PresenterMode.toggle]]'s pure state mutation, with
    * the DOM side effects stripped. Tests call this so they don't
    * need a DOM shim in the Node test env.
    */
  private def togglePure(): Unit = {
    AppState.presenterMaskAll = !AppState.presenterMaskAll
    AppState.maskOverrides.clear()
  }

  def testToggleFlipsFlag: Result = {
    reset()
    AppState.presenterMaskAll = true
    togglePure()
    Result
      .assert(!AppState.presenterMaskAll)
      .log(s"expected false after toggle from true, got ${AppState.presenterMaskAll}")
  }

  def testToggleClearsOverridesA: Result = {
    reset()
    AppState.presenterMaskAll = false
    locally { val _ = AppState.maskOverrides.add("a") }
    locally { val _ = AppState.maskOverrides.add("b") }
    togglePure()
    Result.all(
      List(
        Result.assert(AppState.presenterMaskAll).log("flag should flip to true"),
        Result.assert(AppState.maskOverrides.isEmpty).log(s"overrides should be empty, got ${AppState.maskOverrides}"),
      )
    )
  }

  def testToggleClearsOverridesB: Result = {
    reset()
    AppState.presenterMaskAll = true
    val _ = AppState.maskOverrides.add("a")
    togglePure()
    Result.all(
      List(
        Result.assert(!AppState.presenterMaskAll).log("flag should flip to false"),
        Result.assert(AppState.maskOverrides.isEmpty).log("overrides should be empty"),
      )
    )
  }

  def testDoubleToggleReturns: Result = {
    reset()
    val start = AppState.presenterMaskAll
    togglePure()
    togglePure()
    Result
      .assert(AppState.presenterMaskAll == start)
      .log(s"double-toggle should return to start; was $start, ended ${AppState.presenterMaskAll}")
  }

  def testDoubleToggleClearsOverrides: Result = {
    reset()
    locally { val _ = AppState.maskOverrides.add("a") }
    togglePure()
    locally { val _ = AppState.maskOverrides.add("b") }
    togglePure()
    Result
      .assert(AppState.maskOverrides.isEmpty)
      .log(s"after double-toggle, overrides should be empty: ${AppState.maskOverrides}")
  }

  // ── User-provided behavioural examples (PR 5 design doc) ─────────────

  def testUserExample1: Result = {
    // From the design doc: all-revealed → click one span (X) to mask
    // → click Mask All → all masked including X.
    //
    // Modelled state transitions:
    //   start:        (false, {})
    //   click X:      (false, {X})  — X masked, others revealed
    //   click button: (true, {})    — all masked (decisive flip + clear)
    reset()
    AppState.presenterMaskAll = false
    val _    = AppState.maskOverrides.add("X")
    val midX = AppState.isRevealed("X") // expect false (masked)
    val midY = AppState.isRevealed("Y") // expect true  (revealed)
    togglePure()
    Result.all(
      List(
        Result.assert(!midX).log("step 1: X should be masked after per-span click against reveal-all"),
        Result.assert(midY).log("step 1: Y should remain revealed"),
        Result.assert(!AppState.isRevealed("X")).log("step 2: X must be masked after Mask All"),
        Result.assert(!AppState.isRevealed("Y")).log("step 2: Y must be masked after Mask All"),
        Result.assert(AppState.maskOverrides.isEmpty).log("step 2: overrides cleared after Mask All"),
      )
    )
  }

  def testUserExample2: Result = {
    // From the design doc: all-revealed → click one span (X) to mask
    // → click Reveal All → all revealed including X.
    //
    // Because the global toggle is a single button whose label
    // flips with state, "Reveal All" from a `(false, {X})` state
    // maps to two clicks: one to `(true, {})` (button now reads
    // "Reveal All"), one more to `(false, {})` = all revealed.
    // The design doc accepts this as the price of the decisive
    // baseline. Verify the final state regardless of click count.
    reset()
    AppState.presenterMaskAll = false
    val _ = AppState.maskOverrides.add("X")
    togglePure()
    togglePure()
    Result.all(
      List(
        Result.assert(!AppState.presenterMaskAll).log("final: back to reveal-all"),
        Result.assert(AppState.isRevealed("X")).log("X should be revealed (no override, flag=false)"),
        Result.assert(AppState.isRevealed("Y")).log("Y should be revealed"),
        Result.assert(AppState.maskOverrides.isEmpty).log("overrides empty"),
      )
    )
  }
}
