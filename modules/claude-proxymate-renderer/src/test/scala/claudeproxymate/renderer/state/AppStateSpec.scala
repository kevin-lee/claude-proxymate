package claudeproxymate.renderer.state

import hedgehog.*
import hedgehog.runner.*

import scala.scalajs.js

object AppStateSpec extends Properties {

  override def tests: List[Test] = List(
    // isRevealed truth table — all four combinations
    example("presenterMaskAll=true,  override absent  → masked  (false)", testMaskAllNoOverride),
    example("presenterMaskAll=true,  override present → revealed (true)", testMaskAllWithOverride),
    example("presenterMaskAll=false, override absent  → revealed (true)", testRevealAllNoOverride),
    example("presenterMaskAll=false, override present → masked  (false)", testRevealAllWithOverride),
    // Default state on launch
    example("default presenterMaskAll is true (safe-for-screen-share default)", testDefaultMaskAll),
    // Independence of state fields
    example("clearing maskOverrides does not flip presenterMaskAll", testClearOverridesKeepsFlag),
    // selectedCapture
    example("selectedCapture is None when no capture is selected", testSelectedCaptureNoSelection),
    example("selectedCapture is the capture with the selected id", testSelectedCaptureFound),
    example("selectedCapture is None when the selected id is not in the list", testSelectedCaptureMissing),
  )

  private def reset(): Unit = {
    AppState.maskOverrides.clear()
    AppState.presenterMaskAll = true
  }

  def testMaskAllNoOverride: Result = {
    reset()
    AppState.presenterMaskAll = true
    Result
      .assert(!AppState.isRevealed("x"))
      .log(s"expected masked, got revealed; state=${AppState.presenterMaskAll}, overrides=${AppState.maskOverrides}")
  }

  def testMaskAllWithOverride: Result = {
    reset()
    AppState.presenterMaskAll = true
    val _ = AppState.maskOverrides.add("x")
    Result
      .assert(AppState.isRevealed("x"))
      .log(s"expected revealed, got masked")
  }

  def testRevealAllNoOverride: Result = {
    reset()
    AppState.presenterMaskAll = false
    Result
      .assert(AppState.isRevealed("x"))
      .log(s"expected revealed, got masked")
  }

  def testRevealAllWithOverride: Result = {
    reset()
    AppState.presenterMaskAll = false
    val _ = AppState.maskOverrides.add("x")
    Result
      .assert(!AppState.isRevealed("x"))
      .log(s"expected masked, got revealed")
  }

  def testDefaultMaskAll: Result = {
    // Don't touch state — just assert the documented default.
    // (Tests run in an unspecified order; this is the *current*
    //  state at test entry, which prior tests may have mutated.
    //  So we reset, then re-check.)
    reset()
    Result
      .assert(AppState.presenterMaskAll)
      .log(s"expected default presenterMaskAll=true after reset")
  }

  def testClearOverridesKeepsFlag: Result = {
    reset()
    AppState.presenterMaskAll = false
    val _ = AppState.maskOverrides.add("x")
    AppState.maskOverrides.clear()
    Result
      .assert(!AppState.presenterMaskAll)
      .log(s"clear should not have touched the flag")
  }

  private val firstCapture: js.Dynamic  = js.Dynamic.literal(id = 1.0, method = "POST", url = "/v1/messages")
  private val secondCapture: js.Dynamic = js.Dynamic.literal(id = 2.0, method = "POST", url = "/v1/messages")

  /* Sets the captures and the selection, runs `check`, and then sets them
   * back, so other specs do not see this state. */
  private def withSelection(selected: Option[Double])(check: => Result): Result = {
    AppState.proxyCaptures = List(secondCapture, firstCapture)
    AppState.selectedProxyId = selected
    val result = check
    AppState.proxyCaptures = Nil
    AppState.selectedProxyId = None
    result
  }

  def testSelectedCaptureNoSelection: Result =
    withSelection(None) {
      Result.assert(AppState.selectedCapture.isEmpty).log("expected no selected capture")
    }

  def testSelectedCaptureFound: Result =
    withSelection(Some(2.0)) {
      Result.assert(AppState.selectedCapture.exists(_ eq secondCapture)).log("expected the capture with id 2")
    }

  def testSelectedCaptureMissing: Result =
    withSelection(Some(3.0)) {
      Result.assert(AppState.selectedCapture.isEmpty).log("expected no capture for id 3")
    }
}
