package claudeproxymate.renderer.util

import scala.scalajs.js

/** Debounce timer utility.
  *
  * Ports the `clearTimeout`/`setTimeout` debounce pattern from renderer.js.
  */
final class Debounce(delayMs: Int) {

  private var timerId: Option[js.timers.SetTimeoutHandle] = None

  def apply(fn: () => Unit): Unit = {
    timerId.foreach(js.timers.clearTimeout)
    timerId = Some(js.timers.setTimeout(delayMs.toDouble)(fn()))
  }

  def cancel(): Unit = {
    timerId.foreach(js.timers.clearTimeout)
    timerId = None
  }
}
