package claudeproxymate.renderer.util

import cats.syntax.all.*

import scala.scalajs.js

/** Debounce timer utility.
  *
  * Ports the `clearTimeout`/`setTimeout` debounce pattern from renderer.js.
  */
final class Debounce(delayMs: Int) {

  private var timerId: Option[js.timers.SetTimeoutHandle] = none[js.timers.SetTimeoutHandle]

  def apply(fn: () => Unit): Unit = {
    timerId.foreach(js.timers.clearTimeout)
    timerId = js.timers.setTimeout(delayMs.toDouble)(fn()).some
  }

  def cancel(): Unit = {
    timerId.foreach(js.timers.clearTimeout)
    timerId = none[js.timers.SetTimeoutHandle]
  }
}
