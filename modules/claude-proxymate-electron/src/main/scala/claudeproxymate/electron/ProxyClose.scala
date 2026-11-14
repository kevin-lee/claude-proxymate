package claudeproxymate.electron

import cats.syntax.all.*

import scala.scalajs.js

/** Pure functions for the `"close"` listener of the native proxy child process
  * in [[IpcHandlers]].
  *
  * Issue #95: when `dyld` cannot load a library of the proxy binary, the proxy
  * stops after `spawn` has succeeded, and only the `"close"` event (with its
  * exit code or signal) tells the app that it failed. This object does not
  * refer to [[IpcHandlers]] or to a facade with `@JSImport`, so tests can link
  * it without Electron.
  */
private[electron] object ProxyClose {

  /** The exit code that Node gives to `"close"`: a number, or `null` when the
    * process was stopped by a signal.
    */
  def exitCode(arg: js.Any): Option[Int] =
    if (js.typeOf(arg) === "number") arg.asInstanceOf[Double].toInt.some else none[Int]

  /** The signal name that Node gives to `"close"`: a string, or `null` when the
    * process exited by itself.
    */
  def exitSignal(arg: js.Any): Option[String] =
    if (js.typeOf(arg) === "string") arg.asInstanceOf[String].some else none[String]

  /** `true` only when `child` is the process in the current state.
    *
    * This ignores the `"close"` that Node emits after `"error"` (the state has
    * no process then) and a late `"close"` of an earlier child.
    */
  def isCurrentChild[A <: AnyRef](current: Option[A], child: A): Boolean =
    current.exists(_ eq child)
}
