package claudeproxymate.core

import cats.*
import cats.derived.strict.*
import cats.syntax.all.*

/** The Electron main process's view of the native proxy child process.
  *
  *   - [[Idle]]: no child - never spawned, or the last one's `exit` event
  *     has been seen.
  *   - [[Running]]: spawned, no stop requested.
  *   - [[Stopping]]: a termination signal has been sent, the child has not
  *     exited yet.
  *
  * [[Stopping]] exists because Node's `child.killed` reports only that a
  * signal was *sent*, never that the process exited: a child can be
  * `killed == true`, `exitCode == null`, and still hold its listening socket.
  * Collapsing straight from [[Running]] to [[Idle]] the moment the signal
  * went out is what let a surviving child become unreachable and keep the
  * port, so the next start failed with `Address already in use`. Only the
  * child's own `exit` event may move the state back to [[Idle]].
  */
enum ProxyLifecycle derives CanEqual, Eq, Hash, Show {
  case Idle
  case Running
  case Stopping
}

object ProxyLifecycle {

  /** A spawn is allowed only from [[Idle]]. Spawning on top of a [[Stopping]]
    * child races the dying process for the port, which is precisely the
    * failure this state machine exists to prevent.
    */
  def onSpawn(current: ProxyLifecycle): Option[ProxyLifecycle] =
    current match {
      case ProxyLifecycle.Idle => ProxyLifecycle.Running.some
      case ProxyLifecycle.Running | ProxyLifecycle.Stopping => none[ProxyLifecycle]
    }

  /** A stop request is meaningful only from [[Running]]. Returning `None`
    * while already [[Stopping]] keeps a repeated stop from re-arming the
    * escalation timers.
    */
  def onStopRequested(current: ProxyLifecycle): Option[ProxyLifecycle] =
    current match {
      case ProxyLifecycle.Running => ProxyLifecycle.Stopping.some
      case ProxyLifecycle.Idle | ProxyLifecycle.Stopping => none[ProxyLifecycle]
    }

  /** What `proxy-status` reports. [[Stopping]] reports not-running: the user
    * asked for a stop and the UI must not claim otherwise while the child is
    * winding down.
    */
  def reportsRunning(current: ProxyLifecycle): Boolean =
    current match {
      case ProxyLifecycle.Running => true
      case ProxyLifecycle.Idle | ProxyLifecycle.Stopping => false
    }
}
