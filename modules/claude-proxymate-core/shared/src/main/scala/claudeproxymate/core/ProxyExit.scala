package claudeproxymate.core

import cats.*
import cats.derived.strict.*
import cats.syntax.all.*

/** What the end of the native proxy child process means for the user.
  *
  *   - [[Stopped]]: the user asked for the stop, or the proxy exited cleanly
  *     (exit code 0, no signal).
  *   - [[StartFailed]]: an exit that nobody asked for, before the proxy
  *     reported `ProxyStarted`.
  *   - [[Crashed]]: an exit that nobody asked for, after the proxy reported
  *     `ProxyStarted`.
  *
  * The `detail` is from [[ProxyExit.describe]].
  */
enum ProxyExitOutcome derives CanEqual, Eq, Show {
  case Stopped
  case StartFailed(detail: String)
  case Crashed(detail: String)
}

/** Pure decisions about the exit of the native proxy child process.
  *
  * Issue #95: when `dyld` cannot load a library of the proxy binary, it stops
  * the process after `spawn` has succeeded, so Node's `error` event does not
  * come. Only the exit tells the app that the proxy failed, and only the
  * proxy's `stderr` tells why. These functions keep the last part of `stderr`
  * and decide what the exit means.
  */
object ProxyExit {

  /** The number of the last `stderr` characters that are kept. */
  val MaxStderrChars: Int = 4096

  /** The number of the last non-blank `stderr` lines in a description. */
  val MaxStderrLines: Int = 20

  /** Add a `stderr` chunk, and keep only the last [[MaxStderrChars]]
    * characters.
    */
  def appendStderr(current: String, chunk: String): String =
    (current + chunk).takeRight(MaxStderrChars)

  /** The reason for the exit (the signal, else the exit code), then the last
    * [[MaxStderrLines]] non-blank lines of `stderrTail`, one for each line.
    */
  def describe(code: Option[Int], signal: Option[String], stderrTail: String): String = {
    val reason =
      signal
        .map(name => s"signal $name")
        .orElse(code.map(n => s"exit code $n"))
        .getOrElse("unknown exit")
    val lines  =
      stderrTail
        .split("\n", -1)
        .toList
        .map(_.replaceAll("\\s+$", ""))
        .filter(_.nonEmpty)
        .takeRight(MaxStderrLines)
    lines match {
      case Nil => reason
      case _ => (reason :: lines).mkString("\n")
    }
  }

  /** Decide what the exit of the child process means.
    *
    * Only [[ProxyLifecycle.Running]] can give an unexpected exit:
    * [[ProxyLifecycle.Stopping]] means that the user asked for the stop, and
    * [[ProxyLifecycle.Idle]] means that there is no child to report on.
    */
  def classify(
    lifecycle: ProxyLifecycle,
    started: Boolean,
    code: Option[Int],
    signal: Option[String],
    stderrTail: String,
  ): ProxyExitOutcome =
    lifecycle match {
      case ProxyLifecycle.Stopping | ProxyLifecycle.Idle =>
        ProxyExitOutcome.Stopped
      case ProxyLifecycle.Running =>
        if (code === 0.some && signal.isEmpty) ProxyExitOutcome.Stopped
        else {
          val detail = describe(code, signal, stderrTail)
          if (started) ProxyExitOutcome.Crashed(detail) else ProxyExitOutcome.StartFailed(detail)
        }
    }
}
