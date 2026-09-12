package claudeproxymate.electron

/** What [[IpcHandlers.stopProxy]] did, so the caller knows whether to wait.
  *
  *   - [[Deferred]]: a child was still alive; the `whenGone` callback will
  *     fire later, once the child's `exit` event arrives or the escalation
  *     deadline forces the issue.
  *   - [[NothingToStop]]: there was no live child, and `whenGone` has already
  *     run synchronously.
  */
enum StopOutcome {
  case Deferred
  case NothingToStop
}
