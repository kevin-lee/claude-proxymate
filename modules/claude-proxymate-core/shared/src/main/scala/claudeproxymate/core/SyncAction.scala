package claudeproxymate.core

import cats.*
import cats.syntax.all.*
import cats.derived.strict.*

/** What one settings-file sync attempt did to one target.
  *
  * The main process produces it (`SyncFileOps`) and the renderer's Route
  * Claude control consumes it, so it lives in core rather than in either
  * side: `wire` / `parse` define the exact strings used on the IPC
  * channel, and neither side matches on string literals.
  */
enum SyncAction derives CanEqual, Eq, Show, Hash {

  case Applied
  case Removed
  case Noop
  case AlreadyApplied
  case SkippedForeign
  case Failed
  case Restored
  case RestoreFailed
  case Concurrent
  case NotDetected

}
object SyncAction {

  extension (syncAction: SyncAction) {

    def wire: String = syncAction match {
      case SyncAction.Applied => "applied"
      case SyncAction.Removed => "removed"
      case SyncAction.Noop => "noop"
      case SyncAction.AlreadyApplied => "alreadyApplied"
      case SyncAction.SkippedForeign => "skippedForeign"
      case SyncAction.Failed => "failed"
      case SyncAction.Restored => "restored"
      case SyncAction.RestoreFailed => "restoreFailed"
      case SyncAction.Concurrent => "concurrent"
      case SyncAction.NotDetected => "notDetected"
    }

    /** Outcomes worth telling the user about; the rest stay silent. */
    def isFailure: Boolean = syncAction match {
      case SyncAction.SkippedForeign | SyncAction.Failed | SyncAction.Restored | SyncAction.RestoreFailed |
           SyncAction.Concurrent =>
        true

      case SyncAction.Applied | SyncAction.Removed | SyncAction.Noop | SyncAction.AlreadyApplied |
           SyncAction.NotDetected =>
        false
    }

  }

  def parse(s: String): Either[String, SyncAction] = s match {
    case "applied" => SyncAction.Applied.asRight
    case "removed" => SyncAction.Removed.asRight
    case "noop" => SyncAction.Noop.asRight
    case "alreadyApplied" => SyncAction.AlreadyApplied.asRight
    case "skippedForeign" => SyncAction.SkippedForeign.asRight
    case "failed" => SyncAction.Failed.asRight
    case "restored" => SyncAction.Restored.asRight
    case "restoreFailed" => SyncAction.RestoreFailed.asRight
    case "concurrent" => SyncAction.Concurrent.asRight
    case "notDetected" => SyncAction.NotDetected.asRight
    case unknown => s"Unknown SyncAction: $unknown".asLeft[SyncAction]
  }
}
