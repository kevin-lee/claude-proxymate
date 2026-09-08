package claudeproxymate.renderer.route

import claudeproxymate.core.SyncAction
import hedgehog.*
import hedgehog.runner.*

object RouteControlSpec extends Properties {

  override def tests: List[Test] = List(
    example("alertKeys maps nothing for ok actions", testAlertKeysOkActions),
    example("alertKeys collapses foreign skips into one alert", testAlertKeysForeignCollapsed),
    example("alertKeys maps each failure action", testAlertKeysFailures),
    property("alertKeys never alerts for applied/removed results", testAlertKeysNeverForOk),
  )

  def testAlertKeysOkActions: Result = {
    val results = List(
      RouteControl.SyncResult("VS Code", SyncAction.Applied, ""),
      RouteControl.SyncResult("Cursor", SyncAction.Removed, ""),
      RouteControl.SyncResult("VSCodium", SyncAction.Noop, ""),
      RouteControl.SyncResult("~/.claude/settings.json", SyncAction.AlreadyApplied, ""),
      RouteControl.SyncResult("VS Code Insiders", SyncAction.NotDetected, ""),
    )
    RouteControl.alertKeys(results) ==== Nil
  }

  def testAlertKeysForeignCollapsed: Result = {
    val results = List(
      RouteControl.SyncResult("VS Code", SyncAction.SkippedForeign, "https://gw.example.com"),
      RouteControl.SyncResult("~/.claude/settings.json", SyncAction.SkippedForeign, "https://gw.example.com"),
    )
    RouteControl.alertKeys(results) ==== List(
      ("route.alertForeign", Map("targets" -> "VS Code, ~/.claude/settings.json"))
    )
  }

  def testAlertKeysFailures: Result = {
    val results = List(
      RouteControl.SyncResult("VS Code", SyncAction.Failed, "disk full"),
      RouteControl.SyncResult("Cursor", SyncAction.Restored, ""),
      RouteControl.SyncResult("VSCodium", SyncAction.RestoreFailed, "/backups/b.json"),
      RouteControl.SyncResult("~/.claude/settings.json", SyncAction.Concurrent, ""),
    )
    RouteControl.alertKeys(results) ==== List(
      ("route.alertFail", Map("target" -> "VS Code", "reason" -> "disk full")),
      ("route.alertRestored", Map("target" -> "Cursor")),
      ("route.alertRestoreFail", Map("target" -> "VSCodium", "path" -> "/backups/b.json")),
      ("route.alertConcurrent", Map("target" -> "~/.claude/settings.json")),
    )
  }

  def testAlertKeysNeverForOk: Property =
    for {
      target <- Gen.string(Gen.alphaNum, Range.linear(1, 12)).log("target")
      action <- Gen
                  .element1(
                    SyncAction.Applied,
                    SyncAction.Removed,
                    SyncAction.Noop,
                    SyncAction.AlreadyApplied,
                    SyncAction.NotDetected,
                  )
                  .log("action")
    } yield {
      RouteControl.alertKeys(List(RouteControl.SyncResult(target, action, ""))) ==== Nil
    }
}
