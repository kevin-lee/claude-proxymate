package claudeproxymate.renderer.vscode

import hedgehog.*
import hedgehog.runner.*

object VsCodeSyncToggleSpec extends Properties {

  override def tests: List[Test] = List(
    example("buttonKey selects on/off keys", testButtonKey),
    example("titleKey selects on/off keys", testTitleKey),
    example("alertKeys maps nothing for ok actions", testAlertKeysOkActions),
    example("alertKeys collapses foreign skips into one alert", testAlertKeysForeignCollapsed),
    example("alertKeys maps each failure action", testAlertKeysFailures),
    property("alertKeys never alerts for applied/removed results", testAlertKeysNeverForOk),
  )

  def testButtonKey: Result =
    Result.all(
      List(
        VsCodeSyncToggle.buttonKey(true) ==== "vscode.btnOn",
        VsCodeSyncToggle.buttonKey(false) ==== "vscode.btnOff",
      )
    )

  def testTitleKey: Result =
    Result.all(
      List(
        VsCodeSyncToggle.titleKey(true) ==== "vscode.titleOn",
        VsCodeSyncToggle.titleKey(false) ==== "vscode.titleOff",
      )
    )

  def testAlertKeysOkActions: Result = {
    val results = List(
      VsCodeSyncToggle.SyncResult("VS Code", "applied", ""),
      VsCodeSyncToggle.SyncResult("Cursor", "removed", ""),
      VsCodeSyncToggle.SyncResult("VSCodium", "noop", ""),
      VsCodeSyncToggle.SyncResult("VS Code", "alreadyApplied", ""),
      VsCodeSyncToggle.SyncResult("VS Code Insiders", "notDetected", ""),
    )
    VsCodeSyncToggle.alertKeys(results) ==== Nil
  }

  def testAlertKeysForeignCollapsed: Result = {
    val results = List(
      VsCodeSyncToggle.SyncResult("VS Code", "skippedForeign", "https://gw.example.com"),
      VsCodeSyncToggle.SyncResult("Cursor", "skippedForeign", "https://gw.example.com"),
    )
    VsCodeSyncToggle.alertKeys(results) ==== List(
      ("vscode.alertForeign", Map("editors" -> "VS Code, Cursor"))
    )
  }

  def testAlertKeysFailures: Result = {
    val results = List(
      VsCodeSyncToggle.SyncResult("VS Code", "failed", "disk full"),
      VsCodeSyncToggle.SyncResult("Cursor", "restored", ""),
      VsCodeSyncToggle.SyncResult("VSCodium", "restoreFailed", "/backups/b.json"),
      VsCodeSyncToggle.SyncResult("VS Code", "concurrent", ""),
    )
    VsCodeSyncToggle.alertKeys(results) ==== List(
      ("vscode.alertFail", Map("editor" -> "VS Code", "reason" -> "disk full")),
      ("vscode.alertRestored", Map("editor" -> "Cursor")),
      ("vscode.alertRestoreFail", Map("editor" -> "VSCodium", "path" -> "/backups/b.json")),
      ("vscode.alertConcurrent", Map("editor" -> "VS Code")),
    )
  }

  def testAlertKeysNeverForOk: Property =
    for {
      editor <- Gen.string(Gen.alphaNum, Range.linear(1, 12)).log("editor")
      action <- Gen.element1("applied", "removed", "noop", "alreadyApplied", "notDetected").log("action")
    } yield {
      VsCodeSyncToggle.alertKeys(List(VsCodeSyncToggle.SyncResult(editor, action, ""))) ==== Nil
    }
}
