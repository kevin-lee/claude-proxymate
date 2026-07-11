package claudeproxymate.renderer.vscode

import cats.syntax.all.*
import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.facades.ElectronApi
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.AppState
import org.scalajs.dom

import scala.scalajs.js

/** The proxy-bar "VS Code" toggle.
  *
  * Renderer side of the VS Code ANTHROPIC_BASE_URL auto-sync: flips the
  * main process's VsCodeSync flag over IPC, renders the button state,
  * and surfaces per-editor failures via `window.alert` (the app's
  * existing error pattern, see ProxyControl).
  *
  * Follows PresenterMode's renderButton pattern; state lives in
  * [[AppState.vscodeSyncEnabled]] and is re-synced from `proxyStatus()`
  * on renderer boot (window reload / re-creation).
  */
object VsCodeSyncToggle {

  /** One per-editor outcome from the main process (wire format). */
  final case class SyncResult(editor: String, action: String, reason: String)

  // ── Pure helpers (unit-tested) ──

  def buttonKey(enabled: Boolean): String =
    if (enabled) "vscode.btnOn" else "vscode.btnOff"

  def titleKey(enabled: Boolean): String =
    if (enabled) "vscode.titleOn" else "vscode.titleOff"

  /** Map failure outcomes to alert (i18n key, vars); ok-outcomes map to
    * nothing. All foreign-skips collapse into a single alert.
    */
  def alertKeys(results: List[SyncResult]): List[(String, Map[String, String])] = {
    val foreignEditors = results.collect {
      case SyncResult(editor, "skippedForeign", _) => editor
    }
    val foreign        = Option
      .when(foreignEditors.nonEmpty)(
        ("vscode.alertForeign", Map("editors" -> foreignEditors.mkString(", ")))
      )
      .toList
    val perEditor      = results.flatMap {
      case SyncResult(editor, "failed", reason) =>
        ("vscode.alertFail", Map("editor" -> editor, "reason" -> reason)).some
      case SyncResult(editor, "restored", _) =>
        ("vscode.alertRestored", Map("editor" -> editor)).some
      case SyncResult(editor, "restoreFailed", path) =>
        ("vscode.alertRestoreFail", Map("editor" -> editor, "path" -> path)).some
      case SyncResult(editor, "concurrent", _) =>
        ("vscode.alertConcurrent", Map("editor" -> editor)).some
      case SyncResult(_, _, _) =>
        none[(String, Map[String, String])]
    }
    foreign ++ perEditor
  }

  // ── DOM wiring ──

  /** Editors named in the last non-empty result set — used for the
    * ON-state tooltip's {editors} substitution.
    */
  private var managedEditors: List[String] = Nil

  def install(): Unit = {
    dom.document.addEventListener("click", handleClick(_))
    ElectronApi.get.foreach { api =>
      api.onVscodeSyncEvent { (payload: js.Dynamic) =>
        showAlerts(extractResults(payload.selectDynamic("results")))
      }
    }
  }

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    if (target.closest(s"#${HtmlIds.VsCodeToggleBtn}") != null) toggle()
  }

  def toggle(): Unit = {
    ElectronApi.get match {
      case None => ()
      case Some(api) => doToggle(api)
    }
  }

  private def doToggle(api: ElectronApi): Unit = {
    val btn = dom.document.getElementById(HtmlIds.VsCodeToggleBtn)
    if (btn != null) btn.asInstanceOf[dom.html.Button].disabled = true

    val requestedOn = !AppState.vscodeSyncEnabled

    val onSuccess: js.Function1[js.Dynamic, Unit]  = { (result: js.Dynamic) =>
      AppState.vscodeSyncEnabled = result.enabled.asInstanceOf[Boolean]
      val detected = result.detected.asInstanceOf[Int]
      val results  = extractResults(result.results)
      val named    = results.map(r => r.editor).distinct
      if (named.nonEmpty) managedEditors = named else ()
      if (requestedOn && detected === 0) {
        dom.window.alert(I18n.t("vscode.alertNoEditors"))
      } else ()
      showAlerts(results)
      if (btn != null) btn.asInstanceOf[dom.html.Button].disabled = false
      renderButton()
    }
    val onError: js.Function1[scala.Any, Unit]     = { (err: scala.Any) =>
      dom.console.error("vscodeSyncSet error:", err.asInstanceOf[js.Any])
      dom.window.alert(I18n.t("vscode.alertFail", Map("editor" -> "VS Code", "reason" -> err.toString)))
      if (btn != null) btn.asInstanceOf[dom.html.Button].disabled = false
      renderButton()
    }
    locally { val _ = api.vscodeSyncSet(requestedOn).`then`(onSuccess, onError) }
  }

  /** Render label, tooltip, aria state, and the `on` CSS class from
    * [[AppState.vscodeSyncEnabled]].
    *
    * The `data-i18n` attribute is rewritten to the current state's key so
    * a later `applyI18n` (locale switch) cannot regress the label to OFF
    * while ON. The ON tooltip needs a `{editors}` substitution that
    * `applyI18n` cannot do, so `data-i18n-title` is removed in that state
    * and the title is set directly.
    */
  def renderButton(): Unit = {
    val btn = dom.document.getElementById(HtmlIds.VsCodeToggleBtn)
    if (btn == null) return
    val el      = btn.asInstanceOf[dom.html.Element]
    val enabled = AppState.vscodeSyncEnabled

    el.textContent = I18n.t(buttonKey(enabled))
    el.setAttribute("data-i18n", buttonKey(enabled))

    val title =
      if (enabled) {
        I18n.t(titleKey(enabled), Map("editors" -> editorsLabel))
      } else {
        I18n.t(titleKey(enabled))
      }
    el.setAttribute("title", title)
    el.setAttribute("aria-label", title)
    el.setAttribute("aria-pressed", enabled.toString)
    if (enabled) {
      el.removeAttribute("data-i18n-title")
      locally { val _ = el.classList.add("on") }
    } else {
      el.setAttribute("data-i18n-title", titleKey(enabled))
      locally { val _ = el.classList.remove("on") }
    }
  }

  private def editorsLabel: String =
    managedEditors match {
      case Nil => "VS Code"
      case editors @ (_ :: _) => editors.mkString(", ")
    }

  private def showAlerts(results: List[SyncResult]): Unit =
    alertKeys(results).foreach { case (key, vars) =>
      dom.window.alert(I18n.t(key, vars))
    }

  private def extractResults(value: js.Dynamic): List[SyncResult] =
    if (js.isUndefined(value) || value == null || !js.Array.isArray(value)) {
      Nil
    } else {
      value
        .asInstanceOf[js.Array[js.Dynamic]]
        .toList
        .map { result =>
          SyncResult(
            asString(result.selectDynamic("editor")),
            asString(result.selectDynamic("action")),
            asString(result.selectDynamic("reason")),
          )
        }
    }

  private def asString(value: js.Dynamic): String =
    if (js.typeOf(value) === "string") value.asInstanceOf[String] else ""
}
