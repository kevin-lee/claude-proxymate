package claudeproxymate.renderer.route

import cats.syntax.all.*
import claudeproxymate.core.{HtmlIds, RouteMode, SyncAction}
import claudeproxymate.renderer.facades.ElectronApi
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.AppState
import org.scalajs.dom

import scala.scalajs.js

/** The status-bar "Route Claude" segmented control (Manual / VS Code /
  * Global).
  *
  * Renderer side of the ANTHROPIC_BASE_URL auto-sync: sends the
  * requested [[RouteMode]] to the main process's RouteSync over IPC,
  * renders the active segment, and surfaces per-target failures via
  * `window.alert` (the app's existing error pattern, see ProxyControl).
  *
  * State lives in [[AppState.routeMode]] and is re-synced from
  * `proxyStatus()` on renderer boot (window reload / re-creation).
  */
object RouteControl {

  /** One per-target outcome from the main process. */
  final case class SyncResult(target: String, action: SyncAction, reason: String)

  // ── Pure helpers (unit-tested) ──

  /** Map failure outcomes to alert (i18n key, vars); ok-outcomes map to
    * nothing. All foreign-skips collapse into a single alert.
    */
  def alertKeys(results: List[SyncResult]): List[(String, Map[String, String])] = {
    val foreignTargets = results.collect {
      case SyncResult(target, SyncAction.SkippedForeign, _) => target
    }
    val foreign        = Option
      .when(foreignTargets.nonEmpty)(
        ("route.alertForeign", Map("targets" -> foreignTargets.mkString(", ")))
      )
      .toList
    val perTarget      = results.flatMap {
      case SyncResult(target, SyncAction.Failed, reason) =>
        ("route.alertFail", Map("target" -> target, "reason" -> reason)).some
      case SyncResult(target, SyncAction.Restored, _) =>
        ("route.alertRestored", Map("target" -> target)).some
      case SyncResult(target, SyncAction.RestoreFailed, path) =>
        ("route.alertRestoreFail", Map("target" -> target, "path" -> path)).some
      case SyncResult(target, SyncAction.Concurrent, _) =>
        ("route.alertConcurrent", Map("target" -> target)).some
      case SyncResult(
             _,
             SyncAction.Applied | SyncAction.Removed | SyncAction.Noop | SyncAction.AlreadyApplied |
             SyncAction.SkippedForeign | SyncAction.NotDetected,
             _,
           ) =>
        none[(String, Map[String, String])]
    }
    foreign ++ perTarget
  }

  // ── DOM wiring ──

  def install(): Unit = {
    dom.document.addEventListener("click", handleClick(_))
    ElectronApi.get.foreach { api =>
      api.onRouteModeEvent { (payload: js.Dynamic) =>
        showAlerts(extractResults(payload.selectDynamic("results")))
      }
    }
  }

  private def handleClick(e: dom.MouseEvent): Unit =
    Option(e.target.asInstanceOf[dom.Element])
      .flatMap(target => Option(target.closest(s"#${HtmlIds.RouteSeg} .seg-btn")))
      .flatMap(btn => Option(btn.asInstanceOf[dom.html.Element].getAttribute("data-route")))
      .flatMap(RouteMode.parse)
      .foreach(mode => if (mode =!= AppState.routeMode) setMode(mode) else ())

  def setMode(requested: RouteMode): Unit = {
    ElectronApi.get match {
      case None => ()
      case Some(api) => doSetMode(api, requested)
    }
  }

  private def doSetMode(api: ElectronApi, requested: RouteMode): Unit = {
    setSegmentsDisabled(true)

    val onSuccess: js.Function1[js.Dynamic, Unit] = { (result: js.Dynamic) =>
      val resolved    = RouteMode.parse(asString(result.selectDynamic("mode"))).getOrElse(RouteMode.default)
      AppState.routeMode = resolved
      val detectedRaw = result.selectDynamic("detected")
      val detected    = if (js.typeOf(detectedRaw) === "number") detectedRaw.asInstanceOf[Int] else 0
      if (requested === RouteMode.VsCode && resolved =!= RouteMode.VsCode && detected === 0) {
        dom.window.alert(I18n.t("route.alertNoEditors"))
      } else ()
      showAlerts(extractResults(result.selectDynamic("results")))
      setSegmentsDisabled(false)
      render()
    }
    val onError: js.Function1[scala.Any, Unit]    = { (err: scala.Any) =>
      dom.console.error("routeModeSet error:", err.asInstanceOf[js.Any])
      dom
        .window
        .alert(
          I18n.t("route.alertFail", Map("target" -> I18n.t(s"route.${requested.wire}"), "reason" -> err.toString))
        )
      setSegmentsDisabled(false)
      render()
    }
    locally { val _ = api.routeModeSet(requested.wire).`then`(onSuccess, onError) }
  }

  /** Sync the segmented control with [[AppState.routeMode]]: exactly one
    * segment active. Labels/tooltips are static `data-i18n` /
    * `data-i18n-title` attributes handled by `applyI18n`.
    */
  def render(): Unit =
    segButtons.foreach { el =>
      val active = el.getAttribute("data-route") === AppState.routeMode.wire
      if (active) {
        locally { val _ = el.classList.add("active") }
      } else {
        locally { val _ = el.classList.remove("active") }
      }
      el.setAttribute("aria-pressed", active.toString)
    }

  private def segButtons: List[dom.html.Element] = {
    val seg = dom.document.getElementById(HtmlIds.RouteSeg)
    if (seg == null) Nil
    else {
      seg
        .querySelectorAll(".seg-btn")
        .toList
        .map(node => node.asInstanceOf[dom.html.Element])
    }
  }

  private def setSegmentsDisabled(disabled: Boolean): Unit =
    segButtons.foreach(el => el.asInstanceOf[dom.html.Button].disabled = disabled)

  private def showAlerts(results: List[SyncResult]): Unit =
    alertKeys(results).foreach {
      case (key, vars) =>
        dom.window.alert(I18n.t(key, vars))
    }

  private def extractResults(value: js.Dynamic): List[SyncResult] =
    if (js.isUndefined(value) || value == null || !js.Array.isArray(value)) {
      Nil
    } else {
      value
        .asInstanceOf[js.Array[js.Dynamic]]
        .toList
        .flatMap { result =>
          SyncAction
            .parse(asString(result.selectDynamic("action")))
            .map { action =>
              SyncResult(
                asString(result.selectDynamic("target")),
                action,
                asString(result.selectDynamic("reason")),
              )
            }
            .toOption
        }
    }

  private def asString(value: js.Dynamic): String =
    if (js.typeOf(value) === "string") value.asInstanceOf[String] else ""
}
