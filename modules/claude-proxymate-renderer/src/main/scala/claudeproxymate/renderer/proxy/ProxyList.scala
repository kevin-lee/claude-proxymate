package claudeproxymate.renderer.proxy

import cats.syntax.all.*
import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.{AppState, PresenterMode}
import claudeproxymate.renderer.view.ViewHelpers
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Proxy capture list rendering and selection.
  *
  * Ports `renderProxyList`, `selectProxyEntry`, `clearProxyCaptures`
  * from renderer.js. Click handling is delegated through a single
  * document-level listener installed by [[install]].
  */
object ProxyList {

  /** Install the document-level click handler that dispatches
    * `.prx-entry[data-id]` clicks. Called from RendererMain.
    */
  def install(): Unit = {
    dom.document.addEventListener("click", handleClick _)
  }

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return

    if (target.closest(s"#${HtmlIds.ProxyClearBtn}") != null) {
      val _ = confirmAndClearProxyCaptures()
      return
    }

    val entry = target.closest(s".${ProxyListView.EntryClass}[${ProxyListView.EntryDataIdAttr}]")
    if (entry == null) return
    val raw   = entry.asInstanceOf[dom.html.Element].getAttribute(ProxyListView.EntryDataIdAttr)
    if (raw == null) return
    try {
      val id = raw.toDouble
      selectProxyEntry(id)
    } catch {
      case _: NumberFormatException => ()
    }
  }

  @JSExportTopLevel("renderProxyList")
  def renderProxyList(): Unit = {
    val list    = dom.document.getElementById(HtmlIds.ProxyList)
    val countEl = dom.document.getElementById(HtmlIds.ProxyCount)
    if (list == null) return
    if (countEl != null) countEl.textContent = AppState.proxyCaptures.length.toString

    val entries = AppState.proxyCaptures.map { e =>
      val id = e.id.asInstanceOf[Double]
      ProxyListEntry(
        id = id,
        method = readString(e, "method"),
        path = readString(e, "path"),
        ts = readString(e, "ts"),
        model = readModel(e),
        status = readStatus(e),
        selected = AppState.selectedProxyId.contains(id),
      )
    }

    val labels = ProxyListLabels(
      noCapturesTitle = I18n.t("proxy.noCapturesTitle"),
      noCapturesHint = I18n.t("proxy.noCapturesHint"),
    )

    ViewHelpers.setInnerHtml(list, ProxyListView.buildListFrag(entries, labels))
  }

  private def readString(e: js.Dynamic, key: String): String = {
    val v = e.selectDynamic(key)
    if (js.isUndefined(v) || v == null) "" else v.toString
  }

  private def readModel(e: js.Dynamic): Option[String] = {
    val body = e.selectDynamic("body")
    if (js.isUndefined(body) || body == null) none[String]
    else {
      val m = body.selectDynamic("model")
      Option.unless(js.isUndefined(m) || m == null)(m.toString)
    }
  }

  private def readStatus(e: js.Dynamic): Option[Int] = {
    val resp = e.selectDynamic("response")
    if (js.isUndefined(resp) || resp == null) none[Int]
    else {
      val st = resp.selectDynamic("status")
      if (js.isUndefined(st) || st == null) none[Int]
      else {
        try st.asInstanceOf[Int].some
        catch { case _: Throwable => none[Int] }
      }
    }
  }

  /** Select a capture and re-render the list + detail. Called from Scala
    * (`clearProxyCaptures`, the click handler).
    */
  def selectProxyEntry(id: Double): Unit = {
    // Per-span overrides are per-capture (mask ids aren't stable
    // across captures); drop them on capture switch. The presenter
    // baseline (`presenterMaskAll`) is intentionally preserved
    // across captures — a presenter who flipped to "Reveal All"
    // doesn't want it reset every time they click a different
    // request.
    AppState.maskOverrides.clear()
    AppState.selectedProxyId = id.some
    renderProxyList()
    ProxyControl.callRenderProxyDetail()
  }

  def clearProxyCaptures(): Unit = {
    AppState.proxyCaptures = Nil
    AppState.selectedProxyId = none[Double]
    AppState.maskOverrides.clear()
    // Reset the presenter baseline to the safe default on Clear.
    // The invariant: post-Clear, the next capture loads with
    // everything masked — useful for presenters who hit Clear
    // immediately before screen-sharing a fresh session.
    AppState.presenterMaskAll = true
    PresenterMode.renderButton()
    PresenterMode.renderChip()
    renderProxyList()
    ProxyControl.callRenderProxyDetail()
  }

  /** Clear captures, prompting the user first if the list is non-empty.
    *
    * Used by both the Clear button and the proxy-stop path to reduce
    * the lifetime of captured wire data in renderer memory. Returns
    * `true` if captures were cleared (or were already empty), `false`
    * if the user cancelled the prompt.
    */
  def confirmAndClearProxyCaptures(): Boolean =
    if (AppState.proxyCaptures.isEmpty) {
      clearProxyCaptures()
      true
    } else if (dom.window.confirm(I18n.t("proxy.discardConfirm"))) {
      clearProxyCaptures()
      true
    } else {
      false
    }
}
