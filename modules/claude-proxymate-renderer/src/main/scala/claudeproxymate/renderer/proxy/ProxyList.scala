package claudeproxymate.renderer.proxy

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.AppState
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
    val entry  = target.closest(s".${ProxyListView.EntryClass}[${ProxyListView.EntryDataIdAttr}]")
    if (entry == null) return
    val raw = entry.asInstanceOf[dom.html.Element].getAttribute(ProxyListView.EntryDataIdAttr)
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
        id       = id,
        method   = readString(e, "method"),
        path     = readString(e, "path"),
        ts       = readString(e, "ts"),
        model    = readModel(e),
        status   = readStatus(e),
        selected = AppState.selectedProxyId.contains(id),
      )
    }

    val labels = ProxyListLabels(
      noCapturesTitle = I18n.t("proxy.noCapturesTitle"),
      noCapturesHint  = I18n.t("proxy.noCapturesHint"),
    )

    ViewHelpers.setInnerHtml(list, ProxyListView.buildListFrag(entries, labels))
  }

  private def readString(e: js.Dynamic, key: String): String = {
    val v = e.selectDynamic(key)
    if (js.isUndefined(v) || v == null) "" else v.toString
  }

  private def readModel(e: js.Dynamic): Option[String] = {
    val body = e.selectDynamic("body")
    if (js.isUndefined(body) || body == null) None
    else {
      val m = body.selectDynamic("model")
      if (js.isUndefined(m) || m == null) None else Some(m.toString)
    }
  }

  private def readStatus(e: js.Dynamic): Option[Int] = {
    val resp = e.selectDynamic("response")
    if (js.isUndefined(resp) || resp == null) None
    else {
      val st = resp.selectDynamic("status")
      if (js.isUndefined(st) || st == null) None
      else {
        try Some(st.asInstanceOf[Int])
        catch { case _: Throwable => None }
      }
    }
  }

  /** Select a capture and re-render the list + detail. Called from Scala
    * (`clearProxyCaptures`, the click handler).
    */
  def selectProxyEntry(id: Double): Unit = {
    AppState.selectedProxyId = Some(id)
    renderProxyList()
    ProxyControl.callRenderProxyDetail()
  }

  @JSExportTopLevel("clearProxyCaptures")
  def clearProxyCaptures(): Unit = {
    AppState.proxyCaptures = Nil
    AppState.selectedProxyId = None
    renderProxyList()
    ProxyControl.callRenderProxyDetail()
  }
}
