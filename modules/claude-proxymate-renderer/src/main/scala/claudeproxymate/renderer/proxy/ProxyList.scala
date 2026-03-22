package claudeproxymate.renderer.proxy

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.HtmlUtil.esc
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Proxy capture list rendering and selection.
  *
  * Ports `renderProxyList`, `selectProxyEntry`, `clearProxyCaptures`
  * from renderer.js.
  */
object ProxyList {

  @JSExportTopLevel("renderProxyList")
  def renderProxyList(): Unit = {
    val list    = dom.document.getElementById(HtmlIds.ProxyList)
    val countEl = dom.document.getElementById(HtmlIds.ProxyCount)
    if (list == null) return
    if (countEl != null) countEl.textContent = AppState.proxyCaptures.length.toString

    if (AppState.proxyCaptures.isEmpty) {
      list.innerHTML = s"""<div class="hist-empty">${I18n.t("proxy.noCaptures")}</div>"""
      return
    }

    val sb = new StringBuilder
    for (e <- AppState.proxyCaptures) {
      val model = {
        val body = e.selectDynamic("body")
        if (!js.isUndefined(body) && body != null) {
          val m = body.selectDynamic("model")
          if (!js.isUndefined(m) && m != null) s"""<div class="prx-model">${esc(m.toString)}</div>"""
          else ""
        } else ""
      }

      val sel = AppState.selectedProxyId match {
        case Some(selId) if e.id == selId.asInstanceOf[js.Any] => " selected"
        case _                                                  => ""
      }

      val statusBadge = {
        val resp = e.selectDynamic("response")
        if (!js.isUndefined(resp) && resp != null) {
          val status = resp.selectDynamic("status")
          val statusInt = if (!js.isUndefined(status) && status != null) {
            try status.asInstanceOf[Int] catch { case _: Throwable => 0 }
          } else 0
          val color = if (statusInt > 0 && statusInt < 400) "var(--green)" else "var(--red)"
          val text  = if (statusInt > 0) statusInt.toString else "ERR"
          s"""<span style="font-size:10px;font-family:'SF Mono',monospace;flex-shrink:0;color:$color">$text</span>"""
        } else {
          """<span style="font-size:10px;color:var(--dim);flex-shrink:0">…</span>"""
        }
      }

      val method = e.selectDynamic("method")
      val path   = e.selectDynamic("path")
      val ts     = e.selectDynamic("ts")
      val id     = e.selectDynamic("id")

      sb.append(s"""<div class="prx-entry$sel" onclick="selectProxyEntry($id)">""")
      sb.append("""<div style="display:flex;align-items:center;overflow:hidden">""")
      sb.append(s"""<span class="prx-method">${esc(method.toString)}</span>""")
      sb.append(s"""<span class="prx-path">${esc(path.toString)}</span>""")
      sb.append(statusBadge)
      sb.append(s"""<span class="prx-ts">$ts</span>""")
      sb.append("</div>")
      sb.append(model)
      sb.append("</div>")
    }

    list.innerHTML = sb.toString()
  }

  @JSExportTopLevel("selectProxyEntry")
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
