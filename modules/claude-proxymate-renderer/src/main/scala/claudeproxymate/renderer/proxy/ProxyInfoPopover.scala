package claudeproxymate.renderer.proxy

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.i18n.I18n
import org.scalajs.dom

import scala.scalajs.js.annotation.JSExportTopLevel

/** Click-toggle info popover for the proxy bar ℹ button. */
object ProxyInfoPopover {

  private val PopoverClass: String = "proxy-info-popover"

  def install(): Unit = {
    dom.document.addEventListener("click", outsideClickHandler _)
    dom.document.addEventListener("keydown", escapeKeyHandler _)
  }

  @JSExportTopLevel("toggleProxyInfo")
  def toggle(): Unit = {
    val bar = dom.document.getElementById(HtmlIds.ProxyBar)
    if (bar == null) return
    val existing = bar.querySelector("." + PopoverClass)
    if (existing != null) {
      locally { val _ = existing.parentNode.removeChild(existing) }
    } else {
      val pop = dom.document.createElement("div").asInstanceOf[dom.html.Div]
      pop.className = PopoverClass
      pop.innerHTML =
        s"""<div class="proxy-info-popover-title">${I18n.t("proxy.interceptTitle")}</div>""" +
          s"""<div class="proxy-info-popover-body">${I18n.t("proxy.interceptDesc")}</div>"""
      locally { val _ = bar.appendChild(pop) }
    }
  }

  private def outsideClickHandler(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    val insidePopover = target.closest("." + PopoverClass) != null
    val clickedInfoBtn = target.closest("#" + HtmlIds.ProxyInfoBtn) != null
    if (insidePopover || clickedInfoBtn) return
    val pop = dom.document.querySelector("." + PopoverClass)
    if (pop != null) { locally { val _ = pop.parentNode.removeChild(pop) } }
  }

  private def escapeKeyHandler(e: dom.KeyboardEvent): Unit = {
    if (e.key != "Escape") return
    val pop = dom.document.querySelector("." + PopoverClass)
    if (pop != null) { locally { val _ = pop.parentNode.removeChild(pop) } }
  }
}
