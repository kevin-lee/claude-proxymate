package claudeproxymate.renderer.proxy

import cats.syntax.all.*
import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.view.ViewHelpers
import org.scalajs.dom

/** Click-toggle info popover for the proxy bar ℹ button.
  *
  * One document-level click listener dispatches three cases:
  *   - click on the ℹ button → toggle the popover
  *   - click inside the popover → ignore
  *   - click anywhere else → close the popover if open
  */
object ProxyInfoPopover {

  def install(): Unit = {
    dom.document.addEventListener("click", handleClick _)
    dom.document.addEventListener("keydown", escapeKeyHandler _)
  }

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    if (target.closest("#" + HtmlIds.ProxyInfoBtn) != null) { toggle(); return }
    if (target.closest("." + ProxyInfoPopoverView.PopoverClass) != null) return
    closeIfOpen()
  }

  private def escapeKeyHandler(e: dom.KeyboardEvent): Unit = {
    if (e.key =!= "Escape") return
    closeIfOpen()
  }

  private def toggle(): Unit = {
    val bar      = dom.document.getElementById(HtmlIds.ProxyBar)
    if (bar == null) return
    val existing = bar.querySelector("." + ProxyInfoPopoverView.PopoverClass)
    if (existing != null) {
      locally { val _ = existing.parentNode.removeChild(existing) }
    } else {
      val pop    = dom.document.createElement("div").asInstanceOf[dom.html.Div]
      pop.className = ProxyInfoPopoverView.PopoverClass
      val labels = ProxyInfoPopoverLabels(
        title = I18n.t("proxy.interceptTitle"),
        desc = I18n.t("proxy.interceptDesc"),
      )
      ViewHelpers.setInnerHtml(pop, ProxyInfoPopoverView.buildPopoverFrag(labels))
      locally { val _ = bar.appendChild(pop) }
    }
  }

  private def closeIfOpen(): Unit = {
    val pop = dom.document.querySelector("." + ProxyInfoPopoverView.PopoverClass)
    if (pop != null) { locally { val _ = pop.parentNode.removeChild(pop) } }
  }
}
