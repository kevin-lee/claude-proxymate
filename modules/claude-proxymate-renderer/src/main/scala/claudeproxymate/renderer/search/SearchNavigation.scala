package claudeproxymate.renderer.search

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.Debounce
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Search navigation within proxy detail views.
  *
  * Ports `navigateSearchMatch`, `updateSearchCounter`,
  * `setProxyDetailSearch` from renderer.js.
  */
object SearchNavigation {

  private val detailSearchDebounce = new Debounce(150)

  @JSExportTopLevel("setProxyDetailSearch")
  def setProxyDetailSearch(q: String): Unit = {
    AppState.proxyDetailSearch = q
    // _imeComposing is set by Scala-side composition handlers
    // (ProxyDetailSearchListeners) and the still-inline writers will
    // migrate together with A3h.
    val imeComposing = dom.window.asInstanceOf[js.Dynamic].selectDynamic("_imeComposing")
    if (!js.isUndefined(imeComposing) && imeComposing.asInstanceOf[Boolean]) return

    detailSearchDebounce { () =>
      AppState.detailSearchWasFocused = dom.document.activeElement match {
        case el: dom.html.Element => el.id == HtmlIds.ProxyDetailSearchInput
        case _                    => false
      }
      AppState.searchCurrentIdx = -1
      if (q.isEmpty) AppState.proxyDetailMechFilter = None

      claudeproxymate.renderer.detail.DetailView.renderProxyDetail()

      if (q.nonEmpty) navigateSearchMatch(0)
    }
  }

  @JSExportTopLevel("navigateSearchMatch")
  def navigateSearchMatch(delta: Int): Unit = {
    val container = {
      // Messages tab: the cards live directly in the proxy detail container.
      // Other tabs: marks live inside #proxyDetailCode (json-tree-view) or
      // inside .analysis-view.
      if (AppState.proxyDetailTab == "messages")
        dom.document.getElementById(HtmlIds.ProxyDetailView)
      else {
        val el = dom.document.getElementById(HtmlIds.ProxyDetailCode)
        if (el != null) el
        else dom.document.querySelector(".analysis-view")
      }
    }
    if (container == null) return

    val marks = container.querySelectorAll("mark.search-hl")
    if (marks.length == 0) return

    // Remove current highlight
    val prev = container.querySelector("mark.search-hl.current")
    if (prev != null) { locally { val _ = prev.classList.remove("current") } }

    // Calculate new index
    if (delta == 0) AppState.searchCurrentIdx = 0
    else {
      val len = marks.length
      AppState.searchCurrentIdx = ((AppState.searchCurrentIdx + delta) % len + len) % len
    }

    val mark = marks(AppState.searchCurrentIdx).asInstanceOf[dom.html.Element]
    locally { val _ = mark.classList.add("current") }
    // Many JSON-tree matches live inside collapsed long-string blocks
    // (`jt-str-expanded[display:none]`) or collapsed object/array bodies
    // (`*-b[display:none]`). `scrollIntoView` is a no-op on hidden elements,
    // so expand any collapsed ancestors first.
    claudeproxymate.renderer.analysis.MechHighlight.expandAncestors(mark)
    locally { val _ = mark.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal("behavior" -> "smooth", "block" -> "center")) }
    updateSearchCounter(marks.length)
  }

  @JSExportTopLevel("updateSearchCounter")
  def updateSearchCounter(total: Int): Unit = {
    val el = dom.document.getElementById(HtmlIds.SearchCounter)
    if (el != null) {
      el.textContent = if (total > 0) s"${AppState.searchCurrentIdx + 1}/$total" else "0"
    }
  }
}
