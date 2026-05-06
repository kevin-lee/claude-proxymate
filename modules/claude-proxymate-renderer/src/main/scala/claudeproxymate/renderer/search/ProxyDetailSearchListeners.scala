package claudeproxymate.renderer.search

import claudeproxymate.core.HtmlIds
import org.scalajs.dom

import scala.scalajs.js

/** Document-level listeners for the search input, clear button, and prev /
  * next nav buttons shared by every proxy detail tab (Request, Response,
  * Analysis). The search input id `HtmlIds.ProxyDetailSearchInput` is
  * unique per active tab — only one search input exists at a time — so
  * `target.id` filtering disambiguates correctly.
  *
  * Replaces the previously-inline `oninput`, `oncompositionstart`,
  * `oncompositionend`, `onclick` (clear), and `onclick`
  * (`navigateSearchMatch`) handlers. Inline handlers were observed not to
  * fire reliably from the Scala.js NoModule output in this Electron
  * version: typing into the search input produced no result updates and
  * apparent focus loss.
  *
  * Composition listeners continue to write to `window._imeComposing`
  * because that flag is shared with `SearchNavigation` (Enter-key shortcut).
  * Migrating to `AppState.imeComposing` is a follow-up; doing it here
  * would require flipping `MessageRenderer` and the SearchNavigation
  * reader together.
  */
object ProxyDetailSearchListeners {

  def install(): Unit = {
    dom.document.addEventListener("click", handleClick _)
    dom.document.addEventListener("input", handleInput _)
    dom.document.addEventListener("compositionstart", handleCompositionStart _)
    dom.document.addEventListener("compositionend", handleCompositionEnd _)
  }

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return

    // Search-nav prev / next (only present when query is non-empty).
    if (target.closest(".search-nav-prev") != null) {
      SearchNavigation.navigateSearchMatch(-1)
      return
    }
    if (target.closest(".search-nav-next") != null) {
      SearchNavigation.navigateSearchMatch(1)
      return
    }

    // Clear button. Only act if the proxy detail search input exists in the
    // current DOM (Messages tab also uses .msg-search-clear, but its input
    // id is different and is handled by MessageRenderer).
    if (target.closest(".msg-search-clear") != null) {
      val inp = dom.document.getElementById(HtmlIds.ProxyDetailSearchInput)
      if (inp == null) return
      SearchNavigation.setProxyDetailSearch("")
      inp.asInstanceOf[dom.html.Input].focus()
    }
  }

  private def handleInput(e: dom.Event): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null || target.id != HtmlIds.ProxyDetailSearchInput) return
    SearchNavigation.setProxyDetailSearch(target.asInstanceOf[dom.html.Input].value)
  }

  private def handleCompositionStart(e: dom.Event): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null || target.id != HtmlIds.ProxyDetailSearchInput) return
    dom.window.asInstanceOf[js.Dynamic]._imeComposing = true
  }

  private def handleCompositionEnd(e: dom.Event): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null || target.id != HtmlIds.ProxyDetailSearchInput) return
    dom.window.asInstanceOf[js.Dynamic]._imeComposing = false
    SearchNavigation.setProxyDetailSearch(target.asInstanceOf[dom.html.Input].value)
  }
}
