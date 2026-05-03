package claudeproxymate.renderer.search

import claudeproxymate.core.HtmlIds
import org.scalajs.dom

import scala.scalajs.js

/** Document-level listeners for the search input and clear button shared
  * by the Analysis tab and the (still inline-handler) Detail tabs. The
  * input id `HtmlIds.ProxyDetailSearchInput` is unique per active tab —
  * only one search input exists at a time — so `target.id` filtering
  * disambiguates correctly.
  *
  * Replaces inline `onclick="setProxyDetailSearch('')..."`,
  * `oninput="setProxyDetailSearch(this.value)"`,
  * `oncompositionstart="_imeComposing=true"`,
  * `oncompositionend="_imeComposing=false;setProxyDetailSearch(this.value)"`.
  *
  * Composition listeners continue to write to `window._imeComposing`
  * because that flag is shared with `SearchNavigation` and (until A3h)
  * with `DetailView`'s remaining inline handlers.
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
    val clear = target.closest(".msg-search-clear")
    if (clear == null) return
    // Only trigger when the clear button is *next to* the proxy detail
    // search input — i.e., the input with this id is currently in the DOM.
    val inp = dom.document.getElementById(HtmlIds.ProxyDetailSearchInput)
    if (inp == null) return
    SearchNavigation.setProxyDetailSearch("")
    inp.asInstanceOf[dom.html.Input].focus()
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
