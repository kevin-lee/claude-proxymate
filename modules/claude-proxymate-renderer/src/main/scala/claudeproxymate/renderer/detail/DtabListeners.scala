package claudeproxymate.renderer.detail

import org.scalajs.dom

/** Document-level click listener for the four detail-pane tab buttons
  * (Messages / Request / Response / Analysis).
  *
  * Replaces inline `onclick="showDetailTab('...')"` handlers in the
  * generated `index.html`. Inline handlers are unreliable in Scala.js
  * NoModule output in this Electron version (their `let`-exported names
  * sometimes resolve to undefined at the inline-handler call site —
  * symptom: clicks silently do nothing). Doc-level delegation goes
  * through ordinary Scala dispatch.
  */
object DtabListeners {

  val DtabClass: String    = "dtab"
  val DtabDataAttr: String = "data-dtab"

  def install(): Unit =
    dom.document.addEventListener("click", handleClick _)

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    val btn = target.closest(s".$DtabClass[$DtabDataAttr]")
    if (btn == null) return
    val tab = btn.asInstanceOf[dom.html.Element].getAttribute(DtabDataAttr)
    if (tab != null && tab.nonEmpty) DetailView.showDetailTab(tab)
  }
}
