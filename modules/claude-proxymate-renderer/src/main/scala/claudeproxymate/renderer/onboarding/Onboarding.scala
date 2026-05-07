package claudeproxymate.renderer.onboarding

import claudeproxymate.core.HtmlIds
import org.scalajs.dom

/** Onboarding modal.
  *
  * Ports the initial onboarding check from renderer.js. The close
  * button click is dispatched by [[install]] (doc-level click listener
  * filtered by `#onboardCloseBtn`). Inline `onclick="closeOnboard()"`
  * was removed because Scala.js NoModule `let`-exported globals are
  * unreliable when invoked from inline HTML attributes in this
  * Electron version.
  */
object Onboarding {

  def install(): Unit =
    dom.document.addEventListener("click", handleClick _)

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    if (target.closest(s"#${HtmlIds.OnboardCloseBtn}") != null) closeOnboard()
  }

  def closeOnboard(): Unit = {
    val modal = dom.document.getElementById(HtmlIds.OnboardModal)
    if (modal != null) modal.asInstanceOf[dom.html.Element].style.display = "none"
    dom.window.localStorage.setItem("ci-onboarded", "1")
  }

  /** Show onboarding modal if user has not been onboarded. Called from RendererMain. */
  def showIfNeeded(): Unit = {
    if (dom.window.localStorage.getItem("ci-onboarded") == null) {
      val modal = dom.document.getElementById(HtmlIds.OnboardModal)
      if (modal != null) modal.asInstanceOf[dom.html.Element].style.display = "flex"
    }
  }
}
