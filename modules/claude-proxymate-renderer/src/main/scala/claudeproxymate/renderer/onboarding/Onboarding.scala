package claudeproxymate.renderer.onboarding

import claudeproxymate.core.HtmlIds
import org.scalajs.dom

import scala.scalajs.js.annotation.JSExportTopLevel

/** Onboarding modal.
  *
  * Ports `closeOnboard` and the initial onboarding check from renderer.js.
  */
object Onboarding {

  @JSExportTopLevel("closeOnboard")
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
