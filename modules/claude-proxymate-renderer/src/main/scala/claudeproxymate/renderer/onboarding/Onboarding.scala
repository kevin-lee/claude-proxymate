package claudeproxymate.renderer.onboarding

import cats.syntax.all.*
import claudeproxymate.core.HtmlIds
import org.scalajs.dom

/** Onboarding modal.
  *
  * Ports the initial onboarding check from renderer.js. The carousel
  * (prev / next / dot) and close-button clicks are dispatched by
  * [[install]] via doc-level click delegation. Inline `onclick`
  * handlers were removed because Scala.js NoModule `let`-exported
  * globals are unreliable when invoked from inline HTML attributes in
  * this Electron version, and the strict CSP (`script-src 'self'`)
  * forbids inline handlers entirely.
  */
object Onboarding {

  private val SlideCount: Int = 3

  private var currentSlide: Int = 0

  def install(): Unit = {
    dom.document.addEventListener("click", handleClick(_))
    dom.document.addEventListener("keydown", handleKeydown(_))
  }

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    if (target.closest(s"#${HtmlIds.OnboardPrev}") != null) goTo(currentSlide - 1)
    else if (target.closest(s"#${HtmlIds.OnboardNext}") != null) goTo(currentSlide + 1)
    else {
      val dot = target.closest(".onboard-dot")
      if (dot != null) {
        val raw = dot.asInstanceOf[dom.html.Element].dataset.get("onboardSlide").getOrElse("")
        raw.toIntOption.foreach(goTo)
      } else if (target.closest(s"#${HtmlIds.OnboardCloseBtn}") != null) closeOnboard()
    }
  }

  /** Arrow-key slide navigation, active only while the modal is visible.
    * Does not handle Escape (left to the search-clear keydown handler).
    */
  private def handleKeydown(e: dom.KeyboardEvent): Unit = {
    if (!isModalVisible) return
    e.key match {
      case "ArrowLeft" =>
        e.preventDefault()
        goTo(currentSlide - 1)
      case "ArrowRight" =>
        e.preventDefault()
        goTo(currentSlide + 1)
      case _ => ()
    }
  }

  private def isModalVisible: Boolean = {
    val modal = dom.document.getElementById(HtmlIds.OnboardModal)
    modal != null && modal.asInstanceOf[dom.html.Element].style.display =!= "none"
  }

  /** Move to slide `n` (clamped, non-wrapping) and re-render the carousel. */
  private def goTo(n: Int): Unit = {
    val clamped = math.max(0, math.min(SlideCount - 1, n))
    if (clamped === currentSlide) return
    currentSlide = clamped
    render()
  }

  /** Reflect `currentSlide` in the track transform, dot highlight, and
    * arrow visibility (prev hidden on slide 0, next hidden on last slide).
    */
  private def render(): Unit = {
    val track = dom.document.getElementById(HtmlIds.OnboardTrack)
    if (track != null)
      track.asInstanceOf[dom.html.Element].style.transform = s"translateX(-${currentSlide * 100}%)"

    dom.document.querySelectorAll(".onboard-dot").foreach { node =>
      val el  = node.asInstanceOf[dom.html.Element]
      val idx = el.dataset.get("onboardSlide").getOrElse("")
      if (idx === currentSlide.toString) el.classList.add("active")
      else el.classList.remove("active")
    }

    setHidden(HtmlIds.OnboardPrev, currentSlide === 0)
    setHidden(HtmlIds.OnboardNext, currentSlide === SlideCount - 1)
    // "Get Started →" pill replaces the › arrow on the last slide only.
    setHidden(HtmlIds.OnboardCloseBtn, currentSlide =!= SlideCount - 1)
  }

  private def setHidden(id: String, hidden: Boolean): Unit = {
    val el = dom.document.getElementById(id)
    if (el != null) {
      if (hidden) el.asInstanceOf[dom.html.Element].classList.add("is-hidden")
      else el.asInstanceOf[dom.html.Element].classList.remove("is-hidden")
    }
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
      if (modal != null) {
        modal.asInstanceOf[dom.html.Element].style.display = "flex"
        currentSlide = 0
        render()
      }
    }
  }
}
