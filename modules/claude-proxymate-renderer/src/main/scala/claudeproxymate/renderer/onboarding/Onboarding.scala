package claudeproxymate.renderer.onboarding

import cats.syntax.all.*
import claudeproxymate.core.{GettingStarted, HtmlIds, OnboardingStage, TourPlacement}
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.view.ViewHelpers
import org.scalajs.dom
import scala.scalajs.js

/** First-run Getting Started: a motion-graphic intro in the onboarding
  * modal, then a guided tour that spotlights the real controls.
  *
  * The app underneath is `inert` (no clicks, no focus) from the moment
  * the intro opens until the tour is done or skipped. Every control is
  * dispatched by document-level click delegation because the strict CSP
  * (`script-src 'self'`) forbids inline handlers. The stage transitions
  * are the pure [[GettingStarted.next]] / [[GettingStarted.back]].
  */
object Onboarding {

  private var stage: OnboardingStage = OnboardingStage.Done

  /** Padding around the spotlighted element, in px. */
  private val SpotPadding: Double = 6.0

  private val PlayClass: String  = "gs-play"
  private val NudgeClass: String = "gs-nudge"

  /** App regions made `inert` while onboarding runs. */
  private val AppRegionIds: List[String] =
    List(HtmlIds.AppHeader, HtmlIds.ProxyBar, HtmlIds.ProxyPanel, HtmlIds.StatusBar)

  def isActive: Boolean = stage =!= OnboardingStage.Done

  def install(): Unit = {
    dom.document.addEventListener("click", handleClick(_))
    dom.document.addEventListener("keydown", handleKeydown(_))
    dom.window.addEventListener("resize", (_: dom.UIEvent) => if (isTour) renderStep() else ())
  }

  /** Show the onboarding when the stored last-seen version is older than
    * [[GettingStarted.Version]]. The new version is stored as soon as it
    * opens, so quitting midway does not show it again. Called from
    * RendererMain once the locales are loaded.
    */
  def showIfNeeded(): Unit = {
    val stored = Option(dom.window.localStorage.getItem(GettingStarted.StorageKey))
    if (GettingStarted.shouldShow(stored)) {
      dom.window.localStorage.setItem(GettingStarted.StorageKey, GettingStarted.Version.toString)
      start()
    } else ()
  }

  /** Replay from the About panel. Does not touch the stored version. */
  def replay(): Unit = start()

  private def start(): Unit = {
    stage = OnboardingStage.Intro
    setAppInert(true)
    setDisplay(HtmlIds.TourLayer, "none")
    setDisplay(HtmlIds.OnboardModal, "flex")
    playMotion()
    focusById(HtmlIds.OnboardTourBtn)
  }

  /** (Re)start the motion timeline. Removing and re-adding `gs-play`
    * with a forced reflow in between restarts every CSS animation.
    * Under reduced motion the class is never added, so the stage shows
    * its static end frame.
    */
  private def playMotion(): Unit = {
    val el = dom.document.getElementById(HtmlIds.OnboardStage)
    if (el != null) {
      val stageEl = el.asInstanceOf[dom.html.Element]
      stageEl.classList.remove(PlayClass)
      locally { val _ = stageEl.offsetWidth }
      if (!prefersReducedMotion) stageEl.classList.add(PlayClass) else ()
    } else ()
  }

  private def prefersReducedMotion: Boolean =
    dom.window.matchMedia("(prefers-reduced-motion: reduce)").matches

  private def startTour(): Unit = {
    stage = GettingStarted.next(OnboardingStage.Intro)
    setDisplay(HtmlIds.OnboardModal, "none")
    stopMotion()
    setDisplay(HtmlIds.TourLayer, "block")
    renderStep()
  }

  private def isTour: Boolean = stage match {
    case OnboardingStage.Tour(_) => true
    case OnboardingStage.Intro | OnboardingStage.Done => false
  }

  /** Spotlight the current step's target and place its callout. A step
    * whose target is missing or has no size is skipped.
    */
  private def renderStep(): Unit = stage match {
    case OnboardingStage.Tour(index) =>
      val step   = GettingStarted.tourSteps(index)
      val target = dom.document.getElementById(step.targetId)
      val rect   = Option(target).map(_.getBoundingClientRect())
      rect match {
        case Some(r) if r.width > 0 && r.height > 0 =>
          val padded = TourLayout.pad(TourLayout.Rect(r.left, r.top, r.width, r.height), SpotPadding)
          positionSpot(padded)
          renderCallout(index, padded, step.titleKey, step.bodyKey, step.placement)
        case Some(_) | None =>
          advance()
      }
    case OnboardingStage.Intro | OnboardingStage.Done => ()
  }

  private def positionSpot(padded: TourLayout.Rect): Unit = {
    val spot = dom.document.getElementById(HtmlIds.TourSpot)
    if (spot != null) {
      val style = spot.asInstanceOf[dom.html.Element].style
      style.left = s"${padded.left}px"
      style.top = s"${padded.top}px"
      style.width = s"${padded.width}px"
      style.height = s"${padded.height}px"
    } else ()
  }

  private def renderCallout(
    index: Int,
    padded: TourLayout.Rect,
    titleKey: String,
    bodyKey: String,
    preferred: TourPlacement,
  ): Unit = {
    val el = dom.document.getElementById(HtmlIds.TourCallout)
    if (el != null) {
      val callout = el.asInstanceOf[dom.html.Element]
      val total   = GettingStarted.tourSteps.size
      val isLast  = index === total - 1
      val labels  = TourCalloutLabels(
        progress = I18n.t("tour.progress", Map("step" -> (index + 1).toString, "total" -> total.toString)),
        title = I18n.t(titleKey),
        body = I18n.t(bodyKey),
        back = I18n.t("tour.back"),
        next = I18n.t(if (isLast) "tour.done" else "tour.next"),
        skip = I18n.t("tour.skip"),
        isFirst = index === 0,
      )
      ViewHelpers.setInnerHtml(callout, TourCalloutView.build(labels))

      val box      = TourLayout.Size(callout.offsetWidth, callout.offsetHeight)
      val viewport = TourLayout.Size(dom.window.innerWidth, dom.window.innerHeight)
      val position = TourLayout.place(padded, box, viewport, preferred)
      callout.className = s"tour-callout tour-callout--${placementSuffix(position.placement)}"
      callout.style.left = s"${position.left}px"
      callout.style.top = s"${position.top}px"
      callout.style.setProperty("--tour-arrow", s"${position.arrowOffset}px")

      val next = callout.querySelector(s"""[${TourCalloutView.ActionAttr}="next"]""")
      if (next != null) next.asInstanceOf[dom.html.Element].focus() else ()
    } else ()
  }

  private def placementSuffix(placement: TourPlacement): String = placement match {
    case TourPlacement.Above => "above"
    case TourPlacement.Below => "below"
    case TourPlacement.Left => "left"
    case TourPlacement.Right => "right"
  }

  private def advance(): Unit = {
    stage = GettingStarted.next(stage)
    stage match {
      case OnboardingStage.Done => finish()
      case OnboardingStage.Tour(_) | OnboardingStage.Intro => renderStep()
    }
  }

  private def goBack(): Unit = {
    stage = GettingStarted.back(stage)
    renderStep()
  }

  private def finish(): Unit = {
    stage = OnboardingStage.Done
    setDisplay(HtmlIds.OnboardModal, "none")
    setDisplay(HtmlIds.TourLayer, "none")
    stopMotion()
    setAppInert(false)
    nudgeStartButton()
  }

  private def stopMotion(): Unit = {
    val el = dom.document.getElementById(HtmlIds.OnboardStage)
    if (el != null) el.classList.remove(PlayClass) else ()
  }

  /** scalajs-dom has no `inert` facade, so it is set dynamically. */
  private def setAppInert(on: Boolean): Unit =
    AppRegionIds.foreach { id =>
      val el = dom.document.getElementById(id)
      if (el != null) el.asInstanceOf[js.Dynamic].inert = on else ()
    }

  /** "Your turn": pulse the real Start Proxy button until its first click. */
  private def nudgeStartButton(): Unit =
    if (!AppState.proxyRunning) {
      val btn = dom.document.getElementById(HtmlIds.ProxyStartBtn)
      if (btn != null) btn.classList.add(NudgeClass) else ()
    } else ()

  private def setDisplay(id: String, display: String): Unit = {
    val el = dom.document.getElementById(id)
    if (el != null) el.asInstanceOf[dom.html.Element].style.display = display else ()
  }

  private def focusById(id: String): Unit = {
    val el = dom.document.getElementById(id)
    if (el != null) el.asInstanceOf[dom.html.Element].focus() else ()
  }

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    if (target.closest(s"#${HtmlIds.OnboardSkipBtn}") != null) finish()
    else if (target.closest(s"#${HtmlIds.OnboardReplayBtn}") != null) playMotion()
    else if (target.closest(s"#${HtmlIds.OnboardTourBtn}") != null) startTour()
    else {
      val action = target.closest(s"[${TourCalloutView.ActionAttr}]")
      if (action != null) {
        action.getAttribute(TourCalloutView.ActionAttr) match {
          case "next" => advance()
          case "back" => goBack()
          case "skip" => finish()
          case _ => ()
        }
      } else {
        val start = target.closest(s"#${HtmlIds.ProxyStartBtn}")
        if (start != null) start.classList.remove(NudgeClass) else ()
      }
    }
  }

  /** Escape skips; in the tour, ←/→ step back and forward. Enter and
    * Space are left to the focused button.
    */
  private def handleKeydown(e: dom.KeyboardEvent): Unit = {
    if (!isActive) return
    e.key match {
      case "Escape" =>
        e.preventDefault()
        finish()
      case "ArrowRight" if isTour =>
        e.preventDefault()
        advance()
      case "ArrowLeft" if isTour =>
        e.preventDefault()
        goBack()
      case _ => ()
    }
  }
}
