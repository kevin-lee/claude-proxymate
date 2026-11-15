package claudeproxymate.renderer.onboarding

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*
import org.scalajs.dom

import scala.scalajs.js

/** The keydown handler of [[Onboarding]] (and the click actions it shares),
  * in jsdom. jsdom gives every element a zero-size rectangle, so the tour
  * targets get a fixed rectangle here. The spotlight's left edge is the
  * target's left edge minus 6 (the padding), which tells the step.
  */
object OnboardingHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("keys do nothing when onboarding is not active", testInactive),
    example("Escape on the intro modal finishes onboarding", testEscapeIntro),
    example("an arrow key on the intro modal does nothing", testArrowIntro),
    example("another key on the intro modal does nothing", testOtherKeyIntro),
    example("the tour button starts the tour at step 1", testStartTour),
    example("ArrowRight in the tour goes to the next step", testArrowRight),
    example("ArrowLeft in the tour goes to the previous step", testArrowLeft),
    example("Escape in the tour finishes onboarding", testEscapeTour),
    example("the skip button finishes onboarding", testSkip),
    example("a click on the Start Proxy button stops its nudge", testStartNudge),
  )

  private val Page: String =
    s"""<div id="${HtmlIds.OnboardModal}" style="display:none"><button id="${HtmlIds.OnboardTourBtn}">Tour</button><button id="${HtmlIds.OnboardSkipBtn}">Skip</button></div>
       |<div id="${HtmlIds.TourLayer}" style="display:none"><div id="${HtmlIds.TourSpot}"></div><div id="${HtmlIds.TourCallout}"></div></div>
       |<button id="${HtmlIds.ProxyStartBtn}">Start</button><div id="${HtmlIds.RouteSeg}"></div><i id="other"></i>""".stripMargin

  private def setRect(id: String, left: Double): Unit = {
    val el = dom.document.getElementById(id).asInstanceOf[js.Dynamic] // scalafix:ok DisableSyntax.asInstanceOf
    el.getBoundingClientRect = { () =>
      js.Dynamic.literal(left = left, top = 10, width = 20, height = 20, right = left + 20, bottom = 30)
    }: js.Function0[js.Dynamic]
  }

  private def setUpPage(): Unit = {
    DomTestKit.setUp(Page)
    setRect(HtmlIds.ProxyStartBtn, 10)
    setRect(HtmlIds.RouteSeg, 100)
  }

  private def display(id: String): String = DomTestKit.html(s"#$id").style.display

  private def spotLeft: String = DomTestKit.html(s"#${HtmlIds.TourSpot}").style.left

  private def other: dom.Element = DomTestKit.element("#other")

  private def startIntro(): Unit = {
    setUpPage()
    Onboarding.replay()
  }

  private def startTour(): Unit = {
    startIntro()
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.OnboardTourBtn}"))
  }

  def testInactive: Result = {
    setUpPage()
    val notCancelled = DomTestKit.keyDown(other, "Escape")
    Result.all(
      List(
        Result.assert(notCancelled).log("Escape is not cancelled"),
        Result.assert(!Onboarding.isActive).log("not active"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testEscapeIntro: Result = {
    startIntro()
    val notCancelled = DomTestKit.keyDown(other, "Escape")
    Result.all(
      List(
        Result.assert(!notCancelled).log("Escape is cancelled"),
        Result.assert(!Onboarding.isActive).log("finished"),
        display(HtmlIds.OnboardModal) ==== "none",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testArrowIntro: Result = {
    startIntro()
    val notCancelled = DomTestKit.keyDown(other, "ArrowRight")
    Result.all(
      List(
        Result.assert(notCancelled).log("ArrowRight is not cancelled"),
        Result.assert(Onboarding.isActive).log("still active"),
        display(HtmlIds.OnboardModal) ==== "flex",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testOtherKeyIntro: Result = {
    startIntro()
    val notCancelled = DomTestKit.keyDown(other, "a")
    Result.all(
      List(
        Result.assert(notCancelled).log("the key is not cancelled"),
        Result.assert(Onboarding.isActive).log("still active"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testStartTour: Result = {
    startTour()
    Result.all(
      List(
        display(HtmlIds.OnboardModal) ==== "none",
        display(HtmlIds.TourLayer) ==== "block",
        spotLeft ==== "4px",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testArrowRight: Result = {
    startTour()
    val notCancelled = DomTestKit.keyDown(other, "ArrowRight")
    Result.all(
      List(
        Result.assert(!notCancelled).log("ArrowRight is cancelled"),
        spotLeft ==== "94px",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testArrowLeft: Result = {
    startTour()
    val _            = DomTestKit.keyDown(other, "ArrowRight")
    val notCancelled = DomTestKit.keyDown(other, "ArrowLeft")
    Result.all(
      List(
        Result.assert(!notCancelled).log("ArrowLeft is cancelled"),
        spotLeft ==== "4px",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testEscapeTour: Result = {
    startTour()
    val _ = DomTestKit.keyDown(other, "Escape")
    Result.all(
      List(
        Result.assert(!Onboarding.isActive).log("finished"),
        display(HtmlIds.TourLayer) ==== "none",
        Result
          .assert(DomTestKit.element(s"#${HtmlIds.ProxyStartBtn}").classList.contains("gs-nudge"))
          .log("the Start Proxy button is nudged"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testSkip: Result = {
    startIntro()
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.OnboardSkipBtn}"))
    Result.all(
      List(
        Result.assert(!Onboarding.isActive).log("finished"),
        display(HtmlIds.OnboardModal) ==== "none",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testStartNudge: Result = {
    startIntro()
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.OnboardSkipBtn}"))
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ProxyStartBtn}"))
    Result.all(
      List(
        Result
          .assert(!DomTestKit.element(s"#${HtmlIds.ProxyStartBtn}").classList.contains("gs-nudge"))
          .log("the nudge is gone"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }
}
