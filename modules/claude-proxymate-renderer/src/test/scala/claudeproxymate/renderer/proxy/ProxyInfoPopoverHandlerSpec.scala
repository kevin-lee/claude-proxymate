package claudeproxymate.renderer.proxy

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*

/** The click and keydown handlers of [[ProxyInfoPopover]], in jsdom. */
object ProxyInfoPopoverHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("a click on the info button opens the popover", testOpen),
    example("a second click on the info button closes the popover", testToggleClose),
    example("a click on the info button with no app header changes nothing", testNoHeader),
    example("a click on the Getting Started button closes the popover and replays onboarding", testReplay),
    example("a click inside the popover keeps it open", testClickInside),
    example("a click outside the popover closes it", testClickOutside),
    example("Escape closes the popover", testEscape),
    example("another key keeps the popover open", testOtherKey),
  )

  private val PopoverSelector: String = s".${ProxyInfoPopoverView.PopoverClass}"

  private val Page: String =
    s"""<div id="${HtmlIds.AppHeader}"><button id="${HtmlIds.ProxyInfoBtn}"><span id="icon">i</span></button></div><i id="other"></i><div id="${HtmlIds.OnboardModal}" style="display:none"></div>"""

  private def isOpen: Boolean = DomTestKit.elementOption(PopoverSelector).isDefined

  private def openPopover(): Unit = {
    DomTestKit.setUp(Page)
    val _ = DomTestKit.click(DomTestKit.element("#icon"))
  }

  def testOpen: Result = {
    openPopover()
    Result.all(List(Result.assert(isOpen).log("the popover is open"), DomTestKit.listenerErrors ==== Nil))
  }

  def testToggleClose: Result = {
    openPopover()
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ProxyInfoBtn}"))
    Result.all(List(Result.assert(!isOpen).log("the popover is closed"), DomTestKit.listenerErrors ==== Nil))
  }

  def testNoHeader: Result = {
    DomTestKit.setUp(s"""<button id="${HtmlIds.ProxyInfoBtn}">i</button>""")
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ProxyInfoBtn}"))
    Result.all(List(Result.assert(!isOpen).log("no popover"), DomTestKit.listenerErrors ==== Nil))
  }

  def testReplay: Result = {
    openPopover()
    val _ = DomTestKit.click(DomTestKit.element(s"[${ProxyInfoPopoverView.ReplayAttr}]"))
    Result.all(
      List(
        Result.assert(!isOpen).log("the popover is closed"),
        DomTestKit.html(s"#${HtmlIds.OnboardModal}").style.display ==== "flex",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testClickInside: Result = {
    openPopover()
    val _ = DomTestKit.click(DomTestKit.element(s"$PopoverSelector-title"))
    Result.all(List(Result.assert(isOpen).log("the popover is open"), DomTestKit.listenerErrors ==== Nil))
  }

  def testClickOutside: Result = {
    openPopover()
    val _ = DomTestKit.click(DomTestKit.element("#other"))
    Result.all(List(Result.assert(!isOpen).log("the popover is closed"), DomTestKit.listenerErrors ==== Nil))
  }

  def testEscape: Result = {
    openPopover()
    val _ = DomTestKit.keyDown(DomTestKit.element("#other"), "Escape")
    Result.all(List(Result.assert(!isOpen).log("the popover is closed"), DomTestKit.listenerErrors ==== Nil))
  }

  def testOtherKey: Result = {
    openPopover()
    val _ = DomTestKit.keyDown(DomTestKit.element("#other"), "a")
    Result.all(List(Result.assert(isOpen).log("the popover is open"), DomTestKit.listenerErrors ==== Nil))
  }
}
