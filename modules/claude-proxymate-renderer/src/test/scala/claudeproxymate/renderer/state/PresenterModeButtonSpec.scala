package claudeproxymate.renderer.state

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*

/** `PresenterMode.renderButton`, in jsdom. */
object PresenterModeButtonSpec extends Properties {

  override def tests: List[Test] = List(
    example("renderButton with no mask switch changes nothing", testNoButton),
    example("renderButton shows the switch on when everything is masked", testOn),
    example("renderButton shows the switch off when everything is revealed", testOff),
  )

  private val Page: String = s"""<button id="${HtmlIds.MaskToggleBtn}"><span id="knob"></span></button>"""

  def testNoButton: Result = {
    DomTestKit.setUp("""<i id="other"></i>""")
    PresenterMode.renderButton()
    DomTestKit.element("#other").outerHTML ==== """<i id="other"></i>"""
  }

  def testOn: Result = {
    DomTestKit.setUp(Page)
    PresenterMode.renderButton()
    val btn = DomTestKit.element(s"#${HtmlIds.MaskToggleBtn}")
    Result.all(
      List(
        Result.assert(btn.classList.contains("on")).log("on"),
        btn.getAttribute("aria-checked") ==== "true",
        btn.getAttribute("data-i18n-title") ==== "mask.switchTitleOn",
        Result.assert(DomTestKit.elementOption("#knob").isDefined).log("the knob stays"),
      )
    )
  }

  def testOff: Result = {
    DomTestKit.setUp(Page)
    PresenterMode.renderButton()
    AppState.presenterMaskAll = false
    PresenterMode.renderButton()
    val btn = DomTestKit.element(s"#${HtmlIds.MaskToggleBtn}")
    Result.all(
      List(
        Result.assert(!btn.classList.contains("on")).log("off"),
        btn.getAttribute("aria-checked") ==== "false",
        btn.getAttribute("title") ==== "mask.switchTitleOff",
      )
    )
  }
}
