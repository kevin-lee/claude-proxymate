package claudeproxymate.renderer.theme

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*

/** The click handler of [[Theme]], in jsdom. */
object ThemeHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("a click on the theme button goes to the next theme", testToggle),
    example("a click inside the theme button goes to the next theme", testClickInside),
    example("a click outside the theme button changes nothing", testOutside),
  )

  private val Page: String =
    s"""<button id="${HtmlIds.ThemeToggleBtn}"><span id="inner">T</span></button><i id="other"></i>"""

  def testToggle: Result = {
    DomTestKit.setUp(Page)
    Theme.setSelection("system")
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ThemeToggleBtn}"))
    val r = Result.all(List(Theme.selection ==== "light", DomTestKit.listenerErrors ==== Nil))
    Theme.setSelection("system")
    r
  }

  def testClickInside: Result = {
    DomTestKit.setUp(Page)
    Theme.setSelection("light")
    val _ = DomTestKit.click(DomTestKit.element("#inner"))
    val r = Result.all(List(Theme.selection ==== "dark", DomTestKit.listenerErrors ==== Nil))
    Theme.setSelection("system")
    r
  }

  def testOutside: Result = {
    DomTestKit.setUp(Page)
    Theme.setSelection("system")
    val _ = DomTestKit.click(DomTestKit.element("#other"))
    Result.all(List(Theme.selection ==== "system", DomTestKit.listenerErrors ==== Nil))
  }
}
