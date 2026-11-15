package claudeproxymate.renderer.i18n

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*

/** The click handler of [[I18n]], in jsdom. */
object I18nHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("a click on the language button changes the locale", testToggle),
    example("a click outside the language button changes nothing", testOutside),
  )

  private val Page: String =
    s"""<button id="${HtmlIds.LangToggleBtn}"><span id="inner">EN</span></button><i id="other"></i>"""

  def testToggle: Result = {
    DomTestKit.setUp(Page)
    /* setLocale sets the text of the button, so #inner is gone after it. */
    I18n.setLocale("en")
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.LangToggleBtn}"))
    val r = Result.all(
      List(
        I18n.locale ==== "ko",
        DomTestKit.element(s"#${HtmlIds.LangToggleBtn}").textContent ==== "한",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
    I18n.setLocale("en")
    r
  }

  def testOutside: Result = {
    DomTestKit.setUp(Page)
    I18n.setLocale("en")
    val _ = DomTestKit.click(DomTestKit.element("#other"))
    Result.all(List(I18n.locale ==== "en", DomTestKit.listenerErrors ==== Nil))
  }
}
