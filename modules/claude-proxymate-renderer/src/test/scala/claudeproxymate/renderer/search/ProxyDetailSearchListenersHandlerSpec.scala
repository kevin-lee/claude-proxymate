package claudeproxymate.renderer.search

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*
import org.scalajs.dom

/** The handlers of [[ProxyDetailSearchListeners]], in jsdom.
  *
  * `MessageRenderer` also handles `.msg-search-clear` (for the messages
  * search, with its own debounce timer), so the tests of the clear button
  * do not count the timers.
  */
object ProxyDetailSearchListenersHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("the previous button goes to the previous match", testPrev),
    example("the next button goes to the next match", testNext),
    example("the next button inside a clear button does not clear the search", testNextNotClear),
    example("the clear button with no detail search input changes nothing", testClearNoInput),
    example("the clear button clears the search and focuses the input", testClear),
    example("a click elsewhere changes nothing", testOutside),
    example("input in the detail search input sets the search", testInput),
    example("input in another input does not set the search", testOtherInput),
    example("compositionstart in the detail search input sets the IME flag", testCompositionStart),
    example("compositionstart in another element does not set the IME flag", testCompositionStartOther),
    example("compositionend in the detail search input clears the IME flag and sets the search", testCompositionEnd),
    example("compositionend in another element changes nothing", testCompositionEndOther),
  )

  private val Marks: String =
    """<mark class="search-hl" id="m1">x</mark><mark class="search-hl" id="m2">x</mark><mark class="search-hl" id="m3">x</mark>"""

  private val Page: String =
    s"""<span id="${HtmlIds.SearchCounter}">-</span><div id="${HtmlIds.ProxyDetailView}">$Marks</div>
       |<button class="search-nav-prev">p</button><button class="search-nav-next"><span id="next-inner">n</span></button>
       |<span class="msg-search-clear" id="clear"><span class="search-nav-next" id="nested-next">n</span></span>
       |<input id="${HtmlIds.ProxyDetailSearchInput}" value="q"><input id="other-input"><i id="other"></i>""".stripMargin

  private def input: dom.html.Input = DomTestKit.html(s"#${HtmlIds.ProxyDetailSearchInput}").asInstanceOf[dom.html.Input] // scalafix:ok DisableSyntax.asInstanceOf

  private def otherInput: dom.html.Input = DomTestKit.html("#other-input").asInstanceOf[dom.html.Input] // scalafix:ok DisableSyntax.asInstanceOf

  private def setUpAtFirstMatch(): Unit = {
    DomTestKit.setUp(Page)
    AppState.proxyDetailSearch = "q"
    SearchNavigation.navigateSearchMatch(0)
  }

  def testPrev: Result = DomTestKit.withFakeTimers {
    setUpAtFirstMatch()
    val _ = DomTestKit.click(DomTestKit.element(".search-nav-prev"))
    Result.all(List(AppState.searchCurrentIdx ==== 2, DomTestKit.listenerErrors ==== Nil))
  }

  def testNext: Result = DomTestKit.withFakeTimers {
    setUpAtFirstMatch()
    val _ = DomTestKit.click(DomTestKit.element("#next-inner"))
    Result.all(List(AppState.searchCurrentIdx ==== 1, DomTestKit.listenerErrors ==== Nil))
  }

  def testNextNotClear: Result = DomTestKit.withFakeTimers {
    setUpAtFirstMatch()
    val _ = DomTestKit.click(DomTestKit.element("#nested-next"))
    Result.all(
      List(
        AppState.searchCurrentIdx ==== 1,
        AppState.proxyDetailSearch ==== "q",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testClearNoInput: Result = DomTestKit.withFakeTimers {
    DomTestKit.setUp("""<span class="msg-search-clear" id="clear">x</span>""")
    AppState.proxyDetailSearch = "q"
    val _ = DomTestKit.click(DomTestKit.element("#clear"))
    Result.all(
      List(
        AppState.proxyDetailSearch ==== "q",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testClear: Result = DomTestKit.withFakeTimers {
    setUpAtFirstMatch()
    val _ = DomTestKit.click(DomTestKit.element("#clear"))
    Result.all(
      List(
        AppState.proxyDetailSearch ==== "",
        Result.assert(dom.document.activeElement == input).log("the input has the focus"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testOutside: Result = DomTestKit.withFakeTimers {
    setUpAtFirstMatch()
    val _ = DomTestKit.click(DomTestKit.element("#other"))
    Result.all(
      List(
        AppState.searchCurrentIdx ==== 0,
        AppState.proxyDetailSearch ==== "q",
        DomTestKit.pendingTimers ==== 0,
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testInput: Result = DomTestKit.withFakeTimers {
    DomTestKit.setUp(Page)
    val _ = DomTestKit.typeInto(input, "abc")
    Result.all(
      List(AppState.proxyDetailSearch ==== "abc", DomTestKit.pendingTimers ==== 1, DomTestKit.listenerErrors ==== Nil)
    )
  }

  def testOtherInput: Result = DomTestKit.withFakeTimers {
    DomTestKit.setUp(Page)
    val _ = DomTestKit.typeInto(otherInput, "abc")
    Result.all(List(AppState.proxyDetailSearch ==== "", DomTestKit.pendingTimers ==== 0, DomTestKit.listenerErrors ==== Nil))
  }

  def testCompositionStart: Result = {
    DomTestKit.setUp(Page)
    val _ = DomTestKit.compositionStart(input)
    Result.all(List(AppState.imeComposing ==== true, DomTestKit.listenerErrors ==== Nil))
  }

  def testCompositionStartOther: Result = {
    DomTestKit.setUp(Page)
    val _ = DomTestKit.compositionStart(otherInput)
    Result.all(List(AppState.imeComposing ==== false, DomTestKit.listenerErrors ==== Nil))
  }

  def testCompositionEnd: Result = DomTestKit.withFakeTimers {
    DomTestKit.setUp(Page)
    AppState.imeComposing = true
    input.value = "한글"
    val _ = DomTestKit.compositionEnd(input)
    Result.all(
      List(
        AppState.imeComposing ==== false,
        AppState.proxyDetailSearch ==== "한글",
        DomTestKit.pendingTimers ==== 1,
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testCompositionEndOther: Result = DomTestKit.withFakeTimers {
    DomTestKit.setUp(Page)
    AppState.imeComposing = true
    val _ = DomTestKit.compositionEnd(otherInput)
    Result.all(
      List(
        AppState.imeComposing ==== true,
        AppState.proxyDetailSearch ==== "",
        DomTestKit.pendingTimers ==== 0,
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }
}
