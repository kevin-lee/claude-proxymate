package claudeproxymate.renderer.messages

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*
import org.scalajs.dom

import scala.scalajs.js

/** The handlers of [[MessageRenderer]] and the methods with an early exit,
  * in jsdom. A re-render of the messages replaces the content of
  * `#proxyDetailView`, so `#marker` is gone after it.
  */
object MessageRendererHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    /* handleClick */
    example("a messages-tab token click toggles it and re-renders the messages", testMessageToken),
    example("a JSON-tree token click is left to JsonTreeViewer", testJsonToken),
    example("a messages-tab correlation-id click toggles it and re-renders the messages", testMessageCorr),
    example("a filter button click sets the filter and renders the messages", testFilterButton),
    example("a filter button with an empty key goes on to the search clear button", testFilterEmptyKey),
    example("the search clear button clears the messages search and focuses the input", testSearchClear),
    example("a badge inside the search clear button is not toggled", testBadgeInsideClear),
    example("a badge click opens the badge section", testBadgeClick),
    example("a badge with an empty uid changes nothing", testBadgeEmptyUid),
    example("a click elsewhere changes nothing", testOutside),
    /* rerenderPreservingScroll */
    example("rerenderPreservingScroll on another tab changes nothing", testRerenderOtherTab),
    example("rerenderPreservingScroll with no detail view changes nothing", testRerenderNoView),
    example("rerenderPreservingScroll with no selected capture changes nothing", testRerenderNoCapture),
    example("rerenderPreservingScroll re-renders and re-opens the open badge", testRerenderReopen),
    /* input and composition */
    example("input in the messages search input sets the query and starts the search", testInput),
    example("input in another input changes nothing", testOtherInput),
    example("compositionstart in the messages search input sets the IME flag", testCompositionStart),
    example("compositionstart in another element changes nothing", testCompositionStartOther),
    example("compositionend in the messages search input clears the IME flag and sets the query", testCompositionEnd),
    example("compositionend in another element changes nothing", testCompositionEndOther),
    /* setMsgSearch */
    example("setMsgSearch during IME composition only keeps the query", testSetMsgSearchComposing),
    example("setMsgSearch renders the messages after the debounce", testSetMsgSearchDebounced),
    /* buildVisibleCards, renderProxyMessages and the query match */
    example("buildVisibleCards with no messages gives no cards", testCardsNoMessages),
    example("buildVisibleCards with an empty query gives every card", testCardsEmptyQuery),
    example("buildVisibleCards keeps only the cards that match the query", testCardsQuery),
    example("buildVisibleCards matches the query in a badge label", testCardsQueryBadge),
    example("renderProxyMessages with no messages shows the empty view", testRenderNoMessages),
    example("renderProxyMessages shows the count of cards", testRenderCount),
    example("renderProxyMessages with no match shows the no-results view", testRenderNoResults),
  )

  private def body(): js.Dynamic =
    js.JSON
      .parse(
        """{"messages":[{"role":"user","content":"hello there <system-reminder>remember this</system-reminder>"},{"role":"assistant","content":[{"type":"text","text":"general kenobi"}]}]}"""
      )

  private val DetailHtml: String =
    s"""<div id="${HtmlIds.ProxyDetailView}"><i id="marker"></i></div>"""

  private def setUpMessages(extraHtml: String): Unit = {
    DomTestKit.setUp(DetailHtml + extraHtml)
    DomTestKit.select(DomTestKit.capture(1, body(), js.Dynamic.literal()))
    AppState.proxyDetailTab = "messages"
  }

  private def entry: js.Dynamic = AppState.proxyCaptures.head

  private def detailView: dom.html.Element = DomTestKit.html(s"#${HtmlIds.ProxyDetailView}")

  private def reRendered: Boolean = DomTestKit.elementOption("#marker").isEmpty

  private def noErrors: Result = DomTestKit.listenerErrors ==== Nil

  private def clickOn(selector: String): Unit = {
    val _ = DomTestKit.click(DomTestKit.element(selector))
  }

  private def overrides: Set[String] = AppState.maskOverrides.toSet

  private def input(id: String): dom.html.Input =
    DomTestKit.html(s"#$id").asInstanceOf[dom.html.Input] // scalafix:ok DisableSyntax.asInstanceOf

  // ── handleClick ─────────────────────────────────────────────────────────

  def testMessageToken: Result = {
    setUpMessages("""<span class="jt-token-mask" data-token-id="m.0.0#4">t</span>""")
    clickOn("""[data-token-id="m.0.0#4"]""")
    Result.all(
      List(
        overrides ==== Set("m.0.0#4"),
        Result.assert(reRendered).log("the messages are re-rendered"),
        DomTestKit.runAnimationFrames() ==== 1,
        noErrors,
      )
    )
  }

  def testJsonToken: Result = {
    /* JsonTreeViewer toggles the id once. If MessageRenderer also toggled
     * it, the two toggles would cancel out. */
    setUpMessages("""<span class="jt-token-mask" data-token-id="$.x#0">t</span>""")
    clickOn("""[data-token-id="$.x#0"]""")
    Result.all(List(overrides ==== Set("$.x#0"), noErrors))
  }

  def testMessageCorr: Result = {
    setUpMessages("""<span class="jt-corr-mask" data-corr-id="corr:m.0.0#0">c</span>""")
    clickOn("""[data-corr-id="corr:m.0.0#0"]""")
    Result.all(List(overrides ==== Set("corr:m.0.0#0"), Result.assert(reRendered).log("re-rendered"), noErrors))
  }

  def testFilterButton: Result = {
    setUpMessages(
      s"""<button class="${MessageView.FilterButtonClass}" ${MessageView.FilterDataAttr}="user">u</button>"""
    )
    clickOn(s".${MessageView.FilterButtonClass}")
    Result.all(
      List(
        AppState.msgFilter ==== "user",
        Result.assert(reRendered).log("the messages are rendered"),
        Result.assert(detailView.textContent.contains("hello there")).log(detailView.textContent),
        Result.assert(!detailView.textContent.contains("general kenobi")).log(detailView.textContent),
        noErrors,
      )
    )
  }

  def testFilterEmptyKey: Result = DomTestKit.withFakeTimers {
    setUpMessages(
      s"""<span class="${MessageView.SearchClearClass}"><button class="${MessageView.FilterButtonClass}" ${MessageView.FilterDataAttr}="">u</button></span>"""
    )
    AppState.msgSearchQuery = "q"
    clickOn(s".${MessageView.FilterButtonClass}")
    Result.all(List(AppState.msgFilter ==== "all", AppState.msgSearchQuery ==== "", noErrors))
  }

  def testSearchClear: Result = DomTestKit.withFakeTimers {
    setUpMessages(
      s"""<span class="${MessageView.SearchClearClass}" id="clear">x</span><input id="${HtmlIds.MsgSearchInput}">"""
    )
    AppState.msgSearchQuery = "q"
    clickOn("#clear")
    Result.all(
      List(
        AppState.msgSearchQuery ==== "",
        Result.assert(dom.document.activeElement == input(HtmlIds.MsgSearchInput)).log("the input has the focus"),
        noErrors,
      )
    )
  }

  def testBadgeInsideClear: Result = DomTestKit.withFakeTimers {
    setUpMessages(
      s"""<span class="${MessageView.SearchClearClass}"><span class="${MessageView.BadgeClass}" ${MessageView.BadgeDataAttr}="u1" id="bb_u1">b</span></span><div id="bc_u1" style="display:none"></div>"""
    )
    AppState.msgSearchQuery = "q"
    clickOn("#bb_u1")
    Result.all(
      List(
        AppState.msgSearchQuery ==== "",
        DomTestKit.html("#bc_u1").style.display ==== "none",
        AppState.activeBadgeUid ==== None,
        noErrors,
      )
    )
  }

  def testBadgeClick: Result = {
    setUpMessages(
      s"""<span class="${MessageView.BadgeClass}" ${MessageView.BadgeDataAttr}="u1" id="bb_u1">b</span><div id="bc_u1" style="display:none"></div>"""
    )
    clickOn("#bb_u1")
    Result.all(
      List(
        DomTestKit.html("#bc_u1").style.display ==== "block",
        AppState.activeBadgeUid ==== Some("u1"),
        noErrors,
      )
    )
  }

  def testBadgeEmptyUid: Result = {
    setUpMessages(
      s"""<span class="${MessageView.BadgeClass}" ${MessageView.BadgeDataAttr}="" id="bb_">b</span><div id="bc_" style="display:none"></div>"""
    )
    clickOn("#bb_")
    Result.all(List(DomTestKit.html("#bc_").style.display ==== "none", AppState.activeBadgeUid ==== None, noErrors))
  }

  def testOutside: Result = {
    setUpMessages("""<i id="other"></i>""")
    clickOn("#other")
    Result.all(
      List(
        Result.assert(!reRendered).log("not re-rendered"),
        overrides ==== Set.empty[String],
        AppState.msgFilter ==== "all",
        noErrors,
      )
    )
  }

  // ── rerenderPreservingScroll ────────────────────────────────────────────

  def testRerenderOtherTab: Result = {
    setUpMessages("")
    AppState.proxyDetailTab = "request"
    MessageRenderer.rerenderPreservingScroll()
    Result.all(List(Result.assert(!reRendered).log("not re-rendered"), DomTestKit.runAnimationFrames() ==== 0))
  }

  def testRerenderNoView: Result = {
    DomTestKit.setUp("""<i id="marker"></i>""")
    DomTestKit.select(DomTestKit.capture(1, body(), js.Dynamic.literal()))
    AppState.proxyDetailTab = "messages"
    MessageRenderer.rerenderPreservingScroll()
    Result.all(List(Result.assert(!reRendered).log("not re-rendered"), DomTestKit.runAnimationFrames() ==== 0))
  }

  def testRerenderNoCapture: Result = {
    setUpMessages("")
    AppState.selectedProxyId = None
    MessageRenderer.rerenderPreservingScroll()
    Result.all(List(Result.assert(!reRendered).log("not re-rendered"), DomTestKit.runAnimationFrames() ==== 0))
  }

  def testRerenderReopen: Result = {
    setUpMessages("")
    MessageRenderer.renderProxyMessages(entry, detailView)
    val part  = DomTestKit.element(s"[${MessageView.BadgePartAttr}]").getAttribute(MessageView.BadgePartAttr)
    AppState.activeBadgePart = Some(part)
    MessageRenderer.rerenderPreservingScroll()
    val badge = DomTestKit.element(s"[${MessageView.BadgePartAttr}]")
    val uid   = badge.getAttribute(MessageView.BadgeDataAttr)
    Result.all(
      List(
        DomTestKit.html(s"#bc_$uid").style.display ==== "block",
        AppState.activeBadgeUid ==== Some(uid),
        AppState.activeBadgePart ==== Some(part),
        DomTestKit.runAnimationFrames() ==== 1,
      )
    )
  }

  // ── input and composition ───────────────────────────────────────────────

  private val Inputs: String = s"""<input id="${HtmlIds.MsgSearchInput}"><input id="other-input">"""

  def testInput: Result = DomTestKit.withFakeTimers {
    setUpMessages(Inputs)
    val _ = DomTestKit.typeInto(input(HtmlIds.MsgSearchInput), "hello")
    Result.all(List(AppState.msgSearchQuery ==== "hello", DomTestKit.pendingTimers ==== 1, noErrors))
  }

  def testOtherInput: Result = DomTestKit.withFakeTimers {
    setUpMessages(Inputs)
    val _ = DomTestKit.typeInto(input("other-input"), "hello")
    Result.all(List(AppState.msgSearchQuery ==== "", DomTestKit.pendingTimers ==== 0, noErrors))
  }

  def testCompositionStart: Result = {
    setUpMessages(Inputs)
    val _ = DomTestKit.compositionStart(input(HtmlIds.MsgSearchInput))
    Result.all(List(AppState.imeComposing ==== true, noErrors))
  }

  def testCompositionStartOther: Result = {
    setUpMessages(Inputs)
    val _ = DomTestKit.compositionStart(input("other-input"))
    Result.all(List(AppState.imeComposing ==== false, noErrors))
  }

  def testCompositionEnd: Result = DomTestKit.withFakeTimers {
    setUpMessages(Inputs)
    AppState.imeComposing = true
    input(HtmlIds.MsgSearchInput).value = "안녕"
    val _ = DomTestKit.compositionEnd(input(HtmlIds.MsgSearchInput))
    Result.all(
      List(
        AppState.imeComposing ==== false,
        AppState.msgSearchQuery ==== "안녕",
        DomTestKit.pendingTimers ==== 1,
        noErrors,
      )
    )
  }

  def testCompositionEndOther: Result = DomTestKit.withFakeTimers {
    setUpMessages(Inputs)
    AppState.imeComposing = true
    val _ = DomTestKit.compositionEnd(input("other-input"))
    Result.all(List(AppState.imeComposing ==== true, AppState.msgSearchQuery ==== "", DomTestKit.pendingTimers ==== 0))
  }

  // ── setMsgSearch ────────────────────────────────────────────────────────

  def testSetMsgSearchComposing: Result = DomTestKit.withFakeTimers {
    setUpMessages("")
    AppState.imeComposing = true
    MessageRenderer.setMsgSearch("hel")
    Result.all(List(AppState.msgSearchQuery ==== "hel", DomTestKit.pendingTimers ==== 0))
  }

  def testSetMsgSearchDebounced: Result = DomTestKit.withFakeTimers {
    setUpMessages("")
    MessageRenderer.setMsgSearch("kenobi")
    val pending = DomTestKit.pendingTimers
    val before  = reRendered
    val _       = DomTestKit.runTimers()
    Result.all(
      List(
        pending ==== 1,
        Result.assert(!before).log("not rendered before the debounce"),
        Result.assert(reRendered).log("rendered after the debounce"),
        Result.assert(detailView.textContent.contains("general kenobi")).log(detailView.textContent),
        Result.assert(!detailView.textContent.contains("hello there")).log(detailView.textContent),
      )
    )
  }

  // ── buildVisibleCards, renderProxyMessages and the query match ──────────

  def testCardsNoMessages: Result = {
    DomTestKit.setUp("")
    MessageRenderer.buildVisibleCards(
      DomTestKit.capture(1, js.JSON.parse("""{"messages":[]}"""), js.Dynamic.literal())
    ) ==== Nil
  }

  def testCardsEmptyQuery: Result = {
    DomTestKit.setUp("")
    MessageRenderer.buildVisibleCards(DomTestKit.capture(1, body(), js.Dynamic.literal())).map(_.role) ==== List(
      "user",
      "assistant"
    )
  }

  def testCardsQuery: Result = {
    DomTestKit.setUp("")
    AppState.msgSearchQuery = "KENOBI"
    MessageRenderer.buildVisibleCards(DomTestKit.capture(1, body(), js.Dynamic.literal())).map(_.role) ==== List(
      "assistant"
    )
  }

  def testCardsQueryBadge: Result = {
    DomTestKit.setUp("")
    AppState.msgSearchQuery = "remember this"
    MessageRenderer.buildVisibleCards(DomTestKit.capture(1, body(), js.Dynamic.literal())).map(_.role) ==== List("user")
  }

  def testRenderNoMessages: Result = {
    setUpMessages(s"""<span id="${HtmlIds.MsgCountEl}">-</span>""")
    MessageRenderer.renderProxyMessages(
      DomTestKit.capture(1, js.JSON.parse("""{"messages":[]}"""), js.Dynamic.literal()),
      detailView,
    )
    Result.all(
      List(
        Result.assert(detailView.textContent.contains("proxy.noMessages")).log(detailView.textContent),
        DomTestKit.elementOption(s"#${HtmlIds.MsgSearchInput}") ==== None,
      )
    )
  }

  def testRenderCount: Result = {
    setUpMessages("")
    MessageRenderer.renderProxyMessages(entry, detailView)
    Result.all(
      List(
        Result.assert(DomTestKit.elementOption(s"#${HtmlIds.MsgCountEl}").isDefined).log("count element"),
        Result.assert(DomTestKit.elementOption(s"#${HtmlIds.MsgSearchInput}").isDefined).log("search input"),
        Result.assert(detailView.textContent.contains("general kenobi")).log(detailView.textContent),
      )
    )
  }

  def testRenderNoResults: Result = {
    setUpMessages("")
    AppState.msgSearchQuery = "nothing matches this"
    MessageRenderer.renderProxyMessages(entry, detailView)
    Result.all(
      List(
        Result.assert(detailView.textContent.contains("proxy.noResults")).log(detailView.textContent),
        Result
          .assert(dom.document.activeElement == input(HtmlIds.MsgSearchInput))
          .log("the search input has the focus"),
      )
    )
  }
}
