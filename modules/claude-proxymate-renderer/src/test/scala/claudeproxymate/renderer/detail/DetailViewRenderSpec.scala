package claudeproxymate.renderer.detail

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.analysis.AnalysisRenderer
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*
import org.scalajs.dom

import scala.scalajs.js

/** `DetailView.renderProxyDetail` and `AnalysisRenderer.renderProxyAnalysis`
  * in jsdom: one test for each tab and each early exit.
  */
object DetailViewRenderSpec extends Properties {

  override def tests: List[Test] = List(
    example("renderProxyDetail with no detail view changes nothing", testNoView),
    example("renderProxyDetail with no selected capture shows the empty view", testNoCapture),
    example("renderProxyDetail on the messages tab shows only the messages", testMessagesTab),
    example("renderProxyDetail on the analysis tab shows only the analysis", testAnalysisTab),
    example("renderProxyDetail on the request tab with no body shows the no-body text", testRequestNoBody),
    example("renderProxyDetail on the response tab with no response shows the waiting text", testResponseWaiting),
    example("renderProxyDetail on the request tab shows the JSON tree", testRequestTree),
    example("renderProxyDetail on the request tab with a query marks the matches", testRequestQuery),
    example("renderProxyDetail on the response tab shows the response JSON tree", testResponseTree),
    example("renderProxyAnalysis shows the anatomy cards for a body", testAnatomyCards),
    example("renderProxyAnalysis shows no anatomy cards when the body is null", testAnatomyNullBody),
  )

  private val Page: String =
    s"""<div id="${HtmlIds.ProxyDetailView}"><i id="marker"></i></div><span id="${HtmlIds.SearchCounter}">-</span>"""

  private def requestBody: js.Dynamic =
    js.JSON
      .parse(
        """{"model":"claude-x","messages":[{"role":"user","content":"hello there"},{"role":"assistant","content":[{"type":"text","text":"general kenobi"}]}]}"""
      )

  private def responseBody: js.Dynamic =
    js.JSON
      .parse(
        """{"role":"assistant","content":[{"type":"text","text":"from the response"}],"usage":{"input_tokens":3,"output_tokens":4}}"""
      )

  private def setUpTab(tab: String, entry: js.Dynamic): Unit = {
    DomTestKit.setUp(Page)
    DomTestKit.select(entry)
    AppState.proxyDetailTab = tab
  }

  private def view: dom.html.Element = DomTestKit.html(s"#${HtmlIds.ProxyDetailView}")

  private def has(selector: String): Boolean = DomTestKit.elementOption(selector).isDefined

  def testNoView: Result = {
    DomTestKit.setUp("""<i id="marker"></i>""")
    DomTestKit.select(DomTestKit.capture(1, requestBody, responseBody))
    DetailView.renderProxyDetail()
    Result.assert(has("#marker")).log("nothing changed")
  }

  def testNoCapture: Result = {
    DomTestKit.setUp(Page)
    DetailView.renderProxyDetail()
    Result.all(
      List(
        Result.assert(!has("#marker")).log("rendered"),
        Result.assert(view.textContent.contains("proxy.selectRequestTitle")).log(view.textContent),
      )
    )
  }

  def testMessagesTab: Result = {
    setUpTab("messages", DomTestKit.capture(1, requestBody, responseBody))
    DetailView.renderProxyDetail()
    Result.all(
      List(
        Result.assert(view.textContent.contains("general kenobi")).log(view.textContent),
        Result.assert(has(s"#${HtmlIds.MsgSearchInput}")).log("the messages search input"),
        Result.assert(!has(s"#${HtmlIds.ProxyDetailCode}")).log("no JSON tree"),
        Result.assert(!has(".analysis-view")).log("no analysis"),
      )
    )
  }

  def testAnalysisTab: Result = {
    setUpTab("analysis", DomTestKit.capture(1, requestBody, responseBody))
    DetailView.renderProxyDetail()
    Result.all(
      List(
        Result.assert(has(".analysis-view")).log("the analysis"),
        Result.assert(!has(s"#${HtmlIds.ProxyDetailCode}")).log("no JSON tree"),
        Result.assert(!has(s"#${HtmlIds.MsgSearchInput}")).log("no messages search input"),
      )
    )
  }

  def testRequestNoBody: Result = {
    setUpTab("request", js.Dynamic.literal(id = 1, method = "POST", url = "/v1/messages"))
    DetailView.renderProxyDetail()
    Result.all(
      List(
        Result.assert(view.textContent.contains("proxy.noBody")).log(view.textContent),
        Result.assert(!has(s"#${HtmlIds.ProxyDetailCode}")).log("no JSON tree"),
      )
    )
  }

  def testResponseWaiting: Result = {
    setUpTab("response", js.Dynamic.literal(id = 1, method = "POST", url = "/v1/messages", body = requestBody))
    DetailView.renderProxyDetail()
    Result.all(
      List(
        Result.assert(view.textContent.contains("proxy.waitingResponse")).log(view.textContent),
        Result.assert(!has(s"#${HtmlIds.ProxyDetailCode}")).log("no JSON tree"),
      )
    )
  }

  def testRequestTree: Result = {
    setUpTab("request", DomTestKit.capture(1, requestBody, responseBody))
    DetailView.renderProxyDetail()
    val code = DomTestKit.html(s"#${HtmlIds.ProxyDetailCode}")
    Result.all(
      List(
        Result.assert(code.classList.contains("jt-lined")).log("the JSON tree"),
        Result.assert(code.textContent.contains("hello there")).log(code.textContent),
        Result.assert(!code.textContent.contains("from the response")).log(code.textContent),
      )
    )
  }

  def testRequestQuery: Result = {
    setUpTab("request", DomTestKit.capture(1, requestBody, responseBody))
    AppState.proxyDetailSearch = "kenobi"
    DetailView.renderProxyDetail()
    Result.all(
      List(
        DomTestKit.element(s"#${HtmlIds.ProxyDetailCode}").querySelectorAll("mark.search-hl.current").length ==== 1,
        AppState.searchCurrentIdx ==== 0,
        DomTestKit.element(s"#${HtmlIds.SearchCounter}").textContent ==== "1/1",
      )
    )
  }

  def testResponseTree: Result = {
    setUpTab("response", DomTestKit.capture(1, requestBody, responseBody))
    DetailView.renderProxyDetail()
    val code = DomTestKit.html(s"#${HtmlIds.ProxyDetailCode}")
    Result.all(
      List(
        Result.assert(code.textContent.contains("from the response")).log(code.textContent),
        Result.assert(!code.textContent.contains("hello there")).log(code.textContent),
      )
    )
  }

  def testAnatomyCards: Result = {
    DomTestKit.setUp("""<div id="analysis"></div>""")
    AnalysisRenderer.renderProxyAnalysis(DomTestKit.capture(1, requestBody, responseBody), DomTestKit.html("#analysis"))
    Result.assert(has("#analysis .analysis-view .anatomy-row")).log("anatomy cards")
  }

  def testAnatomyNullBody: Result = {
    DomTestKit.setUp("""<div id="analysis"></div>""")
    AnalysisRenderer.renderProxyAnalysis(
      js.Dynamic.literal(id = 1, method = "POST", url = "/v1/messages", body = null), // scalafix:ok DisableSyntax.null
      DomTestKit.html("#analysis"),
    )
    Result.all(
      List(
        Result.assert(has("#analysis .analysis-view")).log("the analysis view"),
        Result.assert(!has("#analysis .anatomy-row")).log("no anatomy cards"),
      )
    )
  }
}
