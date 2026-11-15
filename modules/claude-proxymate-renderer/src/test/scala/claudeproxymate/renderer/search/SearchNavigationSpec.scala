package claudeproxymate.renderer.search

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*
import org.scalajs.dom


/** [[SearchNavigation]] in jsdom. */
object SearchNavigationSpec extends Properties {

  override def tests: List[Test] = List(
    example("setProxyDetailSearch during IME composition only keeps the query", testSearchComposing),
    example("setProxyDetailSearch starts the debounced search", testSearchDebounced),
    example("an empty search also clears the mechanism filter", testEmptySearch),
    example("navigateSearchMatch on the messages tab with no detail view changes nothing", testNavNoView),
    example("navigateSearchMatch with no marks changes nothing", testNavNoMarks),
    example("navigateSearchMatch(0) goes to the first mark", testNavFirst),
    example("navigateSearchMatch(1) goes to the next mark", testNavNext),
    example("navigateSearchMatch(-1) from the first mark goes to the last mark", testNavWrap),
    example("navigateSearchMatch on the request tab uses the detail code", testNavDetailCode),
    example("navigateSearchMatch with no detail code uses the analysis view", testNavAnalysis),
  )

  private def marks(n: Int): String =
    (1 to n).map(i => s"""<mark class="search-hl" id="m$i">x</mark>""").mkString

  private def messagesPage(n: Int): String =
    s"""<span id="${HtmlIds.SearchCounter}">-</span><div id="${HtmlIds.ProxyDetailView}">${marks(n)}</div>"""

  private def counter: String = DomTestKit.element(s"#${HtmlIds.SearchCounter}").textContent

  private def currentIds: List[String] = {
    val nl = dom.document.querySelectorAll("mark.search-hl.current")
    (0 until nl.length).toList.map(i => nl(i).asInstanceOf[dom.Element].id) // scalafix:ok DisableSyntax.asInstanceOf
  }

  def testSearchComposing: Result = DomTestKit.withFakeTimers {
    DomTestKit.setUp("")
    AppState.imeComposing = true
    SearchNavigation.setProxyDetailSearch("ab")
    Result.all(List(AppState.proxyDetailSearch ==== "ab", DomTestKit.pendingTimers ==== 0))
  }

  def testSearchDebounced: Result = DomTestKit.withFakeTimers {
    DomTestKit.setUp("")
    AppState.searchCurrentIdx = 3
    AppState.proxyDetailMechFilter = Some("cm")
    SearchNavigation.setProxyDetailSearch("ab")
    val pending = DomTestKit.pendingTimers
    val ran     = DomTestKit.runTimers()
    Result.all(
      List(
        AppState.proxyDetailSearch ==== "ab",
        pending ==== 1,
        ran ==== 1,
        AppState.searchCurrentIdx ==== -1,
        AppState.proxyDetailMechFilter ==== Some("cm"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testEmptySearch: Result = DomTestKit.withFakeTimers {
    DomTestKit.setUp("")
    AppState.proxyDetailMechFilter = Some("cm")
    SearchNavigation.setProxyDetailSearch("")
    val _ = DomTestKit.runTimers()
    Result.all(List(AppState.proxyDetailSearch ==== "", AppState.proxyDetailMechFilter ==== None))
  }

  def testNavNoView: Result = {
    DomTestKit.setUp(s"""<span id="${HtmlIds.SearchCounter}">-</span>${marks(2)}""")
    SearchNavigation.navigateSearchMatch(0)
    Result.all(List(counter ==== "-", AppState.searchCurrentIdx ==== -1, currentIds ==== Nil))
  }

  def testNavNoMarks: Result = {
    DomTestKit.setUp(messagesPage(0))
    SearchNavigation.navigateSearchMatch(0)
    Result.all(List(counter ==== "-", AppState.searchCurrentIdx ==== -1))
  }

  def testNavFirst: Result = {
    DomTestKit.setUp(messagesPage(3))
    SearchNavigation.navigateSearchMatch(0)
    Result.all(List(counter ==== "1/3", AppState.searchCurrentIdx ==== 0, currentIds ==== List("m1")))
  }

  def testNavNext: Result = {
    DomTestKit.setUp(messagesPage(3))
    SearchNavigation.navigateSearchMatch(0)
    SearchNavigation.navigateSearchMatch(1)
    Result.all(List(counter ==== "2/3", AppState.searchCurrentIdx ==== 1, currentIds ==== List("m2")))
  }

  def testNavWrap: Result = {
    DomTestKit.setUp(messagesPage(3))
    SearchNavigation.navigateSearchMatch(0)
    SearchNavigation.navigateSearchMatch(-1)
    Result.all(List(counter ==== "3/3", AppState.searchCurrentIdx ==== 2, currentIds ==== List("m3")))
  }

  def testNavDetailCode: Result = {
    DomTestKit.setUp(
      s"""<span id="${HtmlIds.SearchCounter}">-</span><div id="${HtmlIds.ProxyDetailView}">${marks(1)}<div id="${HtmlIds.ProxyDetailCode}"><mark class="search-hl" id="c1">x</mark><mark class="search-hl" id="c2">x</mark></div></div>"""
    )
    AppState.proxyDetailTab = "request"
    SearchNavigation.navigateSearchMatch(0)
    Result.all(List(counter ==== "1/2", currentIds ==== List("c1")))
  }

  def testNavAnalysis: Result = {
    DomTestKit.setUp(
      s"""<span id="${HtmlIds.SearchCounter}">-</span>${marks(1)}<div class="analysis-view"><mark class="search-hl" id="a1">x</mark></div>"""
    )
    AppState.proxyDetailTab = "analysis"
    SearchNavigation.navigateSearchMatch(0)
    Result.all(List(counter ==== "1/1", currentIds ==== List("a1")))
  }

}
