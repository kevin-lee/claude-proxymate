package claudeproxymate.renderer.proxy

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*

import scala.scalajs.js

/** The click handler of [[ProxyList]] and `renderProxyList`, in jsdom. */
object ProxyListHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("Clear with no captures clears without a prompt", testClearEmpty),
    example("Clear with captures asks, and clears on yes", testClearYes),
    example("Clear with captures asks, and keeps them on no", testClearNo),
    example("a click on a capture entry selects it", testSelectEntry),
    example("a click inside a capture entry selects it", testSelectInsideEntry),
    example("a capture entry with a non-number id changes nothing", testBadId),
    example("a click outside the capture entries changes nothing", testOutside),
    example("renderProxyList with no list element changes nothing", testRenderNoList),
    example("renderProxyList shows the count and the entries", testRenderList),
  )

  private def captures: List[js.Dynamic] =
    List(DomTestKit.capture(1, js.Dynamic.literal(), js.Dynamic.literal()), DomTestKit.capture(2, js.Dynamic.literal(), js.Dynamic.literal()))

  private val Page: String =
    s"""<button id="${HtmlIds.ProxyClearBtn}">Clear</button><span id="${HtmlIds.ProxyCount}">-</span><div id="${HtmlIds.ProxyList}"><div class="${ProxyListView.EntryClass}" ${ProxyListView.EntryDataIdAttr}="2"><span id="inner">e</span></div><div class="${ProxyListView.EntryClass}" ${ProxyListView.EntryDataIdAttr}="x">bad</div></div><i id="other"></i>"""

  private def setUpWithCaptures(): Unit = {
    DomTestKit.setUp(Page)
    AppState.proxyCaptures = captures
    AppState.selectedProxyId = Some(1.0)
    val _ = AppState.maskOverrides.add("$.a")
  }

  def testClearEmpty: Result = {
    DomTestKit.setUp(Page)
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ProxyClearBtn}"))
    Result.all(
      List(
        DomTestKit.confirmMessages ==== Nil,
        AppState.proxyCaptures ==== Nil,
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testClearYes: Result = {
    setUpWithCaptures()
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ProxyClearBtn}"))
    Result.all(
      List(
        DomTestKit.confirmMessages.length ==== 1,
        AppState.proxyCaptures ==== Nil,
        AppState.selectedProxyId ==== None,
        AppState.maskOverrides.toSet ==== Set.empty[String],
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testClearNo: Result = {
    setUpWithCaptures()
    DomTestKit.answerConfirm(false)
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ProxyClearBtn}"))
    Result.all(
      List(
        DomTestKit.confirmMessages.length ==== 1,
        AppState.proxyCaptures.length ==== 2,
        AppState.selectedProxyId ==== Some(1.0),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testSelectEntry: Result = {
    setUpWithCaptures()
    val _ = DomTestKit.click(DomTestKit.element(s"""[${ProxyListView.EntryDataIdAttr}="2"]"""))
    Result.all(
      List(
        AppState.selectedProxyId ==== Some(2.0),
        AppState.maskOverrides.toSet ==== Set.empty[String],
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testSelectInsideEntry: Result = {
    setUpWithCaptures()
    val _ = DomTestKit.click(DomTestKit.element("#inner"))
    Result.all(List(AppState.selectedProxyId ==== Some(2.0), DomTestKit.listenerErrors ==== Nil))
  }

  def testBadId: Result = {
    setUpWithCaptures()
    val _ = DomTestKit.click(DomTestKit.element(s"""[${ProxyListView.EntryDataIdAttr}="x"]"""))
    Result.all(
      List(
        AppState.selectedProxyId ==== Some(1.0),
        AppState.maskOverrides.toSet ==== Set("$.a"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testOutside: Result = {
    setUpWithCaptures()
    val _ = DomTestKit.click(DomTestKit.element("#other"))
    Result.all(
      List(
        AppState.selectedProxyId ==== Some(1.0),
        AppState.proxyCaptures.length ==== 2,
        DomTestKit.confirmMessages ==== Nil,
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testRenderNoList: Result = {
    DomTestKit.setUp(s"""<span id="${HtmlIds.ProxyCount}">-</span>""")
    AppState.proxyCaptures = captures
    ProxyList.renderProxyList()
    DomTestKit.element(s"#${HtmlIds.ProxyCount}").textContent ==== "-"
  }

  def testRenderList: Result = {
    DomTestKit.setUp(Page)
    AppState.proxyCaptures = captures
    ProxyList.renderProxyList()
    Result.all(
      List(
        DomTestKit.element(s"#${HtmlIds.ProxyCount}").textContent ==== "2",
        entryCount ==== 2,
      )
    )
  }

  private def entryCount: Int =
    DomTestKit.element(s"#${HtmlIds.ProxyList}").querySelectorAll(s".${ProxyListView.EntryClass}").length
}
