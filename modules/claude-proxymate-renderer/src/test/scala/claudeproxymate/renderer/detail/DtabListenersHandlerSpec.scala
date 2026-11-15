package claudeproxymate.renderer.detail

import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*

/** The click handler of [[DtabListeners]], in jsdom. */
object DtabListenersHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("a click on a detail tab button shows that tab", testTabClick),
    example("a click inside a detail tab button shows that tab", testClickInsideTab),
    example("a detail tab button with an empty data-dtab changes nothing", testEmptyTab),
    example("a click outside the detail tab buttons changes nothing", testOutside),
  )

  private val Tabs: String =
    """<button class="dtab active" data-dtab="messages">M</button><button class="dtab" data-dtab="request"><span id="inner">R</span></button><button class="dtab" data-dtab="">E</button><i id="other"></i>"""

  def testTabClick: Result = {
    DomTestKit.setUp(Tabs)
    val _ = DomTestKit.click(DomTestKit.element("""[data-dtab="request"]"""))
    Result.all(
      List(
        AppState.proxyDetailTab ==== "request",
        Result
          .assert(DomTestKit.element("""[data-dtab="request"]""").classList.contains("active"))
          .log("the request tab is active"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testClickInsideTab: Result = {
    DomTestKit.setUp(Tabs)
    val _ = DomTestKit.click(DomTestKit.element("#inner"))
    Result.all(List(AppState.proxyDetailTab ==== "request", DomTestKit.listenerErrors ==== Nil))
  }

  def testEmptyTab: Result = {
    DomTestKit.setUp(Tabs)
    AppState.proxyDetailSearch = "q"
    val _ = DomTestKit.click(DomTestKit.element("""[data-dtab=""]"""))
    Result.all(
      List(
        AppState.proxyDetailTab ==== "messages",
        AppState.proxyDetailSearch ==== "q",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testOutside: Result = {
    DomTestKit.setUp(Tabs)
    AppState.proxyDetailSearch = "q"
    val _ = DomTestKit.click(DomTestKit.element("#other"))
    Result.all(
      List(
        AppState.proxyDetailTab ==== "messages",
        AppState.proxyDetailSearch ==== "q",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }
}
