package claudeproxymate.renderer.analysis

import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*

import scala.scalajs.js

/** The click handler of [[MechChips]] and `buildMechFilterChips`, in jsdom. */
object MechChipsHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("a click on a chip sets the mechanism filter", testChipOn),
    example("a second click on the same chip clears the mechanism filter", testChipOff),
    example("a click inside a chip sets the mechanism filter", testInsideChip),
    example("a chip with an empty key changes nothing", testEmptyKey),
    example("a click outside the chips changes nothing", testOutside),
    example("buildMechFilterChips gives an empty string when there are no mechanisms", testNoChips),
    example("buildMechFilterChips gives a chip for an MCP tool", testMcpChip),
  )

  private val Page: String =
    s"""<button class="${MechChipsView.ChipClass}" ${MechChipsView.ChipDataAttr}="mc_0"><span id="inner">c</span></button><button class="${MechChipsView.ChipClass}" ${MechChipsView.ChipDataAttr}="">e</button><i id="other"></i>"""

  private def mcpBody: js.Dynamic =
    js.JSON.parse(
      """{"messages":[{"role":"assistant","content":[{"type":"tool_use","id":"tu_3","name":"mcp__serena__find_symbol","input":{"name":"Foo"}}]}]}"""
    )

  def testChipOn: Result = {
    DomTestKit.setUp(Page)
    val _ = DomTestKit.click(DomTestKit.element(s"""[${MechChipsView.ChipDataAttr}="mc_0"]"""))
    Result.all(List(AppState.proxyDetailMechFilter ==== Some("mc_0"), DomTestKit.listenerErrors ==== Nil))
  }

  def testChipOff: Result = {
    DomTestKit.setUp(Page)
    val _ = DomTestKit.click(DomTestKit.element(s"""[${MechChipsView.ChipDataAttr}="mc_0"]"""))
    val _ = DomTestKit.click(DomTestKit.element(s"""[${MechChipsView.ChipDataAttr}="mc_0"]"""))
    Result.all(List(AppState.proxyDetailMechFilter ==== None, DomTestKit.listenerErrors ==== Nil))
  }

  def testInsideChip: Result = {
    DomTestKit.setUp(Page)
    val _ = DomTestKit.click(DomTestKit.element("#inner"))
    Result.all(List(AppState.proxyDetailMechFilter ==== Some("mc_0"), DomTestKit.listenerErrors ==== Nil))
  }

  def testEmptyKey: Result = {
    DomTestKit.setUp(Page)
    AppState.proxyDetailMechFilter = Some("cm")
    val _ = DomTestKit.click(DomTestKit.element(s"""[${MechChipsView.ChipDataAttr}=""]"""))
    Result.all(List(AppState.proxyDetailMechFilter ==== Some("cm"), DomTestKit.listenerErrors ==== Nil))
  }

  def testOutside: Result = {
    DomTestKit.setUp(Page)
    AppState.proxyDetailMechFilter = Some("cm")
    val _ = DomTestKit.click(DomTestKit.element("#other"))
    Result.all(List(AppState.proxyDetailMechFilter ==== Some("cm"), DomTestKit.listenerErrors ==== Nil))
  }

  def testNoChips: Result = {
    DomTestKit.setUp("")
    MechChips.buildMechFilterChips(js.JSON.parse("""{"messages":[]}""")) ==== ""
  }

  def testMcpChip: Result = {
    DomTestKit.setUp("")
    val html = MechChips.buildMechFilterChips(mcpBody)
    Result.all(
      List(
        Result.assert(html.contains(s"""${MechChipsView.ChipDataAttr}="mc_0"""")).log(html),
        Result.assert(html.contains("find_symbol")).log(html),
      )
    )
  }
}
