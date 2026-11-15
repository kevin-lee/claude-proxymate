package claudeproxymate.renderer.filter

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.messages.MessageView
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*
import org.scalajs.dom

/** The mouseup handler of [[MessageFilterMenu]] (text selection) and the
  * selection menu kind buttons, in jsdom.
  */
object MessageFilterMenuHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("a text selection in one typed block opens the selection menu", testSelectionOpensMenu),
    example("mouseup in the selection menu keeps the menu and the selection", testMouseUpInMenu),
    example("mouseup with no selection closes the selection menu", testNoSelection),
    example("a selection across two blocks closes the selection menu", testTwoBlocks),
    example("a selection outside the typed blocks closes the selection menu", testOutsideBlocks),
    example("a kind button in the selection menu changes the active kind", testKindButton),
    example("a kind button with no selection menu changes nothing", testKindNoMenu),
  )

  private val Page: String =
    s"""<div class="msg-body"><div class="${MessageView.TypedClass}" id="block1">first block text</div></div>
       |<div class="msg-body"><div class="${MessageView.TypedClass}" id="block2">second block text</div></div>
       |<p id="plain">plain text</p>""".stripMargin

  private def selectText(fromId: String, toId: String): Unit = {
    val sel   = dom.window.getSelection()
    val range = dom.document.createRange()
    range.setStart(DomTestKit.element(s"#$fromId").firstChild, 0)
    range.setEnd(DomTestKit.element(s"#$toId").firstChild, 5)
    sel.removeAllRanges()
    sel.addRange(range)
  }

  private def clearSelection(): Unit = dom.window.getSelection().removeAllRanges()

  private def menuOpen: Boolean = DomTestKit.elementOption(s"#${HtmlIds.SelectionMenu}").isDefined

  private def activeKind: Option[String] =
    DomTestKit
      .elementOption(s"#${HtmlIds.SelectionMenu} .seg-btn.active")
      .map(_.getAttribute(RequestFilterView.ValueAttr))

  private def openMenu(): Unit = {
    DomTestKit.setUp(Page)
    selectText("block1", "block1")
    val _ = DomTestKit.mouseUp(DomTestKit.element("#block1"))
  }

  def testSelectionOpensMenu: Result = {
    openMenu()
    val r = Result.all(
      List(
        Result.assert(menuOpen).log("the selection menu is open"),
        activeKind ==== Some("text"),
        Result
          .assert(DomTestKit.html(s"#${HtmlIds.SelectionMenu}").parentElement.classList.contains("msg-body"))
          .log("the menu is in the block's parent"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
    clearSelection()
    r
  }

  def testMouseUpInMenu: Result = {
    openMenu()
    clearSelection()
    val _ = DomTestKit.mouseUp(DomTestKit.element(s"#${HtmlIds.SelectionMenu} .sel-menu-title"))
    Result.all(List(Result.assert(menuOpen).log("the selection menu stays open"), DomTestKit.listenerErrors ==== Nil))
  }

  def testNoSelection: Result = {
    openMenu()
    clearSelection()
    val _ = DomTestKit.mouseUp(DomTestKit.element("#plain"))
    Result.all(List(Result.assert(!menuOpen).log("the selection menu is closed"), DomTestKit.listenerErrors ==== Nil))
  }

  def testTwoBlocks: Result = {
    openMenu()
    selectText("block1", "block2")
    val _ = DomTestKit.mouseUp(DomTestKit.element("#block2"))
    clearSelection()
    Result.all(List(Result.assert(!menuOpen).log("the selection menu is closed"), DomTestKit.listenerErrors ==== Nil))
  }

  def testOutsideBlocks: Result = {
    openMenu()
    selectText("plain", "plain")
    val _ = DomTestKit.mouseUp(DomTestKit.element("#plain"))
    clearSelection()
    Result.all(List(Result.assert(!menuOpen).log("the selection menu is closed"), DomTestKit.listenerErrors ==== Nil))
  }

  def testKindButton: Result = {
    openMenu()
    clearSelection()
    val _ = DomTestKit.click(DomTestKit.element(s"""#${HtmlIds.SelectionMenu} [${RequestFilterView.ValueAttr}="regex"]"""))
    Result.all(
      List(
        Result.assert(menuOpen).log("the selection menu stays open"),
        activeKind ==== Some("regex"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testKindNoMenu: Result = {
    DomTestKit.setUp(
      s"""<div class="${MessageFilterView.BarClass}"><button id="kind" ${MessageFilterView.ActionAttr}="${MessageFilterView.Action.SelKind}" ${RequestFilterView.ValueAttr}="regex">r</button></div>"""
    )
    val _ = DomTestKit.click(DomTestKit.element("#kind"))
    Result.all(List(Result.assert(!menuOpen).log("no selection menu"), DomTestKit.listenerErrors ==== Nil))
  }
}
