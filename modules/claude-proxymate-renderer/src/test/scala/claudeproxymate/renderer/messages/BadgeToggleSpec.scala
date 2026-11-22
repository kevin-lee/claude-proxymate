package claudeproxymate.renderer.messages

import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*

/** [[BadgeToggle]] in jsdom. */
object BadgeToggleSpec extends Properties {

  override def tests: List[Test] = List(
    example("toggleBadge with no content element changes nothing", testToggleNoContent),
    example("toggleBadge opens a closed badge", testToggleOpen),
    example("toggleBadge closes an open badge", testToggleClose),
    example("toggleBadge closes the badge that was open before", testToggleOther),
    example("toggleBadge with no button still opens the content", testToggleNoButton),
    example("reopen with no button for the part changes nothing", testReopenNoButton),
    example("reopen with no content for the button changes nothing", testReopenNoContent),
    example("reopen opens the badge of the part", testReopen),
  )

  private def badge(uid: String, part: String): String =
    s"""<span id="bb_$uid" ${MessageView.BadgeDataAttr}="$uid" ${MessageView.BadgePartAttr}="$part">b</span><div id="bc_$uid" style="display:none"></div>"""

  private def display(uid: String): String = DomTestKit.html(s"#bc_$uid").style.display

  private def hasClass(id: String, cls: String): Boolean = DomTestKit.element(s"#$id").classList.contains(cls)

  def testToggleNoContent: Result = {
    DomTestKit.setUp(s"""<span id="bb_u1" ${MessageView.BadgePartAttr}="0.0">b</span>""")
    BadgeToggle.toggleBadge("u1")
    Result.all(
      List(
        AppState.activeBadgeUid ==== None,
        AppState.activeBadgePart ==== None,
        Result.assert(!hasClass("bb_u1", "open")).log("not open"),
      )
    )
  }

  def testToggleOpen: Result = {
    DomTestKit.setUp(badge("u1", "0.0"))
    BadgeToggle.toggleBadge("u1")
    Result.all(
      List(
        display("u1") ==== "block",
        Result.assert(hasClass("bc_u1", "badge-section-hl")).log("highlighted"),
        Result.assert(hasClass("bb_u1", "open")).log("button open"),
        AppState.activeBadgeUid ==== Some("u1"),
        AppState.activeBadgePart ==== Some("0.0"),
      )
    )
  }

  def testToggleClose: Result = {
    DomTestKit.setUp(badge("u1", "0.0"))
    BadgeToggle.toggleBadge("u1")
    BadgeToggle.toggleBadge("u1")
    Result.all(
      List(
        display("u1") ==== "none",
        Result.assert(!hasClass("bb_u1", "open")).log("button closed"),
        AppState.activeBadgeUid ==== None,
        AppState.activeBadgePart ==== None,
      )
    )
  }

  def testToggleOther: Result = {
    DomTestKit.setUp(badge("u1", "0.0") + badge("u2", "0.1"))
    BadgeToggle.toggleBadge("u1")
    BadgeToggle.toggleBadge("u2")
    Result.all(
      List(
        display("u1") ==== "none",
        Result.assert(!hasClass("bb_u1", "open")).log("button 1 closed"),
        display("u2") ==== "block",
        AppState.activeBadgeUid ==== Some("u2"),
        AppState.activeBadgePart ==== Some("0.1"),
      )
    )
  }

  def testToggleNoButton: Result = {
    DomTestKit.setUp("""<div id="bc_u1" style="display:none"></div>""")
    BadgeToggle.toggleBadge("u1")
    Result.all(
      List(display("u1") ==== "block", AppState.activeBadgeUid ==== Some("u1"), AppState.activeBadgePart ==== None)
    )
  }

  def testReopenNoButton: Result = {
    DomTestKit.setUp(badge("u1", "0.0"))
    BadgeToggle.reopen("9.9")
    Result.all(List(display("u1") ==== "none", AppState.activeBadgeUid ==== None))
  }

  def testReopenNoContent: Result = {
    DomTestKit.setUp(
      s"""<span id="bb_u1" ${MessageView.BadgeDataAttr}="u1" ${MessageView.BadgePartAttr}="0.0">b</span>"""
    )
    BadgeToggle.reopen("0.0")
    Result.all(List(Result.assert(!hasClass("bb_u1", "open")).log("not open"), AppState.activeBadgeUid ==== None))
  }

  def testReopen: Result = {
    DomTestKit.setUp(badge("u1", "0.0"))
    BadgeToggle.reopen("0.0")
    Result.all(
      List(
        display("u1") ==== "block",
        Result.assert(hasClass("bb_u1", "open")).log("button open"),
        AppState.activeBadgeUid ==== Some("u1"),
        AppState.activeBadgePart ==== Some("0.0"),
      )
    )
  }
}
