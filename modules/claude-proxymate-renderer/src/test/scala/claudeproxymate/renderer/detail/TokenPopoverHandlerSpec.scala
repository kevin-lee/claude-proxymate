package claudeproxymate.renderer.detail

import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*

/** The click handler of [[TokenPopover]], in jsdom. */
object TokenPopoverHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("a click on an external link in the popover is cancelled", testExternalLink),
    example("a link with no href is not handled as an external link", testLinkNoHref),
    example("the popover copy button copies its text and keeps the popover open", testCopyButton),
    example("a click inside the popover keeps it open", testClickInside),
    example("a click outside the popover closes it", testClickOutside),
    example("a click on the token pill opens the popover in the pill", testOpenOnPill),
    example("a click on another pill closes the old popover and opens a new one", testReopen),
  )

  private val Cost: String =
    """{"model":"claude-x","kb":"1.0","total":"$0.01","cachePct":60,"pricingDate":"2026-01-01","fastMode":true,"rows":[{"label":"Output","tokens":"10","price":15,"cost":"$0.01","unit":"mtok"}]}"""

  private def pill(id: String): String =
    s"""<span id="$id" class="proxy-token-pill" data-cost='$Cost'><b id="$id-text">pill</b></span>"""

  private val OpenPopover: String =
    """<div class="token-popover"><span id="inside">x</span><button id="copy" class="token-popover-copy" data-text="copied text">Copy</button><a id="link" class="token-popover-link" href="https://example.com/">doc</a><a id="nohref" class="token-popover-link">doc</a></div>"""

  private def popoverCount: Int = org.scalajs.dom.document.querySelectorAll(".token-popover").length

  def testExternalLink: Result = {
    DomTestKit.setUp(OpenPopover)
    val notCancelled = DomTestKit.click(DomTestKit.element("#link"))
    Result.all(
      List(
        Result.assert(!notCancelled).log("the click is cancelled"),
        popoverCount ==== 1,
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testLinkNoHref: Result = {
    DomTestKit.setUp(OpenPopover)
    val notCancelled = DomTestKit.click(DomTestKit.element("#nohref"))
    Result.all(
      List(
        Result.assert(notCancelled).log("the click is not cancelled"),
        popoverCount ==== 1,
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testCopyButton: Result = {
    DomTestKit.setUp(OpenPopover)
    val _ = DomTestKit.click(DomTestKit.element("#copy"))
    Result.all(
      List(
        DomTestKit.clipboardTexts ==== List("copied text"),
        DomTestKit.element("#copy").textContent ==== "token.copied",
        popoverCount ==== 1,
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testClickInside: Result = {
    DomTestKit.setUp(OpenPopover)
    val _ = DomTestKit.click(DomTestKit.element("#inside"))
    Result.all(List(popoverCount ==== 1, DomTestKit.listenerErrors ==== Nil))
  }

  def testClickOutside: Result = {
    DomTestKit.setUp(OpenPopover + """<i id="other"></i>""")
    val _ = DomTestKit.click(DomTestKit.element("#other"))
    Result.all(List(popoverCount ==== 0, DomTestKit.listenerErrors ==== Nil))
  }

  def testOpenOnPill: Result = {
    DomTestKit.setUp(pill("p1"))
    val _ = DomTestKit.click(DomTestKit.element("#p1-text"))
    Result.all(
      List(
        popoverCount ==== 1,
        Result
          .assert(DomTestKit.element("#p1 .token-popover") != null)
          .log("the popover is in the pill"), // scalafix:ok DisableSyntax.null
        Result
          .assert(DomTestKit.element("#p1 .token-popover").textContent.contains("claude-x"))
          .log("the popover shows the model"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testReopen: Result = {
    DomTestKit.setUp(pill("p1") + pill("p2"))
    val _ = DomTestKit.click(DomTestKit.element("#p1-text"))
    val _ = DomTestKit.click(DomTestKit.element("#p2-text"))
    Result.all(
      List(
        popoverCount ==== 1,
        Result
          .assert(DomTestKit.element("#p2 .token-popover") != null)
          .log("the popover is in pill 2"), // scalafix:ok DisableSyntax.null
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }
}
