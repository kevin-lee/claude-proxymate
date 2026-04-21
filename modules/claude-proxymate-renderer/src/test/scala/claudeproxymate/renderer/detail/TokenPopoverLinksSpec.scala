package claudeproxymate.renderer.detail

import hedgehog.*
import hedgehog.runner.*

object TokenPopoverLinksSpec extends Properties {

  override def tests: List[Test] = List(
    example(
      "external-link class + non-empty href -> Some(href)",
      testExternalLinkReturnsHref,
    ),
    example(
      "external-link class + empty href -> None",
      testExternalLinkEmptyHref,
    ),
    example(
      "missing external-link class + non-empty href -> None",
      testNoClassReturnsNone,
    ),
    example(
      "unrelated classes only -> None",
      testUnrelatedClassesOnly,
    ),
    property(
      "any classes list NOT containing ExternalLinkClass -> None",
      testNonMatchingClasses,
    ),
  )

  def testExternalLinkReturnsHref: Result =
    TokenPopoverLinks.extractExternalLinkHref(
      List(TokenPopoverLinks.ExternalLinkClass),
      "https://docs.anthropic.com/",
    ) ==== Some("https://docs.anthropic.com/")

  def testExternalLinkEmptyHref: Result =
    TokenPopoverLinks.extractExternalLinkHref(
      List(TokenPopoverLinks.ExternalLinkClass),
      "",
    ) ==== None

  def testNoClassReturnsNone: Result =
    TokenPopoverLinks.extractExternalLinkHref(
      List("some-other-class"),
      "https://docs.anthropic.com/",
    ) ==== None

  def testUnrelatedClassesOnly: Result =
    TokenPopoverLinks.extractExternalLinkHref(
      List("token-popover-copy", "tp-label"),
      "https://docs.anthropic.com/",
    ) ==== None

  def testNonMatchingClasses: Property =
    for {
      classes <- Gen
        .list(
          Gen.string(Gen.alpha, Range.linear(1, 15)),
          Range.linear(0, 5),
        )
        .map(_.filterNot(_ == TokenPopoverLinks.ExternalLinkClass))
        .log("classes")
    } yield {
      val result = TokenPopoverLinks.extractExternalLinkHref(classes, "https://x")
      (result ==== None).log(s"classes=$classes")
    }
}
