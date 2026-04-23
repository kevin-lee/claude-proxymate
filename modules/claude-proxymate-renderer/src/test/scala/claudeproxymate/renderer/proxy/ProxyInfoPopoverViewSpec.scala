package claudeproxymate.renderer.proxy

import hedgehog.*
import hedgehog.runner.*

object ProxyInfoPopoverViewSpec extends Properties {

  override def tests: List[Test] = List(
    example("buildPopoverFrag contains title class", testTitleClass),
    example("buildPopoverFrag contains body class", testBodyClass),
    example("buildPopoverFrag renders title text", testTitleText),
    example("buildPopoverFrag renders {{br}} in desc as <br />", testDescBrRendered),
    example("buildPopoverFrag does not leak literal {{br}}", testNoLiteralBrToken),
    example("buildPopoverFrag preserves unknown {{foo}} as literal", testUnknownTokenLiteral),
    example("buildPopoverFrag with empty desc still renders title", testEmptyDesc),
    property("buildPopoverFrag never leaks raw <script> from title or desc", testNoScriptLeak),
  )

  private def render(labels: ProxyInfoPopoverLabels): String =
    ProxyInfoPopoverView.buildPopoverFrag(labels).render

  private val sampleLabels = ProxyInfoPopoverLabels(
    title = "⚡ Live API Traffic Intercept",
    desc  = "Intercepts API requests from the Claude Code CLI.{{br}}Start the proxy, then run Claude Code.",
  )

  def testTitleClass: Result =
    Result.assert(render(sampleLabels).contains(s"${ProxyInfoPopoverView.PopoverClass}-title"))
      .log(render(sampleLabels))

  def testBodyClass: Result =
    Result.assert(render(sampleLabels).contains(s"${ProxyInfoPopoverView.PopoverClass}-body"))
      .log(render(sampleLabels))

  def testTitleText: Result =
    Result.assert(render(sampleLabels).contains("⚡ Live API Traffic Intercept"))
      .log(render(sampleLabels))

  def testDescBrRendered: Result =
    Result.assert(render(sampleLabels).contains("<br />"))
      .log(render(sampleLabels))

  def testNoLiteralBrToken: Result = {
    val html = render(sampleLabels)
    Result.assert(!html.contains("{{br}}")).log(s"literal {{br}} leaked: $html")
  }

  def testUnknownTokenLiteral: Result = {
    val html = render(sampleLabels.copy(desc = "before {{foo}}x{{/foo}} after"))
    Result.all(
      List(
        Result.assert(html.contains("{{foo}}")).log(s"opening not literal: $html"),
        Result.assert(html.contains("{{/foo}}")).log(s"closing not literal: $html"),
        Result.assert(!html.contains("<foo")).log(s"leaked tag: $html"),
      )
    )
  }

  def testEmptyDesc: Result = {
    val html = render(sampleLabels.copy(desc = ""))
    Result.assert(html.contains("⚡ Live API Traffic Intercept")).log(html)
  }

  def testNoScriptLeak: Property =
    for {
      evil <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("evil")
    } yield {
      val payload = s"<script>alert('$evil')</script>"
      val labels = ProxyInfoPopoverLabels(
        title = payload,
        desc  = s"line1 {{br}} $payload",
      )
      val html = render(labels)
      Result.assert(!html.contains("<script>"))
        .log(s"raw <script> leaked for evil=$evil html=$html")
    }
}
