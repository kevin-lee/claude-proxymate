package claudeproxymate.renderer.proxy

import claudeproxymate.renderer.detail.TokenPopoverLinks
import hedgehog.*
import hedgehog.runner.*

object ProxyInfoPopoverViewSpec extends Properties {

  override def tests: List[Test] = List(
    example("buildPopoverFrag contains title class", testTitleClass),
    example("buildPopoverFrag contains body class", testBodyClass),
    example("buildPopoverFrag renders title text", testTitleText),
    example("buildPopoverFrag contains website link href", testWebsiteHref),
    example("buildPopoverFrag contains bug report link href", testIssuesHref),
    example("buildPopoverFrag links carry external-link, _blank and noopener", testLinkAttrs),
    example("buildPopoverFrag renders both link labels", testLinkLabels),
    property("buildPopoverFrag never leaks raw <script> from labels", testNoScriptLeak),
  )

  private def render(labels: ProxyInfoPopoverLabels): String =
    ProxyInfoPopoverView.buildPopoverFrag(labels).render

  private val sampleLabels = ProxyInfoPopoverLabels(
    title = "About Claude Proxymate",
    websiteLabel = "Website",
    bugReportLabel = "Report a bug",
  )

  def testTitleClass: Result =
    Result
      .assert(render(sampleLabels).contains(s"${ProxyInfoPopoverView.PopoverClass}-title"))
      .log(render(sampleLabels))

  def testBodyClass: Result =
    Result
      .assert(render(sampleLabels).contains(s"${ProxyInfoPopoverView.PopoverClass}-body"))
      .log(render(sampleLabels))

  def testTitleText: Result =
    Result
      .assert(render(sampleLabels).contains("About Claude Proxymate"))
      .log(render(sampleLabels))

  def testWebsiteHref: Result =
    Result
      .assert(render(sampleLabels).contains(s"""href="${ProxyInfoPopoverView.WebsiteHref}""""))
      .log(render(sampleLabels))

  def testIssuesHref: Result =
    Result
      .assert(render(sampleLabels).contains(s"""href="${ProxyInfoPopoverView.IssuesHref}""""))
      .log(render(sampleLabels))

  def testLinkAttrs: Result = {
    val html = render(sampleLabels)
    Result.all(
      List(
        (html.split(TokenPopoverLinks.ExternalLinkClass, -1).length - 1 ==== 2)
          .log(s"expected 2 external-link anchors: $html"),
        (html.split("""target="_blank"""", -1).length - 1 ==== 2)
          .log(s"expected 2 target=_blank anchors: $html"),
        (html.split("""rel="noopener"""", -1).length - 1 ==== 2)
          .log(s"expected 2 rel=noopener anchors: $html"),
      )
    )
  }

  def testLinkLabels: Result = {
    val html = render(sampleLabels)
    Result.all(
      List(
        Result.assert(html.contains("Website")).log(s"website label missing: $html"),
        Result.assert(html.contains("Report a bug")).log(s"bug report label missing: $html"),
      )
    )
  }

  def testNoScriptLeak: Property =
    for {
      evil <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("evil")
    } yield {
      val payload = s"<script>alert('$evil')</script>"
      val labels  = ProxyInfoPopoverLabels(
        title = payload,
        websiteLabel = payload,
        bugReportLabel = payload,
      )
      val html    = render(labels)
      Result
        .assert(!html.contains("<script>"))
        .log(s"raw <script> leaked for evil=$evil html=$html")
    }
}
