package claudeproxymate.renderer.proxy

import hedgehog.*
import hedgehog.runner.*

object ProxyListViewSpec extends Properties {

  override def tests: List[Test] = List(
    example("buildListFrag empty list contains hist-empty class", testEmptyHasHistEmpty),
    example("buildListFrag empty list contains noCapturesTitle label", testEmptyHasTitle),
    example("buildListFrag empty list wraps hint in <small>", testEmptyWrapsHintInSmall),
    example("buildListFrag empty list renders {{br}} hint as <br />", testEmptyHintBrTokenRendered),
    example("buildListFrag empty list does not contain literal {{br}}", testEmptyNoLiteralBrToken),
    example("buildListFrag empty list does not contain prx-entry", testEmptyHasNoEntry),
    example("buildListFrag one entry contains prx-entry class", testOneEntryClass),
    example("buildListFrag selected entry has selected suffix", testSelectedClass),
    example("buildListFrag entry includes data-id attribute", testDataIdAttr),
    example("buildListFrag entry contains method/path/ts", testEntryFields),
    example("buildListFrag with model Some renders prx-model", testModelShown),
    example("buildListFrag with model None omits prx-model", testModelOmitted),
    example("buildListFrag status 200 renders green", testStatus200),
    example("buildListFrag status 500 renders red", testStatus500),
    example("buildListFrag status None renders ellipsis placeholder", testStatusNone),
    property("buildListFrag never leaks literal <script> for any field", testNoScriptLeak),
    property("buildListFrag round-trips id as data-id parsable Double", testIdRoundTrip),
    // Query-string masking (C3 PR4)
    example("path with sensitive query param renders masked value", testPathQueryMaskSensitive),
    example("path with non-sensitive query param renders verbatim", testPathQueryMaskNonSensitive),
  )

  private val sampleLabels = ProxyListLabels(
    noCapturesTitle = "no captures yet",
    noCapturesHint = "Start the proxy and{{br}}run Claude Code",
  )

  private def sampleEntry(
    id: Double = 1.0,
    method: String = "POST",
    path: String = "/v1/messages",
    ts: String = "12:34:56",
    model: Option[String] = Some("claude-opus-4-7"),
    status: Option[Int] = Some(200),
    selected: Boolean = false,
  ): ProxyListEntry =
    ProxyListEntry(id, method, path, ts, model, status, selected)

  private def render(entries: List[ProxyListEntry], labels: ProxyListLabels = sampleLabels): String =
    ProxyListView.buildListFrag(entries, labels).render

  def testEmptyHasHistEmpty: Result =
    Result.assert(render(Nil).contains("hist-empty")).log(render(Nil))

  def testEmptyHasTitle: Result =
    Result.assert(render(Nil).contains("no captures yet")).log(render(Nil))

  def testEmptyWrapsHintInSmall: Result = {
    val html = render(Nil)
    Result.all(
      List(
        Result.assert(html.contains("<small>")).log(s"<small> open missing: $html"),
        Result.assert(html.contains("</small>")).log(s"</small> close missing: $html"),
        Result.assert(html.contains("Start the proxy and")).log(s"hint line1 missing: $html"),
        Result.assert(html.contains("run Claude Code")).log(s"hint line2 missing: $html"),
      )
    )
  }

  def testEmptyHintBrTokenRendered: Result = {
    val html = render(Nil)
    Result.assert(html.contains("<br />")).log(s"<br /> missing: $html")
  }

  def testEmptyNoLiteralBrToken: Result = {
    val html = render(Nil)
    Result.assert(!html.contains("{{br}}")).log(s"literal {{br}} leaked: $html")
  }

  def testEmptyHasNoEntry: Result = {
    val html = render(Nil)
    Result.assert(!html.contains("prx-entry")).log(html)
  }

  def testOneEntryClass: Result = {
    val html = render(List(sampleEntry()))
    Result.all(
      List(
        Result.assert(html.contains("prx-entry")).log("prx-entry missing"),
        Result.assert(!html.contains("prx-entry selected")).log(s"unexpected selected: $html"),
      )
    )
  }

  def testSelectedClass: Result = {
    val html = render(List(sampleEntry(selected = true)))
    Result.assert(html.contains("prx-entry selected")).log(html)
  }

  def testDataIdAttr: Result = {
    val html = render(List(sampleEntry(id = 42.0)))
    Result.assert(html.contains(s"""${ProxyListView.EntryDataIdAttr}="42""")).log(html)
  }

  def testEntryFields: Result = {
    val html = render(List(sampleEntry(method = "GET", path = "/foo", ts = "00:00:01")))
    Result.all(
      List(
        Result.assert(html.contains("GET")).log("method missing"),
        Result.assert(html.contains("/foo")).log("path missing"),
        Result.assert(html.contains("00:00:01")).log("ts missing"),
      )
    )
  }

  def testModelShown: Result = {
    val html = render(List(sampleEntry(model = Some("claude-sonnet-4-6"))))
    Result.all(
      List(
        Result.assert(html.contains("prx-model")).log("prx-model div missing"),
        Result.assert(html.contains("claude-sonnet-4-6")).log("model name missing"),
      )
    )
  }

  def testModelOmitted: Result = {
    val html = render(List(sampleEntry(model = None)))
    Result.assert(!html.contains("prx-model")).log(html)
  }

  def testStatus200: Result = {
    val html = render(List(sampleEntry(status = Some(200))))
    Result.all(
      List(
        Result.assert(html.contains("var(--green)")).log("green color missing"),
        Result.assert(html.contains("200")).log("200 text missing"),
      )
    )
  }

  def testStatus500: Result = {
    val html = render(List(sampleEntry(status = Some(500))))
    Result.all(
      List(
        Result.assert(html.contains("var(--red)")).log("red color missing"),
        Result.assert(html.contains("500")).log("500 text missing"),
      )
    )
  }

  def testStatusNone: Result = {
    val html = render(List(sampleEntry(status = None)))
    Result.all(
      List(
        Result.assert(html.contains("…")).log("ellipsis missing"),
        Result.assert(html.contains("var(--dim)")).log("dim color missing"),
      )
    )
  }

  def testNoScriptLeak: Property =
    for {
      evil <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("evil")
    } yield {
      val payload = s"<script>alert('$evil')</script>"
      val html    = render(
        List(
          sampleEntry(
            method = payload,
            path = payload,
            ts = payload,
            model = Some(payload),
          )
        )
      )
      Result
        .assert(!html.contains("<script>"))
        .log(s"raw <script> leaked for evil=$evil: $html")
    }

  def testIdRoundTrip: Property =
    for {
      n <- Gen.int(Range.linear(0, 1000000)).log("n")
    } yield {
      val id      = n.toDouble
      val html    = render(List(sampleEntry(id = id)))
      val pattern = s"""${ProxyListView.EntryDataIdAttr}="${id.toString}""""
      Result
        .assert(html.contains(pattern))
        .log(s"data-id round-trip failed for id=$id html=$html")
    }

  // ── Query-string masking (C3 PR4) ─────────────────────────────────────

  def testPathQueryMaskSensitive: Result = {
    val html = render(List(sampleEntry(path = "/v1/x?token=fakeKEY12345")))
    Result.all(
      List(
        Result.assert(html.contains("/v1/x?token=***")).log(s"masked path missing: $html"),
        Result.assert(!html.contains("fakeKEY12345")).log(s"raw token leaked: $html"),
      )
    )
  }

  def testPathQueryMaskNonSensitive: Result = {
    val html = render(List(sampleEntry(path = "/v1/x?model=claude")))
    Result.all(
      List(
        Result.assert(html.contains("/v1/x?model=claude")).log(s"non-sensitive param mangled: $html"),
        Result.assert(!html.contains("***")).log(s"unexpected sentinel: $html"),
      )
    )
  }
}
