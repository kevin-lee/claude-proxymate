package claudeproxymate.renderer.detail

import hedgehog.*
import hedgehog.runner.*

object TokenPopoverViewSpec extends Properties {

  override def tests: List[Test] = List(
    example("buildPopoverFrag contains token-popover-title class", testTitleClass),
    example("buildPopoverFrag contains token-popover-copy class", testCopyClass),
    example("buildPopoverFrag contains external-link class on docs anchor", testExternalLinkClass),
    example("buildPopoverFrag contains all row values", testRowValues),
    example("buildPopoverFrag renders description when label matches", testDescriptionShown),
    example("buildPopoverFrag omits description when label does not match", testDescriptionOmitted),
    example("buildPopoverFrag includes cache-saving note when label Some", testCacheSavingIncluded),
    example("buildPopoverFrag omits cache-saving note when label None", testCacheSavingOmitted),
    example("buildPopoverFrag escapes HTML in model name", testHtmlEscapedInModel),
    example("buildCopyText produces expected multiline format", testCopyTextFormat),
    example("buildCopyText includes all rows", testCopyTextAllRows),
    property("buildPopoverFrag output never contains literal <script>", testNoScriptLeak),
  )

  private val sampleRow = TokenPopoverRow(
    label  = "Input",
    tokens = "1.2K",
    price  = "3.00",
    cost   = "$0.0036",
  )

  private def sampleData(model: String = "claude-opus-4-7"): TokenPopoverData =
    TokenPopoverData(
      model       = model,
      kb          = "2.5",
      total       = "$0.0100",
      cachePct    = 75,
      pricingDate = "2026-04-22",
      rows        = List(sampleRow),
    )

  private val sampleLabels = TokenPopoverLabels(
    costTitle         = "Cost",
    modelLabel        = "Model",
    reqSizeLabel      = "Size",
    copyBtn           = "Copy",
    totalLabel        = "Total",
    cacheHitRateLabel = "Cache hit rate",
    notePricingDate   = "Pricing as of: 2026-04-22",
    noteModelPrice    = "Pricing varies by model",
    noteOfficialDoc   = "Official Docs",
    noteMTok          = "MTok = million tokens",
    noteCacheSaving   = Some("Cache 75%"),
  )

  private val sampleDescriptions = Map(
    "Input" -> "Tokens that count toward input cost",
  )

  private def render(
    data: TokenPopoverData = sampleData(),
    labels: TokenPopoverLabels = sampleLabels,
    descs: Map[String, String] = sampleDescriptions,
  ): String =
    TokenPopoverView.buildPopoverFrag(data, labels, descs).render

  def testTitleClass: Result =
    Result.assert(render().contains("token-popover-title")).log(render())

  def testCopyClass: Result =
    Result.assert(render().contains("token-popover-copy")).log(render())

  def testExternalLinkClass: Result =
    Result.assert(render().contains(TokenPopoverLinks.ExternalLinkClass)).log(render())

  def testRowValues: Result = {
    val html = render()
    Result.all(
      List(
        Result.assert(html.contains("Input")).log("label missing"),
        Result.assert(html.contains("1.2K")).log("tokens missing"),
        Result.assert(html.contains("3.00")).log("price missing"),
        Result.assert(html.contains("$0.0036")).log("cost missing"),
      )
    )
  }

  def testDescriptionShown: Result =
    Result.assert(render().contains("Tokens that count toward input cost")).log(render())

  def testDescriptionOmitted: Result = {
    val html = render(descs = Map.empty)
    Result.assert(!html.contains("tp-desc")).log(html)
  }

  def testCacheSavingIncluded: Result =
    Result.assert(render().contains("Cache 75%")).log(render())

  def testCacheSavingOmitted: Result = {
    val html = render(labels = sampleLabels.copy(noteCacheSaving = None))
    Result.assert(!html.contains("Cache 75%")).log(html)
  }

  def testHtmlEscapedInModel: Result = {
    val html = render(data = sampleData(model = "<script>alert(1)</script>"))
    Result.all(
      List(
        Result.assert(!html.contains("<script>")).log(s"raw <script> leaked: $html"),
        Result.assert(html.contains("&lt;script&gt;")).log(s"escaped form missing: $html"),
      )
    )
  }

  def testCopyTextFormat: Result = {
    val text = TokenPopoverView.buildCopyText(sampleData(), sampleLabels)
    val expected = List(
      "Model: claude-opus-4-7",
      "Size: 2.5 KB",
      "",
      "Input: 1.2K tok × $3.00/MTok = $0.0036",
      "",
      "Total: $0.0100",
      "Cache hit rate: 75%",
    ).mkString("\n")
    text ==== expected
  }

  def testCopyTextAllRows: Result = {
    val row2 = TokenPopoverRow("Output", "0.3K", "15.00", "$0.0045")
    val data = sampleData().copy(rows = List(sampleRow, row2))
    val text = TokenPopoverView.buildCopyText(data, sampleLabels)
    Result.all(
      List(
        Result.assert(text.contains("Input: 1.2K")).log(text),
        Result.assert(text.contains("Output: 0.3K")).log(text),
      )
    )
  }

  def testNoScriptLeak: Property =
    for {
      evil <- Gen.string(Gen.alpha, Range.linear(0, 20)).log("evil")
    } yield {
      val html = render(data = sampleData(model = s"<script>alert('$evil')</script>"))
      Result.assert(!html.contains("<script>"))
        .log(s"raw <script> leaked for evil=$evil: $html")
    }
}
