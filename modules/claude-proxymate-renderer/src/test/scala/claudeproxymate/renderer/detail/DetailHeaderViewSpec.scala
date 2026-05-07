package claudeproxymate.renderer.detail

import hedgehog.*
import hedgehog.runner.*

object DetailHeaderViewSpec extends Properties {

  override def tests: List[Test] = List(
    // Search bar
    example("buildSearchBarFrag empty query has no clear button or nav", testSearchBarEmpty),
    example("buildSearchBarFrag non-empty query has clear + prev + next + counter", testSearchBarNonEmpty),
    example("buildSearchBarFrag input has correct id + class", testSearchBarInput),
    example("buildSearchBarFrag input value reflects query", testSearchBarValue),
    example("buildSearchBarFrag never contains inline event handlers", testSearchBarNoInline),
    // Token pill
    example("buildTokenPillFrag empty badges renders proxy-token-pill", testPillEmpty),
    example("buildTokenPillFrag multiple badges render in order", testPillMultiOrder),
    example("buildTokenPillFrag badge with color emits style attribute", testPillBadgeColor),
    example("buildTokenPillFrag badge without color has no style attribute", testPillBadgeNoColor),
    example("buildTokenPillFrag carries data-cost attribute verbatim", testPillDataCost),
    example("buildTokenPillFrag round-trips ' in data-cost (double-quoted)", testPillDataCostQuoteRoundTrip),
    // XSS regression
    property("<script> in badge text never leaks raw", testNoScriptLeakInBadge),
    property("<script> in data-cost never breaks the attribute", testNoScriptLeakInDataCost),
    property("<script> in search labels never leaks raw", testNoScriptLeakInSearchLabels),
  )

  private val sampleSearchLabels = SearchBarLabels(
    placeholder = "search…",
    clear       = "clear",
    searchPrev  = "prev",
    searchNext  = "next",
  )

  private def renderSearchBar(query: String, labels: SearchBarLabels = sampleSearchLabels): String =
    DetailHeaderView.buildSearchBarFrag("proxyDetailSearchInput", "searchCounter", query, labels).render

  private def renderPill(pill: TokenPill): String =
    DetailHeaderView.buildTokenPillFrag(pill).render

  // ── Search bar ───────────────────────────────────────────────────────

  def testSearchBarEmpty: Result = {
    val out = renderSearchBar("")
    Result.all(
      List(
        Result.assert(!out.contains("msg-search-clear")).log(s"unexpected clear: $out"),
        Result.assert(!out.contains("search-nav-prev")).log(s"unexpected prev: $out"),
        Result.assert(!out.contains("search-nav-next")).log(s"unexpected next: $out"),
      )
    )
  }

  def testSearchBarNonEmpty: Result = {
    val out = renderSearchBar("foo")
    Result.all(
      List(
        Result.assert(out.contains("msg-search-clear")).log("clear missing"),
        Result.assert(out.contains("search-nav-prev")).log("prev missing"),
        Result.assert(out.contains("search-nav-next")).log("next missing"),
        Result.assert(out.contains("id=\"searchCounter\"")).log("counter id missing"),
      )
    )
  }

  def testSearchBarInput: Result = {
    val out = renderSearchBar("")
    Result.all(
      List(
        Result.assert(out.contains("class=\"msg-search-input\"")).log("class missing"),
        Result.assert(out.contains("id=\"proxyDetailSearchInput\"")).log("id missing"),
      )
    )
  }

  def testSearchBarValue: Result = {
    val out = renderSearchBar("hello")
    Result.assert(out.contains("value=\"hello\"")).log(out)
  }

  def testSearchBarNoInline: Result = {
    val out = renderSearchBar("foo")
    Result.all(
      List(
        Result.assert(!out.contains("onclick=")).log(s"unexpected onclick: $out"),
        Result.assert(!out.contains("oninput=")).log(s"unexpected oninput: $out"),
        Result.assert(!out.contains("oncompositionstart=")).log(s"unexpected oncompositionstart: $out"),
        Result.assert(!out.contains("oncompositionend=")).log(s"unexpected oncompositionend: $out"),
      )
    )
  }

  // ── Token pill ───────────────────────────────────────────────────────

  def testPillEmpty: Result = {
    val out = renderPill(TokenPill(badges = Nil, dataCost = "{}"))
    Result.all(
      List(
        Result.assert(out.contains("class=\"proxy-token-pill\"")).log(s"class missing: $out"),
        Result.assert(out.contains("data-cost=\"{}\"")).log(s"data-cost missing: $out"),
        Result.assert(!out.contains("class=\"tt-badge\"")).log(s"unexpected badge: $out"),
      )
    )
  }

  def testPillMultiOrder: Result = {
    val pill = TokenPill(
      badges   = List(TokenBadge("first", None), TokenBadge("second", None), TokenBadge("third", None)),
      dataCost = "{}",
    )
    val out = renderPill(pill)
    val firstIdx  = out.indexOf("first")
    val secondIdx = out.indexOf("second")
    val thirdIdx  = out.indexOf("third")
    Result.assert(firstIdx >= 0 && secondIdx > firstIdx && thirdIdx > secondIdx)
      .log(s"order violated: first=$firstIdx second=$secondIdx third=$thirdIdx out=$out")
  }

  def testPillBadgeColor: Result = {
    val pill = TokenPill(badges = List(TokenBadge("colored", Some("var(--green)"))), dataCost = "{}")
    val out  = renderPill(pill)
    Result.assert(out.contains("style=\"color:var(--green)\""))
      .log(s"color style missing: $out")
  }

  def testPillBadgeNoColor: Result = {
    val pill = TokenPill(badges = List(TokenBadge("plain", None)), dataCost = "{}")
    val out  = renderPill(pill)
    Result.all(
      List(
        Result.assert(out.contains(">plain<")).log(s"text missing: $out"),
        Result.assert(!out.contains("style=\"color")).log(s"unexpected color style: $out"),
      )
    )
  }

  def testPillDataCost: Result = {
    val cost = """{"model":"claude","kb":"1.0","total":"$0.01"}"""
    val pill = TokenPill(badges = Nil, dataCost = cost)
    val out  = renderPill(pill)
    // Scalatags will escape " as &quot; inside attribute values.
    Result.assert(out.contains("data-cost=") && out.contains("claude"))
      .log(s"data-cost not present or model missing: $out")
  }

  def testPillDataCostQuoteRoundTrip: Result = {
    val cost = "x'y"
    val pill = TokenPill(badges = Nil, dataCost = cost)
    val out  = renderPill(pill)
    // Scalatags emits the attribute as data-cost="..." (double-quoted), so a
    // literal ' inside is safe and round-trips verbatim. The original
    // implementation manually escaped ' → &#39; because it used a
    // single-quoted attribute delimiter; that workaround is unnecessary here.
    Result.assert(out.contains("data-cost=\"x'y\""))
      .log(s"' should round-trip verbatim inside a \"-quoted attr: $out")
  }

  // ── XSS regression ───────────────────────────────────────────────────

  def testNoScriptLeakInBadge: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val pill    = TokenPill(badges = List(TokenBadge(payload, None)), dataCost = "{}")
      val out     = renderPill(pill)
      Result.assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakInDataCost: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val pill    = TokenPill(badges = Nil, dataCost = payload)
      val out     = renderPill(pill)
      Result.assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakInSearchLabels: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val labels  = sampleSearchLabels.copy(placeholder = payload, clear = payload, searchPrev = payload, searchNext = payload)
      val out     = renderSearchBar("hello", labels)
      Result.assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }
}
