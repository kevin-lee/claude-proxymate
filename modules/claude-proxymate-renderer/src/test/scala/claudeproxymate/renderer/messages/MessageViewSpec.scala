package claudeproxymate.renderer.messages

import claudeproxymate.renderer.messages.MsgContent.*
import claudeproxymate.renderer.messages.MsgPart.*
import hedgehog.*
import hedgehog.runner.*

object MessageViewSpec extends Properties {

  override def tests: List[Test] = List(
    // Empty / no-results
    example("buildEmptyFrag contains proxy-empty + label", testEmptyFrag),
    example("buildCardsFrag with empty list renders no cards", testCardsEmpty),
    example("buildNoResultsFrag contains proxy-empty + label", testNoResultsFrag),
    // Header
    example("buildHeaderFrag contains all four filter buttons", testHeaderAllFilterButtons),
    example("buildHeaderFrag active filter has 'active' suffix", testHeaderActiveFilter),
    example("buildHeaderFrag filter buttons carry data-msg-filter", testHeaderFilterDataAttr),
    example("buildHeaderFrag search bar has input + clear button when q nonempty", testHeaderSearchBarWithQuery),
    // Single card
    example("user card carries msg-user class", testUserCardClass),
    example("assistant card carries msg-assistant class", testAssistantCardClass),
    example("card body contains text content", testCardBodyText),
    example("card has msg-role label", testCardRoleLabel),
    // Content types
    example("tool-use renders msg-tool with name", testToolUseContent),
    example("tool-result truncated renders ellipsis", testToolResultTruncated),
    example("other content type renders msg-other [typeName]", testOtherContent),
    example("multiple contents render in order", testMultiContentOrder),
    // User message parts
    example("TextMsgPart renders msg-typed", testTextMsgPart),
    example("InjectedMsgPart renders msg-injected-row + badge", testInjectedMsgPart),
    example("InjectedMsgPart badge has bb_/bc_ ids and data-msg-badge-uid", testInjectedMsgPartIds),
    // Search highlighting
    example("search query match wraps in mark", testSearchHighlightMatch),
    example("empty query produces no mark", testSearchHighlightEmpty),
    // XSS regression
    property("<script> in card text never leaks raw", testNoScriptLeakInText),
    property("<script> in tool name / preview never leaks raw", testNoScriptLeakInTool),
    property("<script> in injected badge label / content never leaks raw", testNoScriptLeakInBadge),
    // Inline-handler regression
    example("output never contains inline onclick / oninput / oncomposition", testNoInlineHandlers),
    // Token-shape masking (C3 PR2b)
    example("TextContent containing sk-ant-… renders a token-mask span", testTokenMaskInTextContent),
    example("token mask in TextContent does not leak the raw token", testTokenMaskNoLeakInTextContent),
    example("InjectedMsgPart with embedded token is masked", testTokenMaskInInjectedMsgPart),
    example("ToolResultContent.preview with embedded token is masked", testTokenMaskInToolResult),
    example("token id includes the m. namespace prefix", testTokenIdNamespace),
    example("injected token mask id uses partIdx, not badge uid", testInjTokenIdUsesPartIdx),
    // Correlation-id masking (C3 PR3)
    example("TextContent with msg_… renders a corr-mask span", testCorrMaskInTextContent),
    example("corr-mask in TextContent does not leak the raw id", testCorrMaskNoLeakInTextContent),
    example("corr id in messages tab uses corr:m. namespace", testCorrIdNamespace),
  )

  // ── Helpers ───────────────────────────────────────────────────────────

  private val sampleFilterLabels = FilterLabels("User", "Typed", "Assistant", "All")
  private val sampleSearchLabels = SearchLabels("Search…", "Clear")

  private def renderHeader(
    activeFilter: String,
    query: String,
  ): String =
    MessageView
      .buildHeaderFrag(
        activeFilter = activeFilter,
        filterLabels = sampleFilterLabels,
        msgCountId = "msgCount",
        searchInputId = "msgSearchInput",
        searchLabels = sampleSearchLabels,
        query = query,
      )
      .render

  private def renderCards(cards: List[MsgCard]): String =
    renderCards(cards, isUserFilter = false, query = "")

  private def renderCards(cards: List[MsgCard], isUserFilter: Boolean, query: String): String =
    MessageView.buildCardsFrag(cards, isUserFilter, query).render

  private def textCard(role: String, text: String): MsgCard =
    MsgCard(role, contents = List(TextContent(text)), userParts = Nil, rawIdx = 0)

  private def userCard(parts: List[MsgPart]): MsgCard =
    MsgCard("user", contents = Nil, userParts = parts, rawIdx = 0)

  // ── Empty / no-results ────────────────────────────────────────────────

  def testEmptyFrag: Result = {
    val out = MessageView.buildEmptyFrag("no msgs").render
    Result.all(
      List(
        Result.assert(out.contains("class=\"proxy-empty\"")).log(out),
        Result.assert(out.contains("no msgs")).log(out),
      )
    )
  }

  def testCardsEmpty: Result =
    renderCards(Nil) ==== ""

  def testNoResultsFrag: Result = {
    val out = MessageView.buildNoResultsFrag("no results").render
    Result.all(
      List(
        Result.assert(out.contains("class=\"proxy-empty\"")).log(out),
        Result.assert(out.contains("no results")).log(out),
      )
    )
  }

  // ── Header ────────────────────────────────────────────────────────────

  def testHeaderAllFilterButtons: Result = {
    val out = renderHeader(activeFilter = "user", query = "")
    Result.all(
      List(
        Result.assert(out.contains(">User<")).log("user btn missing"),
        Result.assert(out.contains(">Typed<")).log("typed btn missing"),
        Result.assert(out.contains(">Assistant<")).log("assistant btn missing"),
        Result.assert(out.contains(">All<")).log("all btn missing"),
      )
    )
  }

  def testHeaderActiveFilter: Result = {
    val out         = renderHeader(activeFilter = "typed", query = "")
    val activeCount = out.split(" active\"", -1).length - 1
    Result
      .assert(activeCount == 1)
      .log(s"expected 1 active class, got $activeCount: $out")
  }

  def testHeaderFilterDataAttr: Result = {
    val out = renderHeader(activeFilter = "user", query = "")
    Result.all(
      List(
        Result.assert(out.contains("data-msg-filter=\"user\"")).log("user data-attr missing"),
        Result.assert(out.contains("data-msg-filter=\"typed\"")).log("typed data-attr missing"),
        Result.assert(out.contains("data-msg-filter=\"assistant\"")).log("assistant data-attr missing"),
        Result.assert(out.contains("data-msg-filter=\"all\"")).log("all data-attr missing"),
      )
    )
  }

  def testHeaderSearchBarWithQuery: Result = {
    val out = renderHeader(activeFilter = "all", query = "foo")
    Result.all(
      List(
        Result.assert(out.contains("class=\"msg-search-input\"")).log("search input class missing"),
        Result.assert(out.contains("id=\"msgSearchInput\"")).log("search input id missing"),
        Result.assert(out.contains("value=\"foo\"")).log("search value missing"),
        Result.assert(out.contains("class=\"msg-search-clear\"")).log("clear button missing"),
      )
    )
  }

  // ── Single card ───────────────────────────────────────────────────────

  def testUserCardClass: Result = {
    val out = renderCards(List(userCard(List(TextMsgPart("hi")))))
    Result.assert(out.contains("class=\"msg-card msg-user\"")).log(out)
  }

  def testAssistantCardClass: Result = {
    val out = renderCards(List(textCard("assistant", "hi")))
    Result.assert(out.contains("class=\"msg-card msg-assistant\"")).log(out)
  }

  def testCardBodyText: Result = {
    val out = renderCards(List(textCard("assistant", "hello world")))
    Result.assert(out.contains("hello world")).log(out)
  }

  def testCardRoleLabel: Result = {
    val out = renderCards(List(textCard("assistant", "x")))
    Result.assert(out.contains("class=\"msg-role\">assistant<")).log(out)
  }

  // ── Content types ─────────────────────────────────────────────────────

  def testToolUseContent: Result = {
    val card = MsgCard("assistant", List(ToolUseContent("Read")), Nil, rawIdx = 0)
    val out  = renderCards(List(card))
    Result.all(
      List(
        Result.assert(out.contains("class=\"msg-tool\"")).log("msg-tool missing"),
        Result.assert(out.contains("Read()")).log("tool name missing"),
      )
    )
  }

  def testToolResultTruncated: Result = {
    val card  = MsgCard("assistant", List(ToolResultContent("preview text", truncated = true)), Nil, rawIdx = 0)
    val cardF = MsgCard("assistant", List(ToolResultContent("full preview", truncated = false)), Nil, rawIdx = 0)
    val outT  = renderCards(List(card))
    val outF  = renderCards(List(cardF))
    Result.all(
      List(
        Result.assert(outT.contains("class=\"msg-tool-result\"")).log("msg-tool-result missing"),
        Result.assert(outT.contains("…")).log(s"ellipsis missing for truncated: $outT"),
        Result.assert(!outF.contains("…")).log(s"unexpected ellipsis for non-truncated: $outF"),
      )
    )
  }

  def testOtherContent: Result = {
    val card = MsgCard("assistant", List(OtherContent("image")), Nil, rawIdx = 0)
    val out  = renderCards(List(card))
    Result.all(
      List(
        Result.assert(out.contains("class=\"msg-other\"")).log("msg-other missing"),
        Result.assert(out.contains("[image]")).log(s"[image] missing: $out"),
      )
    )
  }

  def testMultiContentOrder: Result = {
    val card      = MsgCard("assistant", List(TextContent("first"), ToolUseContent("second")), Nil, rawIdx = 0)
    val out       = renderCards(List(card))
    val firstIdx  = out.indexOf("first")
    val secondIdx = out.indexOf("second")
    Result.all(
      List(
        Result
          .assert(firstIdx >= 0 && secondIdx > firstIdx)
          .log(s"order violated, firstIdx=$firstIdx secondIdx=$secondIdx: $out"),
      )
    )
  }

  // ── User message parts ────────────────────────────────────────────────

  def testTextMsgPart: Result = {
    val out = renderCards(List(userCard(List(TextMsgPart("hi there")))))
    Result.all(
      List(
        Result.assert(out.contains("class=\"msg-typed\"")).log("msg-typed missing"),
        Result.assert(out.contains("hi there")).log("text missing"),
      )
    )
  }

  def testInjectedMsgPart: Result = {
    val card = userCard(List(InjectedMsgPart("u1", "Skills", "skills body", "green")))
    val out  = renderCards(List(card))
    Result.all(
      List(
        Result.assert(out.contains("class=\"msg-injected-row\"")).log("row missing"),
        Result.assert(out.contains("msg-badge green expandable")).log("badge classes missing"),
        Result.assert(out.contains("Skills")).log("label missing"),
        Result.assert(out.contains("skills body")).log("content missing"),
      )
    )
  }

  def testInjectedMsgPartIds: Result = {
    val card = userCard(List(InjectedMsgPart("u42", "L", "C", "yellow")))
    val out  = renderCards(List(card))
    Result.all(
      List(
        Result.assert(out.contains("id=\"bb_u42\"")).log("bb_u42 missing"),
        Result.assert(out.contains("id=\"bc_u42\"")).log("bc_u42 missing"),
        Result.assert(out.contains("data-msg-badge-uid=\"u42\"")).log("data-attr missing"),
      )
    )
  }

  // ── Search highlighting ───────────────────────────────────────────────

  def testSearchHighlightMatch: Result = {
    val out = renderCards(List(textCard("assistant", "find foo here")), isUserFilter = false, query = "foo")
    Result.assert(out.contains("<mark class=\"search-hl\">foo</mark>")).log(out)
  }

  def testSearchHighlightEmpty: Result = {
    val out = renderCards(List(textCard("assistant", "find foo here")), isUserFilter = false, query = "")
    Result.assert(!out.contains("<mark")).log(out)
  }

  // ── XSS regression ────────────────────────────────────────────────────

  def testNoScriptLeakInText: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val out     = renderCards(List(textCard("assistant", payload)))
      Result
        .assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakInTool: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val card    = MsgCard(
        "assistant",
        List(ToolUseContent(payload), ToolResultContent(payload, truncated = false)),
        Nil,
        rawIdx = 0
      )
      val out     = renderCards(List(card))
      Result
        .assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakInBadge: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val card    = userCard(List(InjectedMsgPart("u1", payload, payload, "green")))
      val out     = renderCards(List(card))
      Result
        .assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  // ── Inline-handler regression ─────────────────────────────────────────

  def testNoInlineHandlers: Result = {
    val card = userCard(List(InjectedMsgPart("u1", "L", "C", "green")))
    val out  = renderCards(List(card)) + renderHeader(activeFilter = "user", query = "q")
    Result.all(
      List(
        Result.assert(!out.contains("onclick=")).log("unexpected onclick"),
        Result.assert(!out.contains("oninput=")).log("unexpected oninput"),
        Result.assert(!out.contains("oncompositionstart=")).log("unexpected oncompositionstart"),
        Result.assert(!out.contains("oncompositionend=")).log("unexpected oncompositionend"),
      )
    )
  }

  // ── Token-shape masking (C3 PR2b) ─────────────────────────────────────

  private val FakeAnthropic = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"

  def testTokenMaskInTextContent: Result = {
    val card = MsgCard("assistant", List(TextContent(s"prefix $FakeAnthropic suffix")), Nil, rawIdx = 0)
    val out  = renderCards(List(card))
    Result.all(
      List(
        Result
          .assert(out.contains("class=\"jt-token-mask\""))
          .log(s"jt-token-mask span missing: $out"),
        Result
          .assert(out.contains("data-token-id=\"m."))
          .log(s"messages-namespace token id missing: $out"),
      )
    )
  }

  def testTokenMaskNoLeakInTextContent: Result = {
    val card = MsgCard("assistant", List(TextContent(s"prefix $FakeAnthropic suffix")), Nil, rawIdx = 0)
    val out  = renderCards(List(card))
    Result
      .assert(!out.contains(FakeAnthropic))
      .log(s"raw token leaked: $out")
  }

  def testTokenMaskInInjectedMsgPart: Result = {
    val card = userCard(List(InjectedMsgPart("u1", "label", FakeAnthropic, "badge-x")))
    val out  = renderCards(List(card))
    Result.all(
      List(
        Result.assert(out.contains("class=\"jt-token-mask\"")).log(s"jt-token-mask span missing: $out"),
        Result.assert(!out.contains(FakeAnthropic)).log(s"raw token leaked: $out"),
      )
    )
  }

  def testTokenMaskInToolResult: Result = {
    val card =
      MsgCard("assistant", List(ToolResultContent(s"result: $FakeAnthropic", truncated = false)), Nil, rawIdx = 0)
    val out  = renderCards(List(card))
    Result.all(
      List(
        Result.assert(out.contains("class=\"jt-token-mask\"")).log(s"jt-token-mask span missing: $out"),
        Result.assert(!out.contains(FakeAnthropic)).log(s"raw token leaked: $out"),
      )
    )
  }

  def testTokenIdNamespace: Result = {
    // Card at rawIdx = 7, first text content. Token at offset 0.
    val card = MsgCard(
      role = "assistant",
      contents = List(TextContent(FakeAnthropic)),
      userParts = Nil,
      rawIdx = 7,
    )
    val out  = renderCards(List(card))
    Result
      .assert(out.contains("data-token-id=\"m.7.text.0#0\""))
      .log(s"expected `data-token-id=\"m.7.text.0#0\"` in: $out")
  }

  def testInjTokenIdUsesPartIdx: Result = {
    /* The badge uid ("u9") is minted from a global counter per parse
     * and must NOT appear in mask ids — those derive from partIdx so
     * they're stable across re-parses and reconstructible by Copy. */
    val card = MsgCard(
      role = "user",
      contents = Nil,
      userParts = List(TextMsgPart("x"), InjectedMsgPart("u9", "L", FakeAnthropic, "badge-x")),
      rawIdx = 3,
    )
    val out  = renderCards(List(card))
    Result.all(
      List(
        Result
          .assert(out.contains("data-token-id=\"m.3.inj.1#0\""))
          .log(s"expected `data-token-id=\"m.3.inj.1#0\"` in: $out"),
        Result
          .assert(!out.contains("data-token-id=\"m.3.inj.u9#0\""))
          .log(s"uid-based mask id still rendered: $out"),
      )
    )
  }

  // ── Correlation-id masking (C3 PR3) ───────────────────────────────────

  private val FakeMsgId = "msg_01ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def testCorrMaskInTextContent: Result = {
    val card = MsgCard("assistant", List(TextContent(s"response id: $FakeMsgId done")), Nil, rawIdx = 0)
    val out  = renderCards(List(card))
    Result.all(
      List(
        Result
          .assert(out.contains("class=\"jt-corr-mask\""))
          .log(s"jt-corr-mask span missing: $out"),
        Result
          .assert(out.contains("data-corr-id=\"corr:m."))
          .log(s"messages-namespace corr id missing: $out"),
      )
    )
  }

  def testCorrMaskNoLeakInTextContent: Result = {
    val card = MsgCard("assistant", List(TextContent(s"id=$FakeMsgId")), Nil, rawIdx = 0)
    val out  = renderCards(List(card))
    Result
      .assert(!out.contains(FakeMsgId))
      .log(s"raw id leaked: $out")
  }

  def testCorrIdNamespace: Result = {
    // Card at rawIdx = 5, first text content. msg_… at offset 0.
    val card = MsgCard(
      role = "assistant",
      contents = List(TextContent(FakeMsgId)),
      userParts = Nil,
      rawIdx = 5,
    )
    val out  = renderCards(List(card))
    Result
      .assert(out.contains("data-corr-id=\"corr:m.5.text.0#0\""))
      .log(s"expected `data-corr-id=\"corr:m.5.text.0#0\"` in: $out")
  }
}
