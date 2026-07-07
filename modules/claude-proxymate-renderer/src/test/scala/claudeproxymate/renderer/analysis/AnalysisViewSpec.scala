package claudeproxymate.renderer.analysis

import hedgehog.*
import hedgehog.runner.*

object AnalysisViewSpec extends Properties {

  override def tests: List[Test] = List(
    // Empty / no-mechanisms
    example("buildEmptyFrag contains analysis-none class", testEmptyClass),
    example("buildEmptyFrag contains title and small(hint)", testEmptyTitleAndHint),
    example("buildFrag with data=None has no analysis-section", testEmptyNoSections),
    // Search bar
    example("buildSearchBarFrag contains msg-search-input + id", testSearchInput),
    example("buildSearchBarFrag with nonempty query renders clear button", testSearchClearVisible),
    example("buildSearchBarFrag with empty query has no clear button", testSearchClearHidden),
    example("buildSearchBarFrag carries placeholder + value", testSearchAttrs),
    // Mech chips passthrough
    example("buildFrag renders mechChipsHtml raw", testMechChipsPassthrough),
    // Sections — model + CLAUDE.md
    example("modelName Some renders analysis-kv with model key", testModelKv),
    example("claudeMd Right(parsed) renders cm + cm_<i> data-mech-keys", testClaudeMdParsed),
    example("claudeMd Left(raw) renders cm with analysis-block", testClaudeMdRaw),
    // Sections — slash + skills
    example("each slash command carries data-mech-key sc_<i>", testSlashCommandKey),
    example("skill renders jt-json-block with data-json", testSkillJsonBlock),
    example("skill with result renders block; without renders noToolResult", testSkillResult),
    // Sections — sub-agents + MCP
    example("subAgent renders data-mech-key sa", testSubAgentKey),
    example("mcpTool renders data-mech-key mc_<i>", testMcpToolKey),
    example("mcpTool isJson true → jt-json-block; false → plain block", testMcpJsonBranching),
    // XSS regression
    property("<script> in modelName never leaks raw", testNoScriptLeakInModel),
    property("<script> in slash command / skill never leaks raw", testNoScriptLeakInSlashSkill),
    property("<script> in MCP fields never leaks raw", testNoScriptLeakInMcp),
    // Inline-handler regression (search bar deliberately keeps inline handlers
    // until A3h; body of analysis sections must stay handler-free)
    example("buildFrag never contains inline event handlers", testNoInlineHandlers),
    // data-mech-key preservation
    example("data-mech-key present on every mechanism section type", testAllDataMechKeys),
  )

  // ── Helpers ──────────────────────────────────────────────────────────

  private val sampleLabels = AnalysisLabels(
    noMechanismsTitle = "No mechs",
    noMechanismsHint = "Plain request",
    claudeMdTitle = "CMD title",
    claudeMdDesc = "CMD desc",
    outputStyleTitle = "OS title",
    outputStyleDesc = "OS desc",
    slashCmdTitle = "Slash title",
    slashCmdDescs = List("desc-0", "desc-1", "desc-2"),
    skillTitle = "Skill title",
    skillLinkedTitle = "Linked skill title",
    skillDesc = "Skill desc",
    skillLinkedDesc = "Linked skill desc",
    noToolResult = "no tool result",
    subAgentDesc = "Sub agent desc",
    mcpDesc = "MCP desc",
    searchPlaceholder = "search…",
    searchClear = "clear",
  )

  private def render(
    data: Option[AnalysisData],
    query: String = "",
    mechChipsHtml: String = "",
  ): String =
    AnalysisView
      .buildFrag(
        data = data,
        labels = sampleLabels,
        searchQuery = query,
        searchInputId = "proxyDetailSearchInput",
        mechChipsHtml = mechChipsHtml,
      )
      .render

  private def renderSearchBar(query: String): String =
    AnalysisView.buildSearchBarFrag("proxyDetailSearchInput", query, sampleLabels).render

  private def emptyData: AnalysisData =
    AnalysisData(
      modelName = None,
      claudeMd = None,
      outputStyle = None,
      slashCommands = Nil,
      skills = Nil,
      slashSkillLinked = false,
      subAgents = Nil,
      mcpTools = Nil,
    )

  // ── Empty / no-mechanisms ────────────────────────────────────────────

  def testEmptyClass: Result =
    Result
      .assert(AnalysisView.buildEmptyFrag(sampleLabels).render.contains("class=\"analysis-none\""))
      .log(AnalysisView.buildEmptyFrag(sampleLabels).render)

  def testEmptyTitleAndHint: Result = {
    val out = AnalysisView.buildEmptyFrag(sampleLabels).render
    Result.all(
      List(
        Result.assert(out.contains("No mechs")).log("title missing"),
        Result.assert(out.contains("<small>")).log("small wrapper missing"),
        Result.assert(out.contains("Plain request")).log("hint missing"),
      )
    )
  }

  def testEmptyNoSections: Result = {
    val out = render(None)
    Result.assert(!out.contains("analysis-section")).log(out)
  }

  // ── Search bar ───────────────────────────────────────────────────────

  def testSearchInput: Result = {
    val out = renderSearchBar("")
    Result.all(
      List(
        Result.assert(out.contains("class=\"msg-search-input\"")).log("class missing"),
        Result.assert(out.contains("id=\"proxyDetailSearchInput\"")).log("id missing"),
      )
    )
  }

  def testSearchClearVisible: Result = {
    val out = renderSearchBar("foo")
    Result.assert(out.contains("class=\"msg-search-clear\"")).log(out)
  }

  def testSearchClearHidden: Result = {
    val out = renderSearchBar("")
    Result.assert(!out.contains("msg-search-clear")).log(out)
  }

  def testSearchAttrs: Result = {
    val out = renderSearchBar("hello")
    Result.all(
      List(
        Result.assert(out.contains("placeholder=\"search…\"")).log("placeholder missing"),
        Result.assert(out.contains("value=\"hello\"")).log("value missing"),
      )
    )
  }

  // ── Mech chips passthrough ───────────────────────────────────────────

  def testMechChipsPassthrough: Result = {
    val chips = """<span class="mech-chip cm found btn">📋</span>"""
    val out   = render(Some(emptyData), mechChipsHtml = chips)
    Result.assert(out.contains(chips)).log(out)
  }

  // ── Sections — model + CLAUDE.md ─────────────────────────────────────

  def testModelKv: Result = {
    val data = emptyData.copy(modelName = Some("claude-opus-4-7"))
    val out  = render(Some(data))
    Result.all(
      List(
        Result.assert(out.contains("class=\"ak\">model<")).log("ak/model missing"),
        Result.assert(out.contains("claude-opus-4-7")).log("model value missing"),
      )
    )
  }

  def testClaudeMdParsed: Result = {
    val sections = List(
      CmSectionView("L1", "/p1", "c1", "global"),
      CmSectionView("L2", "/p2", "c2", "project"),
    )
    val data     = emptyData.copy(claudeMd = Some(Right(sections)))
    val out      = render(Some(data))
    Result.all(
      List(
        Result.assert(out.contains("data-mech-key=\"cm\"")).log("cm key missing"),
        Result.assert(out.contains("data-mech-key=\"cm_0\"")).log("cm_0 key missing"),
        Result.assert(out.contains("data-mech-key=\"cm_1\"")).log("cm_1 key missing"),
        Result.assert(out.contains("L1")).log("L1 label missing"),
        Result.assert(out.contains("/p1")).log("path missing"),
      )
    )
  }

  def testClaudeMdRaw: Result = {
    val data = emptyData.copy(claudeMd = Some(Left("raw md content")))
    val out  = render(Some(data))
    Result.all(
      List(
        Result.assert(out.contains("data-mech-key=\"cm\"")).log("cm key missing"),
        Result.assert(out.contains("class=\"analysis-block highlight-green\"")).log("analysis-block missing"),
        Result.assert(out.contains("raw md content")).log("raw content missing"),
      )
    )
  }

  // ── Sections — slash + skills ────────────────────────────────────────

  def testSlashCommandKey: Result = {
    val data = emptyData.copy(slashCommands = List(SlashCommandView("foo", "/foo invocation")))
    val out  = render(Some(data))
    Result.all(
      List(
        Result.assert(out.contains("data-mech-key=\"sc_0\"")).log("sc_0 key missing"),
        Result.assert(out.contains("/foo invocation")).log("full text missing"),
      )
    )
  }

  def testSkillJsonBlock: Result = {
    val data = emptyData.copy(skills = List(SkillView("toolu_X", """{"a":1}""", None)))
    val out  = render(Some(data))
    Result.all(
      List(
        Result.assert(out.contains("data-mech-key=\"sk_0\"")).log("sk_0 key missing"),
        Result
          .assert(out.contains(s"class=\"analysis-block highlight-purple ${AnalysisView.JsonBlockClass}\""))
          .log("jt-json-block class missing"),
        Result.assert(out.contains("data-json=")).log("data-json attr missing"),
      )
    )
  }

  def testSkillResult: Result = {
    val withResult    = emptyData.copy(skills = List(SkillView("toolu_X", "{}", Some("RESULT-TEXT"))))
    val withoutResult = emptyData.copy(skills = List(SkillView("toolu_X", "{}", None)))
    val outWith       = render(Some(withResult))
    val outWithout    = render(Some(withoutResult))
    Result.all(
      List(
        Result.assert(outWith.contains("RESULT-TEXT")).log(s"result missing: $outWith"),
        Result.assert(outWithout.contains("no tool result")).log(s"noToolResult label missing: $outWithout"),
      )
    )
  }

  // ── Sections — sub-agents + MCP ──────────────────────────────────────

  def testSubAgentKey: Result = {
    val sa   = SubAgentView("Researcher", "general-purpose", """{"prompt":"x"}""", "x", isJson = true)
    val data = emptyData.copy(subAgents = List(sa))
    val out  = render(Some(data))
    Result.all(
      List(
        Result.assert(out.contains("data-mech-key=\"sa\"")).log("sa key missing"),
        Result.assert(out.contains("Researcher — Sub-Agent")).log("name title missing"),
        Result.assert(out.contains("general-purpose")).log("subagent_type missing"),
      )
    )
  }

  def testMcpToolKey: Result = {
    val mc   = McpToolView("srv", "myserver", "myTool", """{"x":1}""", isJson = true, None)
    val data = emptyData.copy(mcpTools = List(mc))
    val out  = render(Some(data))
    Result.all(
      List(
        Result.assert(out.contains("data-mech-key=\"mc_0\"")).log("mc_0 key missing"),
        Result.assert(out.contains("myTool")).log("toolName missing"),
        Result.assert(out.contains("myserver")).log("serverName missing"),
      )
    )
  }

  def testMcpJsonBranching: Result = {
    val mcJson   = McpToolView("a", "s", "t", """{"k":1}""", isJson = true, None)
    val mcPlain  = McpToolView("b", "s", "t", "plain text", isJson = false, None)
    val outJson  = render(Some(emptyData.copy(mcpTools = List(mcJson))))
    val outPlain = render(Some(emptyData.copy(mcpTools = List(mcPlain))))
    Result.all(
      List(
        Result.assert(outJson.contains(AnalysisView.JsonBlockClass)).log(s"jt-json-block missing: $outJson"),
        Result.assert(!outPlain.contains(AnalysisView.JsonBlockClass)).log(s"unexpected jt-json-block: $outPlain"),
        Result.assert(outPlain.contains("plain text")).log(s"plain text missing: $outPlain"),
      )
    )
  }

  // ── XSS regression ───────────────────────────────────────────────────

  def testNoScriptLeakInModel: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val data    = emptyData.copy(modelName = Some(payload))
      val out     = render(Some(data))
      Result
        .assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakInSlashSkill: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val data    = emptyData.copy(
        slashCommands = List(SlashCommandView(payload, payload)),
        skills = List(SkillView(payload, payload, Some(payload))),
      )
      val out     = render(Some(data))
      Result
        .assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakInMcp: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val mc      = McpToolView(payload, payload, payload, payload, isJson = false, Some(payload))
      val data    = emptyData.copy(mcpTools = List(mc))
      val out     = render(Some(data))
      Result
        .assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  // ── Inline-handler regression ────────────────────────────────────────

  def testNoInlineHandlers: Result = {
    val data = emptyData.copy(
      modelName = Some("m"),
      slashCommands = List(SlashCommandView("foo", "/foo")),
      skills = List(SkillView("id", "{}", Some("r"))),
    )
    val out  = render(Some(data), query = "q")
    Result.all(
      List(
        Result.assert(!out.contains("onclick=")).log(s"unexpected onclick: $out"),
        Result.assert(!out.contains("oninput=")).log(s"unexpected oninput: $out"),
        Result.assert(!out.contains("oncompositionstart=")).log(s"unexpected oncompositionstart: $out"),
        Result.assert(!out.contains("oncompositionend=")).log(s"unexpected oncompositionend: $out"),
      )
    )
  }

  // ── data-mech-key preservation ───────────────────────────────────────

  def testAllDataMechKeys: Result = {
    val data = emptyData.copy(
      claudeMd = Some(Left("md")),
      outputStyle = Some("style content"),
      slashCommands = List(SlashCommandView("foo", "/foo")),
      skills = List(SkillView("id", "{}", None)),
      subAgents = List(SubAgentView("Agent", "type", "{}", "p", isJson = true)),
      mcpTools = List(McpToolView("id", "srv", "tool", "{}", isJson = true, None)),
    )
    val out  = render(Some(data))
    Result.all(
      List(
        Result.assert(out.contains("data-mech-key=\"cm\"")).log("cm missing"),
        Result.assert(out.contains("data-mech-key=\"st\"")).log("st missing"),
        Result.assert(out.contains("data-mech-key=\"sc_0\"")).log("sc_0 missing"),
        Result.assert(out.contains("data-mech-key=\"sk_0\"")).log("sk_0 missing"),
        Result.assert(out.contains("data-mech-key=\"sa\"")).log("sa missing"),
        Result.assert(out.contains("data-mech-key=\"mc_0\"")).log("mc_0 missing"),
      )
    )
  }
}
