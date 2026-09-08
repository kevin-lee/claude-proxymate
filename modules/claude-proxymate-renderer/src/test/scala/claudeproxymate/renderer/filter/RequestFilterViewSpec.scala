package claudeproxymate.renderer.filter

import claudeproxymate.core.HtmlIds
import claudeproxymate.core.filter.*
import hedgehog.*
import hedgehog.runner.*

object RequestFilterViewSpec extends Properties {

  override def tests: List[Test] = List(
    example("card renders the three category sections and the text-rules section", testSections),
    example("mode buttons carry action / cat / value attrs and mark the active mode", testModeButtons),
    example("checkbox rows reflect the draft keys and are disabled unless Remove selected", testCheckboxRows),
    example("source tags: seen / on disk / not found, with the absent note", testSourceTags),
    example("text-rule rows carry index, pattern, kind, scope, hits and remove/enable controls", testTextRuleRows),
    example("preview text formats tokens and the percentage", testPreviewText),
    example("preview text with no drop reads 0%", testPreviewNoDrop),
    example("preview text without a capture shows the placeholder", testPreviewNone),
    example("buttonState counts active rules and is inactive when filtering is off", testButtonState),
    example("footer carries the preview id and save / cancel actions", testFooter),
    property("<script> in labels, keys or patterns never leaks raw", testNoScriptLeak),
  )

  private val labels: SheetLabels = SheetLabels(
    title = "Request Filter",
    sub = "sub",
    enabled = "Filtering",
    sectionRules = "Rules",
    sectionDocs = "Docs",
    sectionSkills = "Skills",
    sectionText = "Text rules",
    found = "{count} found",
    noneFound = "none yet",
    modeKeepAll = "Keep all",
    modeRemoveSelected = "Remove selected",
    modeRemoveAll = "Remove all",
    addRule = "Add rule",
    kindText = "Text",
    kindRegex = "Regex",
    kindTag = "Tag",
    scopeTyped = "Typed",
    scopeAssistant = "Claude",
    scopeBoth = "Both",
    patternPlaceholder = "text ph",
    regexPlaceholder = "regex ph",
    tagPlaceholder = "tag ph",
    hits = "{count} hits",
    sourceDisk = "on disk",
    sourceTraffic = "seen",
    sourceAbsent = "not found",
    absentNote = "absent note",
    hintRegex = "regex hint",
    hintCache = "cache hint",
    skipped = "Skipped rules",
    preview = "Last request: {from} → {to} tokens ({pct})",
    previewNone = "no capture yet",
    cancel = "Cancel",
    save = "Save",
  )

  private val ruleKey  = "/u/.claude/rules/Simple-Global-Rules.md"
  private val goneKey  = "/old/.claude/rules/Gone.md"
  private val ruleItem = MergedItem(
    InventoryItem(FilterCategory.Rules, ruleKey, "📜 Global Rule: Simple-Global-Rules.md", 1400),
    onDisk = true,
    inTraffic = true
  )
  private val goneItem =
    MergedItem(InventoryItem(FilterCategory.Rules, goneKey, "Gone.md", 0), onDisk = false, inTraffic = false)
  private val docItem  = MergedItem(
    InventoryItem(FilterCategory.Docs, "/u/.claude/CLAUDE.md", "CLAUDE.md", 0),
    onDisk = true,
    inTraffic = false
  )
  private val skill    =
    MergedItem(InventoryItem(FilterCategory.Skills, "graphify", "🔧 graphify", 300), onDisk = true, inTraffic = true)

  private val draft: FilterConfig = FilterConfig
    .default
    .copy(
      rules = CategoryFilter(CategoryMode.RemoveSelected, List(ruleKey, goneKey)),
      docs = CategoryFilter(CategoryMode.KeepAll, Nil),
      skills = CategoryFilter(CategoryMode.RemoveAll, Nil),
      textRules = List(
        TextRule(TextRuleKind.Tag, "my-tag", TextRuleScope.Both, enabled = true),
        TextRule(TextRuleKind.Regex, "[0-9]+", TextRuleScope.Assistant, enabled = false),
      ),
    )

  private def model(
    d: FilterConfig = draft,
    rules: List[MergedItem] = List(ruleItem, goneItem),
    docs: List[MergedItem] = List(docItem),
    skills: List[MergedItem] = List(skill),
    preview: Option[(Int, Int)] = Some((12480, 9130)),
  ): SheetModel =
    SheetModel(d, rules, docs, skills, preview, Map("my-tag" -> 2), Nil, labels)

  private def render(m: SheetModel): String = RequestFilterView.buildCardFrag(m).render

  def testSections: Result = {
    val html = render(model())
    Result.all(
      List(
        Result.assert(html.contains(s"""${RequestFilterView.CatAttr}="rules"""")).log("rules section"),
        Result.assert(html.contains(s"""${RequestFilterView.CatAttr}="docs"""")).log("docs section"),
        Result.assert(html.contains(s"""${RequestFilterView.CatAttr}="skills"""")).log("skills section"),
        Result.assert(html.contains(s"""${RequestFilterView.CatAttr}="text"""")).log("text section"),
        Result.assert(html.contains("2 found")).log("rules count"),
        Result.assert(html.contains("regex hint") && html.contains("cache hint")).log("hints"),
        Result
          .assert(html.contains(s"""${RequestFilterView.ActionAttr}="${RequestFilterView.Action.Enabled}""""))
          .log("enabled switch"),
      )
    )
  }

  def testModeButtons: Result = {
    val html   = render(model())
    val active =
      s"""class="seg-btn active" ${RequestFilterView.ActionAttr}="mode" ${RequestFilterView.CatAttr}="rules" ${RequestFilterView.ValueAttr}="removeSelected""""
    val idle   =
      s"""class="seg-btn" ${RequestFilterView.ActionAttr}="mode" ${RequestFilterView.CatAttr}="rules" ${RequestFilterView.ValueAttr}="keepAll""""
    Result.all(
      List(
        Result.assert(html.contains(active)).log(s"active mode button missing: $html"),
        Result.assert(html.contains(idle)).log(s"idle mode button missing: $html"),
        Result
          .assert(
            html.contains(
              s"""${RequestFilterView.CatAttr}="skills" ${RequestFilterView.ValueAttr}="removeAll" aria-pressed="true""""
            )
          )
          .log("skills remove-all active"),
      )
    )
  }

  def testCheckboxRows: Result = {
    val html = render(model())
    Result.all(
      List(
        Result
          .assert(
            html.contains(
              s"""${RequestFilterView.ActionAttr}="toggle-item" ${RequestFilterView.CatAttr}="rules" ${RequestFilterView.KeyAttr}="$ruleKey" checked="checked""""
            )
          )
          .log(s"selected rule should be checked: $html"),
        Result
          .assert(html.contains(s"""${RequestFilterView.KeyAttr}="/u/.claude/CLAUDE.md" disabled="disabled""""))
          .log("docs (keep all) rows should be disabled"),
        Result.assert(html.contains("""class="filter-row disabled"""")).log("disabled row class"),
        Result.assert(html.contains("filter-skill-chip")).log("skills render as chips"),
        Result.assert(html.contains("~400 tok")).log("rule bytes as tokens"),
      )
    )
  }

  def testSourceTags: Result = {
    val html = render(model())
    Result.all(
      List(
        Result.assert(html.contains("""<span class="filter-src">seen</span>""")).log("traffic tag"),
        Result.assert(html.contains("""<span class="filter-src">on disk</span>""")).log("disk tag"),
        Result.assert(html.contains("""<span class="filter-src absent">not found</span>""")).log("absent tag"),
        Result.assert(html.contains("absent note")).log("absent note shown"),
        Result
          .assert(!render(model(rules = List(ruleItem))).contains("absent note"))
          .log("no note without absent items"),
      )
    )
  }

  def testTextRuleRows: Result = {
    val html = render(model())
    Result.all(
      List(
        Result
          .assert(
            html.contains(
              s"""${RequestFilterView.ActionAttr}="rule-pattern" ${RequestFilterView.IdxAttr}="0" value="my-tag" placeholder="tag ph""""
            )
          )
          .log(s"pattern input: $html"),
        Result
          .assert(
            html.contains(
              s"""${RequestFilterView.ActionAttr}="rule-kind" ${RequestFilterView.IdxAttr}="0" ${RequestFilterView.ValueAttr}="tag""""
            )
          )
          .log("kind button"),
        Result
          .assert(
            html.contains(
              s"""${RequestFilterView.ActionAttr}="rule-scope" ${RequestFilterView.IdxAttr}="1" ${RequestFilterView.ValueAttr}="assistant""""
            )
          )
          .log("scope button"),
        Result.assert(html.contains("2 hits")).log("hits"),
        Result
          .assert(html.contains(s"""${RequestFilterView.ActionAttr}="remove-rule" ${RequestFilterView.IdxAttr}="1""""))
          .log("remove"),
        Result
          .assert(html.contains(s"""${RequestFilterView.ActionAttr}="rule-enabled" ${RequestFilterView.IdxAttr}="1""""))
          .log("enable switch"),
        Result.assert(html.contains("""class="filter-rule-row disabled"""")).log("disabled rule row"),
        Result.assert(html.contains(s"""${RequestFilterView.ActionAttr}="add-rule"""")).log("add rule"),
      )
    )
  }

  def testPreviewText: Result =
    (RequestFilterView.buildPreviewText(labels, Some((12480, 9130))) ==== "Last request: 12,480 → 9,130 tokens (-26%)")
      .log("preview text")

  def testPreviewNoDrop: Result =
    (RequestFilterView.buildPreviewText(labels, Some((251, 251))) ==== "Last request: 251 → 251 tokens (0%)")
      .log("no drop should read 0%, never -0%")

  def testPreviewNone: Result =
    (RequestFilterView.buildPreviewText(labels, None) ==== "no capture yet").log("placeholder")

  def testButtonState: Result =
    Result.all(
      List(
        (RequestFilterView.buttonState(draft) ==== ((4, true))).log("2 keys + remove-all + 1 enabled rule"),
        (RequestFilterView.buttonState(draft.copy(enabled = false)) ==== ((4, false))).log("disabled keeps the count"),
        (RequestFilterView.buttonState(FilterConfig.default) ==== ((0, false))).log("default"),
      )
    )

  def testFooter: Result = {
    val html = render(model())
    Result.all(
      List(
        Result.assert(html.contains(s"""id="${HtmlIds.RequestFilterPreview}"""")).log("preview id"),
        Result.assert(html.contains("12,480 → 9,130")).log("preview text in footer"),
        Result.assert(html.contains(s"""${RequestFilterView.ActionAttr}="save"""")).log("save"),
        Result.assert(html.contains(s"""${RequestFilterView.ActionAttr}="cancel"""")).log("cancel"),
      )
    )
  }

  def testNoScriptLeak: Property =
    for {
      evil <- Gen.string(Gen.alpha, Range.linear(0, 8)).log("evil")
    } yield {
      val payload = s"<script>alert('$evil')</script>"
      val d    = draft.copy(textRules = List(TextRule(TextRuleKind.Text, payload, TextRuleScope.Both, enabled = true)))
      val item = MergedItem(InventoryItem(FilterCategory.Rules, payload, payload, 10), onDisk = false, inTraffic = true)
      val html = render(model(d = d, rules = List(item)))
      Result.assert(!html.contains("<script>")).log(s"raw <script> leaked: $html")
    }
}
