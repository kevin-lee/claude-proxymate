package claudeproxymate.renderer.filter

import cats.syntax.all.*
import claudeproxymate.core.HtmlIds
import claudeproxymate.core.filter.*
import scalatags.Text.all.*

/** Every string the Request Filter sheet shows, resolved by the DOM sibling
  * from `I18n.t` so this view stays pure and unit-testable.
  */
final case class SheetLabels(
  title: String,
  sub: String,
  enabled: String,
  sectionRules: String,
  sectionDocs: String,
  sectionSkills: String,
  sectionText: String,
  found: String,
  noneFound: String,
  modeKeepAll: String,
  modeRemoveSelected: String,
  modeRemoveAll: String,
  addRule: String,
  kindText: String,
  kindRegex: String,
  kindTag: String,
  scopeTyped: String,
  scopeAssistant: String,
  scopeBoth: String,
  patternPlaceholder: String,
  regexPlaceholder: String,
  tagPlaceholder: String,
  hits: String,
  sourceDisk: String,
  sourceTraffic: String,
  sourceAbsent: String,
  absentNote: String,
  hintRegex: String,
  hintCache: String,
  skipped: String,
  preview: String,
  previewNone: String,
  cancel: String,
  save: String,
)

/** What the sheet renders: the draft config, the merged inventory per
  * category, and the preview computed against the last captured request.
  */
final case class SheetModel(
  draft: FilterConfig,
  rules: List[MergedItem],
  docs: List[MergedItem],
  skills: List[MergedItem],
  preview: Option[(Int, Int)],
  hitsByPattern: Map[String, Int],
  skipped: List[String],
  labels: SheetLabels,
)

/** Pure Scalatags for the Request Filter sheet (Option A of the design canvas). */
object RequestFilterView {

  val ActionAttr: String = "data-filter-action"
  val CatAttr: String    = "data-filter-cat"
  val KeyAttr: String    = "data-filter-key"
  val IdxAttr: String    = "data-filter-idx"
  val ValueAttr: String  = "data-filter-value"

  object Action {
    val Enabled: String     = "enabled"
    val Mode: String        = "mode"
    val ToggleItem: String  = "toggle-item"
    val AddRule: String     = "add-rule"
    val RemoveRule: String  = "remove-rule"
    val RuleKind: String    = "rule-kind"
    val RuleScope: String   = "rule-scope"
    val RuleEnabled: String = "rule-enabled"
    val RulePattern: String = "rule-pattern"
    val Save: String        = "save"
    val Cancel: String      = "cancel"
  }

  private val action = attr(ActionAttr)
  private val catA   = attr(CatAttr)
  private val keyA   = attr(KeyAttr)
  private val idxA   = attr(IdxAttr)
  private val valueA = attr(ValueAttr)

  /** Same funnel as the address-bar button (`IndexHtmlGenerator.FunnelSvg`). */
  val FunnelSvg: String =
    """<svg viewBox="0 0 16 16" width="16" height="16" fill="none" stroke="currentColor" stroke-width="1.6" """ +
      """stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M2 3h12L9.5 8.5V13l-3 1.2V8.5z"/></svg>"""

  /** `(active rule count, show the button as active)`. */
  def buttonState(config: FilterConfig): (Int, Boolean) = {
    val count = config.activeRuleCount
    (count, config.enabled && count > 0)
  }

  def formatInt(n: Int): String = {
    val digits = n.abs.toString.reverse.grouped(3).mkString(",").reverse
    if (n < 0) s"-$digits" else digits
  }

  def buildPreviewText(labels: SheetLabels, preview: Option[(Int, Int)]): String =
    preview match {
      case None => labels.previewNone
      case Some((from, to)) =>
        val drop = if (from > 0) ((from - to).toLong * 100 / from).toInt else 0
        val pct  = if (drop > 0) s"-$drop%" else "0%"
        labels
          .preview
          .replace("{from}", formatInt(from))
          .replace("{to}", formatInt(to))
          .replace("{pct}", pct)
    }

  def buildCardFrag(model: SheetModel): Frag = {
    val l = model.labels
    frag(
      div(cls := "filter-head")(
        div(cls := "filter-title")(raw(FunnelSvg), span(l.title)),
        tag("label")(cls := "sb-item filter-enabled")(
          span(l.enabled),
          switchButton(model.draft.enabled, action := Action.Enabled),
        ),
      ),
      div(cls := "filter-sub")(l.sub),
      categorySection(model, FilterCategory.Rules, l.sectionRules, model.rules),
      categorySection(model, FilterCategory.Docs, l.sectionDocs, model.docs),
      categorySection(model, FilterCategory.Skills, l.sectionSkills, model.skills),
      textRulesSection(model),
      div(cls := "filter-hint")(l.hintRegex),
      div(cls := "filter-hint")(l.hintCache),
      if (model.skipped.isEmpty) frag()
      else
        div(cls := "filter-skipped")(
          div(cls := "filter-hint filter-skipped-title")(l.skipped),
          ul(model.skipped.map(s => li(cls := "filter-hint")(s))),
        ),
      div(cls := "filter-foot")(
        span(id := HtmlIds.RequestFilterPreview, cls := "filter-preview")(buildPreviewText(l, model.preview)),
        span(cls := "filter-foot-actions")(
          button(cls := "filter-btn-cancel", action := Action.Cancel)(l.cancel),
          button(cls := "filter-btn-save", action := Action.Save)(l.save),
        ),
      ),
    )
  }

  private def switchButton(on: Boolean, mods: Modifier*): Frag =
    button(
      cls := (if (on) "switch on" else "switch"),
      attr("role") := "switch",
      attr("aria-checked") := on.toString,
      valueA := (if (on) "on" else "off"),
      mods,
    )(span(cls := "knob"))

  private def categorySection(model: SheetModel, cat: FilterCategory, title: String, items: List[MergedItem]): Frag = {
    val l          = model.labels
    val filter     = model.draft.category(cat)
    val selectable = filter.mode === CategoryMode.RemoveSelected
    val anyAbsent  = items.exists(_.absent)

    def modeBtn(mode: CategoryMode, label: String): Frag =
      button(
        cls := (if (filter.mode === mode) "seg-btn active" else "seg-btn"),
        action := Action.Mode,
        catA := cat.wire,
        valueA := mode.wire,
        attr("aria-pressed") := (filter.mode === mode).toString,
      )(label)

    def row(item: MergedItem): Frag = {
      val isChecked          = filter.keys.contains(item.item.key)
      val rowClasses         = if (selectable) "filter-row" else "filter-row disabled"
      val (srcLabel, srcCls) =
        if (item.inTraffic) (l.sourceTraffic, "filter-src")
        else if (item.onDisk) (l.sourceDisk, "filter-src")
        else (l.sourceAbsent, "filter-src absent")
      tag("label")(cls := rowClasses)(
        input(
          tpe := "checkbox",
          action := Action.ToggleItem,
          catA := cat.wire,
          keyA := item.item.key,
          if (isChecked) checked := "checked" else frag(),
          if (selectable) frag() else disabled := "disabled",
        ),
        if (cat === FilterCategory.Skills) span(cls := "filter-skill-chip")(item.item.label)
        else span(cls := "msg-badge green")(item.item.label),
        span(cls := srcCls)(srcLabel),
        if (item.item.bytes > 0)
          span(cls := "filter-bytes")(
            s"~${formatInt(claudeproxymate.core.RequestAnatomy.estTokens(item.item.bytes))} tok"
          )
        else frag(),
      )
    }

    div(cls := "filter-section", catA := cat.wire)(
      div(cls := "filter-section-head")(
        span(cls := "filter-section-title")(
          title,
          span(cls := "filter-section-count")(l.found.replace("{count}", items.length.toString)),
        ),
        div(cls := "seg")(
          modeBtn(CategoryMode.KeepAll, l.modeKeepAll),
          modeBtn(CategoryMode.RemoveSelected, l.modeRemoveSelected),
          modeBtn(CategoryMode.RemoveAll, l.modeRemoveAll),
        ),
      ),
      if (items.isEmpty) div(cls := "filter-hint")(l.noneFound)
      else
        div(cls := (if (cat === FilterCategory.Skills) "filter-rows filter-chips" else "filter-rows"))(items.map(row)),
      if (anyAbsent) div(cls := "filter-hint")(l.absentNote) else frag(),
    )
  }

  private def textRulesSection(model: SheetModel): Frag = {
    val l     = model.labels
    val rules = model.draft.textRules

    def kindBtn(idx: Int, rule: TextRule, kind: TextRuleKind, label: String): Frag =
      button(
        cls := (if (rule.kind === kind) "seg-btn active" else "seg-btn"),
        action := Action.RuleKind,
        idxA := idx.toString,
        valueA := kind.wire,
      )(label)

    def scopeBtn(idx: Int, rule: TextRule, scope: TextRuleScope, label: String): Frag =
      button(
        cls := (if (rule.scope === scope) "seg-btn active" else "seg-btn"),
        action := Action.RuleScope,
        idxA := idx.toString,
        valueA := scope.wire,
      )(label)

    def ruleRow(rule: TextRule, idx: Int): Frag = {
      val placeholderText = rule.kind match {
        case TextRuleKind.Text => l.patternPlaceholder
        case TextRuleKind.Regex => l.regexPlaceholder
        case TextRuleKind.Tag => l.tagPlaceholder
      }
      val hits            = model.hitsByPattern.getOrElse(rule.pattern, 0)
      div(cls := (if (rule.enabled) "filter-rule-row" else "filter-rule-row disabled"))(
        div(cls := "seg")(
          kindBtn(idx, rule, TextRuleKind.Text, l.kindText),
          kindBtn(idx, rule, TextRuleKind.Regex, l.kindRegex),
          kindBtn(idx, rule, TextRuleKind.Tag, l.kindTag),
        ),
        input(
          tpe := "text",
          cls := "filter-pattern",
          action := Action.RulePattern,
          idxA := idx.toString,
          value := rule.pattern,
          placeholder := placeholderText,
          spellcheck := "false",
        ),
        div(cls := "seg")(
          scopeBtn(idx, rule, TextRuleScope.UserTyped, l.scopeTyped),
          scopeBtn(idx, rule, TextRuleScope.Assistant, l.scopeAssistant),
          scopeBtn(idx, rule, TextRuleScope.Both, l.scopeBoth),
        ),
        span(cls := "filter-hits")(l.hits.replace("{count}", hits.toString)),
        switchButton(rule.enabled, action := Action.RuleEnabled, idxA := idx.toString),
        button(cls := "x-btn", action := Action.RemoveRule, idxA := idx.toString, attr("aria-label") := "remove")("✕"),
      )
    }

    div(cls := "filter-section", catA := "text")(
      div(cls := "filter-section-head")(
        span(cls := "filter-section-title")(
          l.sectionText,
          span(cls := "filter-section-count")(rules.length.toString),
        ),
        button(cls := "copy-small", action := Action.AddRule, valueA := TextRuleKind.Text.wire)(s"+ ${l.addRule}"),
      ),
      if (rules.isEmpty) frag()
      else div(cls := "filter-rows")(rules.zipWithIndex.map { case (r, i) => ruleRow(r, i) }),
    )
  }
}
