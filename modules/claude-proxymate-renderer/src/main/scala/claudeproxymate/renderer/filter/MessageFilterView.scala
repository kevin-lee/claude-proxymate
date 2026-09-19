package claudeproxymate.renderer.filter

import cats.syntax.all.*
import claudeproxymate.core.{HtmlIds, RequestAnatomy}
import claudeproxymate.core.filter.*
import claudeproxymate.renderer.messages.{BadgeFilter, BadgeState, MessageView}
import scalatags.Text.all.*

/** Strings of the badge popover, resolved by the DOM sibling from `I18n.t`. */
final case class MenuLabels(
  remove: String,
  restore: String,
  removeAllRules: String,
  removeAllDocs: String,
  removeAllSkills: String,
  expand: String,
  collapse: String,
)

final case class BadgeMenuModel(
  uid: String,
  filter: BadgeFilter,
  state: BadgeState,
  expanded: Boolean,
  labels: MenuLabels,
)

/** Strings of the selection menu; `kindTag` carries a `{name}` placeholder. */
final case class SelectionLabels(title: String, kindText: String, kindRegex: String, kindTag: String, add: String)

final case class SelectionMenuModel(kind: TextRuleKind, tagName: Option[String], labels: SelectionLabels)

/** Strings of the filter bar; `rules` carries `{count}`, `removed` carries `{tokens}` and `{pct}`. */
final case class BarLabels(
  title: String,
  rules: String,
  removed: String,
  removedNone: String,
  off: String,
  textRule: String,
  manage: String,
)

/** `removed` is `(originalTokens, filteredTokens)` of the selected capture's filter report. */
final case class BarModel(enabled: Boolean, ruleCount: Int, removed: Option[(Int, Int)], labels: BarLabels)

/** Pure Scalatags for the inline filter controls of the Messages tab (Option B
  * of the design canvas): the badge popover, the selection menu and the
  * request filter bar.
  */
object MessageFilterView {

  val ActionAttr: String   = "data-msg-filter-action"
  val MenuClass: String    = "badge-menu"
  val SelMenuClass: String = "sel-menu"
  val BarClass: String     = "msg-filter-bar"

  object Action {
    val BadgeRemove: String    = "badge-remove"
    val BadgeRestore: String   = "badge-restore"
    val BadgeRemoveAll: String = "badge-remove-all"
    val BadgeExpand: String    = "badge-expand"
    val ToggleSkill: String    = "toggle-skill"
    val SelKind: String        = "sel-kind"
    val SelAdd: String         = "sel-add"
    val BarEnabled: String     = "bar-enabled"
    val BarAddRule: String     = "bar-add-rule"
    val BarManage: String      = "bar-manage"
  }

  private val action = attr(ActionAttr)
  private val catA   = attr(RequestFilterView.CatAttr)
  private val keyA   = attr(RequestFilterView.KeyAttr)
  private val valueA = attr(RequestFilterView.ValueAttr)

  // ── badge menu ──

  def buildBadgeMenuFrag(m: BadgeMenuModel): Frag = {
    val l = m.labels

    def item(label: String, extraCls: String, enabled: Boolean, funnel: Boolean, mods: Modifier*): Frag =
      button(
        cls := (if (extraCls.isEmpty) "badge-menu-item" else s"badge-menu-item $extraCls"),
        if (enabled) frag() else disabled := "disabled",
        mods,
      )(if (funnel) raw(RequestFilterView.FunnelSvg) else frag(), label)

    val expandItem =
      item(
        if (m.expanded) l.collapse else l.expand,
        "",
        enabled = true,
        funnel = false,
        action := Action.BadgeExpand,
        valueA := m.uid,
      )

    val items: List[Frag] = m.filter match {
      case BadgeFilter.Item(category, key) =>
        val removeAllLabel = category match {
          case FilterCategory.Rules => l.removeAllRules
          case FilterCategory.Docs => l.removeAllDocs
          case FilterCategory.Skills => l.removeAllSkills
        }
        val first          = m.state match {
          case BadgeState.Live =>
            item(
              l.remove,
              "primary",
              enabled = true,
              funnel = true,
              action := Action.BadgeRemove,
              catA := category.wire,
              keyA := key
            )
          case BadgeState.RemovedSelected =>
            item(
              l.restore,
              "",
              enabled = true,
              funnel = true,
              action := Action.BadgeRestore,
              catA := category.wire,
              keyA := key
            )
          case BadgeState.RemovedAll =>
            item(
              l.remove,
              "",
              enabled = false,
              funnel = true,
              action := Action.BadgeRemove,
              catA := category.wire,
              keyA := key
            )
        }
        val all            = m.state match {
          case BadgeState.RemovedAll =>
            item(
              removeAllLabel,
              "active",
              enabled = false,
              funnel = true,
              action := Action.BadgeRemoveAll,
              catA := category.wire
            )
          case BadgeState.Live | BadgeState.RemovedSelected =>
            item(
              removeAllLabel,
              "",
              enabled = true,
              funnel = true,
              action := Action.BadgeRemoveAll,
              catA := category.wire
            )
        }
        List(first, all)

      case BadgeFilter.SkillsReminder =>
        val all = m.state match {
          case BadgeState.RemovedAll =>
            item(
              l.removeAllSkills,
              "active",
              enabled = false,
              funnel = true,
              action := Action.BadgeRemoveAll,
              catA := FilterCategory.Skills.wire
            )
          case BadgeState.Live | BadgeState.RemovedSelected =>
            item(
              l.removeAllSkills,
              "",
              enabled = true,
              funnel = true,
              action := Action.BadgeRemoveAll,
              catA := FilterCategory.Skills.wire
            )
        }
        List(all)
    }

    div(id := HtmlIds.BadgeMenu, cls := MenuClass, attr(MessageView.BadgeDataAttr) := m.uid)(
      items,
      div(cls := "badge-menu-sep"),
      expandItem,
    )
  }

  // ── selection menu ──

  /** `Some(name)` when `selection` is exactly one `<name …>…</name>` element. */
  def tagNameOf(selection: String): Option[String] = {
    val s = selection.trim
    if (!s.startsWith("<")) none[String]
    else {
      val nameEnd = s.indexWhere(c => c.isWhitespace || c === '>' || c === '/', 1)
      val name    = if (nameEnd < 0) "" else s.substring(1, nameEnd)
      val openEnd = s.indexOf('>')
      val close   = s"</$name>"
      Option.when(
        name.nonEmpty &&
          TextRules.TagNamePattern.matches(name) &&
          openEnd > 0 &&
          s.endsWith(close) &&
          s.length - close.length >= openEnd + 1
      )(name)
    }
  }

  /** The rule the selection menu adds: `None` when nothing usable is selected. */
  def ruleFor(kind: TextRuleKind, selection: String, tagName: Option[String]): Option[(TextRuleKind, String)] = {
    val text = selection.stripPrefix("\n").stripSuffix("\n").dropWhile(_ === '\n').reverse.dropWhile(_ === '\n').reverse
    kind match {
      case TextRuleKind.Text => Option.when(text.trim.nonEmpty)((TextRuleKind.Text, text))
      case TextRuleKind.Regex => Option.when(text.trim.nonEmpty)((TextRuleKind.Regex, text))
      case TextRuleKind.Tag => tagName.map(n => (TextRuleKind.Tag, n))
    }
  }

  def buildSelectionMenuFrag(m: SelectionMenuModel): Frag = {
    val l = m.labels

    def kindBtn(kind: TextRuleKind, label: String): Frag =
      button(
        cls := (if (m.kind === kind) "seg-btn active" else "seg-btn"),
        action := Action.SelKind,
        valueA := kind.wire,
      )(label)

    div(id := HtmlIds.SelectionMenu, cls := SelMenuClass)(
      span(cls := "sel-menu-title")(l.title),
      div(cls := "seg")(
        kindBtn(TextRuleKind.Text, l.kindText),
        kindBtn(TextRuleKind.Regex, l.kindRegex),
        m.tagName.fold(frag())(n => kindBtn(TextRuleKind.Tag, l.kindTag.replace("{name}", s"<$n>"))),
      ),
      button(cls := "sel-menu-add", action := Action.SelAdd)(l.add),
    )
  }

  // ── filter bar ──

  def barRemoved(report: Option[FilterReport]): Option[(Int, Int)] =
    report.map(r => (RequestAnatomy.estTokens(r.originalBytes), RequestAnatomy.estTokens(r.filteredBytes)))

  def buildBarFrag(m: BarModel): Frag = {
    val l          = m.labels
    val rulesText  = l.rules.replace("{count}", RequestFilterView.formatInt(m.ruleCount))
    val stat: Frag =
      if (!m.enabled) frag(strong(rulesText), " · ", l.off)
      else
        m.removed match {
          case Some((from, to)) if from > to =>
            val drop = if (from > 0) ((from - to).toLong * 100 / from).toInt else 0
            val text = l.removed.replace("{tokens}", RequestFilterView.formatInt(from - to))
            val idx  = text.indexOf("{pct}")
            if (idx < 0) frag(strong(rulesText), " · ", text)
            else
              frag(
                strong(rulesText),
                " · ",
                text.substring(0, idx),
                span(cls := "msg-filter-bar-pct")(s"-$drop%"),
                text.substring(idx + "{pct}".length),
              )
          case Some(_) | None => frag(strong(rulesText), " · ", l.removedNone)
        }

    div(id := HtmlIds.MsgFilterBar, cls := (if (m.enabled) BarClass else s"$BarClass off"))(
      span(cls := "msg-filter-bar-title")(raw(RequestFilterView.FunnelSvg), l.title),
      RequestFilterView.switchButton(m.enabled, action := Action.BarEnabled),
      span(cls := "msg-filter-bar-stat")(stat),
      span(cls := "msg-filter-bar-actions")(
        button(cls := "copy-small", action := Action.BarAddRule)(l.textRule),
        button(cls := "copy-small", action := Action.BarManage)(l.manage),
      ),
    )
  }
}
