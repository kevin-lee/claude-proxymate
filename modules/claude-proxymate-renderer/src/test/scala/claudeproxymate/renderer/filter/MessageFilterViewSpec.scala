package claudeproxymate.renderer.filter

import claudeproxymate.core.HtmlIds
import claudeproxymate.core.filter.*
import claudeproxymate.renderer.messages.{BadgeFilter, BadgeState}
import hedgehog.*
import hedgehog.runner.*

object MessageFilterViewSpec extends Properties {

  override def tests: List[Test] = List(
    example("live item menu: remove (primary), remove-all and expand carry their attrs", testMenuLive),
    example("removed-selected item menu offers restore", testMenuRestore),
    example("removed-all item menu disables remove and marks remove-all active", testMenuRemovedAll),
    example("skills menu has no per-item remove and shows collapse when expanded", testMenuSkills),
    property("tagNameOf accepts exactly one tag element", testTagNameValid),
    example("tagNameOf rejects partial, mismatched and invalid tags", testTagNameInvalid),
    example("selection menu shows Tag only with a tag name and marks the active kind", testSelectionMenu),
    example("ruleFor maps the kinds", testRuleFor),
    example("bar formats rule count, removed tokens and percentage", testBarRemoved),
    example("bar reads nothing removed without a report and off when disabled", testBarNoneAndOff),
    property("<script> in key, tag name or labels never leaks raw", testNoScriptLeak),
  )

  private val menuLabels = MenuLabels("Remove", "Keep", "All rules", "All docs", "All skills", "Expand", "Collapse")
  private val selLabels  = SelectionLabels("Add rule", "Text", "Regex", "Tag {name}", "Add")
  private val barLabels  = BarLabels(
    "Request filter",
    "{count} rules",
    "removed {tokens} tokens ({pct})",
    "nothing",
    "off",
    "+ Text rule",
    "Manage"
  )

  private val A                         = MessageFilterView.ActionAttr
  private def action(a: String): String = s"""$A="$a""""

  private def menu(filter: BadgeFilter, state: BadgeState, expanded: Boolean = false): String =
    MessageFilterView.buildBadgeMenuFrag(BadgeMenuModel("b1", filter, state, expanded, menuLabels)).render

  def testMenuLive: Result = {
    val html = menu(BadgeFilter.Item(FilterCategory.Rules, "/r/a.md"), BadgeState.Live)
    Result.all(
      List(
        Result.assert(html.contains(s"""id="${HtmlIds.BadgeMenu}"""")).log("id"),
        Result.assert(html.contains(action(MessageFilterView.Action.BadgeRemove))).log("remove action"),
        Result.assert(html.contains("badge-menu-item primary")).log("primary"),
        Result.assert(html.contains(s"""${RequestFilterView.KeyAttr}="/r/a.md"""")).log("key"),
        Result.assert(html.contains(s"""${RequestFilterView.CatAttr}="rules"""")).log("cat"),
        Result
          .assert(html.contains(action(MessageFilterView.Action.BadgeRemoveAll)) && html.contains("All rules"))
          .log("remove all"),
        Result
          .assert(
            html.contains(action(MessageFilterView.Action.BadgeExpand)) && html.contains(
              s"""${RequestFilterView.ValueAttr}="b1""""
            )
          )
          .log("expand"),
        Result.assert(html.contains(">Expand<") && !html.contains("Collapse")).log("expand label"),
        Result.assert(!html.contains("disabled")).log("nothing disabled"),
      )
    )
  }

  def testMenuRestore: Result = {
    val html = menu(BadgeFilter.Item(FilterCategory.Docs, "/d"), BadgeState.RemovedSelected)
    Result.all(
      List(
        Result
          .assert(html.contains(action(MessageFilterView.Action.BadgeRestore)) && html.contains("Keep"))
          .log("restore"),
        Result.assert(!html.contains(action(MessageFilterView.Action.BadgeRemove))).log("no remove"),
        Result.assert(html.contains("All docs")).log("docs label"),
      )
    )
  }

  def testMenuRemovedAll: Result = {
    val html          = menu(BadgeFilter.Item(FilterCategory.Docs, "/d"), BadgeState.RemovedAll)
    val disabledCount = html.split("disabled=\"disabled\"", -1).length - 1
    Result.all(
      List(
        (disabledCount ==== 2).log(html),
        Result.assert(html.contains("badge-menu-item active")).log("active"),
      )
    )
  }

  def testMenuSkills: Result = {
    val html = menu(BadgeFilter.SkillsReminder, BadgeState.Live, expanded = true)
    Result.all(
      List(
        Result.assert(!html.contains(action(MessageFilterView.Action.BadgeRemove))).log("no per-item remove"),
        Result
          .assert(html.contains("All skills") && html.contains(s"""${RequestFilterView.CatAttr}="skills""""))
          .log("remove all skills"),
        Result.assert(html.contains(">Collapse<")).log("collapse"),
      )
    )
  }

  private val genName: Gen[String] =
    for {
      h <- Gen.alpha
      t <- Gen.string(Gen.frequency1(9 -> Gen.alphaNum, 1 -> Gen.element1('-', '_', '.', ':')), Range.linear(0, 8))
    } yield s"$h$t"

  def testTagNameValid: Property =
    for {
      name  <- genName.log("name")
      inner <- Gen.string(Gen.unicode.filter(c => c != '<' && c != '>'), Range.linear(0, 20)).log("inner")
    } yield Result.all(
      List(
        (MessageFilterView.tagNameOf(s"<$name>$inner</$name>") ==== Some(name)).log("plain"),
        (MessageFilterView.tagNameOf(s"  <$name a=\"b\">$inner</$name>\n") ==== Some(name)).log("attrs + whitespace"),
      )
    )

  def testTagNameInvalid: Result =
    Result.all(
      List(
        (MessageFilterView.tagNameOf("<n>x") ==== None).log("open only"),
        (MessageFilterView.tagNameOf("x</n>") ==== None).log("close only"),
        (MessageFilterView.tagNameOf("<n>x</m>") ==== None).log("mismatch"),
        (MessageFilterView.tagNameOf("<1n>x</1n>") ==== None).log("invalid name"),
        (MessageFilterView.tagNameOf("plain text") ==== None).log("plain"),
        (MessageFilterView.tagNameOf("</n>") ==== None).log("closing only"),
      )
    )

  def testSelectionMenu: Result = {
    val withTag =
      MessageFilterView.buildSelectionMenuFrag(SelectionMenuModel(TextRuleKind.Tag, Some("my-tag"), selLabels)).render
    val noTag = MessageFilterView.buildSelectionMenuFrag(SelectionMenuModel(TextRuleKind.Regex, None, selLabels)).render
    Result.all(
      List(
        Result.assert(withTag.contains("Tag &lt;my-tag&gt;")).log(withTag),
        Result
          .assert(withTag.contains(s"""seg-btn active" $A="${MessageFilterView
              .Action
              .SelKind}" ${RequestFilterView.ValueAttr}="tag""""))
          .log(withTag),
        Result.assert(!noTag.contains("Tag ")).log(noTag),
        Result
          .assert(noTag.contains(s"""seg-btn active" $A="${MessageFilterView
              .Action
              .SelKind}" ${RequestFilterView.ValueAttr}="regex""""))
          .log(noTag),
        Result
          .assert(
            noTag.contains(action(MessageFilterView.Action.SelAdd)) && noTag.contains(
              s"""id="${HtmlIds.SelectionMenu}""""
            )
          )
          .log("add"),
      )
    )
  }

  def testRuleFor: Result =
    Result.all(
      List(
        (MessageFilterView.ruleFor(TextRuleKind.Text, "\nhello world\n", None) ==== Some(
          (TextRuleKind.Text, "hello world")
        )).log("text"),
        (MessageFilterView.ruleFor(TextRuleKind.Regex, "a+b", None) ==== Some((TextRuleKind.Regex, "a+b")))
          .log("regex"),
        (MessageFilterView.ruleFor(TextRuleKind.Tag, "<t>x</t>", Some("t")) ==== Some((TextRuleKind.Tag, "t")))
          .log("tag"),
        (MessageFilterView.ruleFor(TextRuleKind.Tag, "x", None) ==== None).log("tag without name"),
        (MessageFilterView.ruleFor(TextRuleKind.Text, "  \n", None) ==== None).log("blank"),
      )
    )

  def testBarRemoved: Result = {
    val html = MessageFilterView.buildBarFrag(BarModel(enabled = true, 4, Some((12480, 9130)), barLabels)).render
    Result.all(
      List(
        Result.assert(html.contains("<strong>4 rules</strong>")).log(html),
        Result.assert(html.contains("removed 3,350 tokens (")).log(html),
        Result.assert(html.contains("""<span class="msg-filter-bar-pct">-26%</span>""")).log(html),
        Result
          .assert(
            html.contains("""aria-checked="true"""") && html.contains(action(MessageFilterView.Action.BarEnabled))
          )
          .log("switch"),
        Result
          .assert(
            html.contains(action(MessageFilterView.Action.BarAddRule)) && html.contains(
              action(MessageFilterView.Action.BarManage)
            )
          )
          .log("actions"),
        Result.assert(html.contains(s"""id="${HtmlIds.MsgFilterBar}" class="msg-filter-bar"""")).log("id/class"),
      )
    )
  }

  def testBarNoneAndOff: Result = {
    val none = MessageFilterView.buildBarFrag(BarModel(enabled = true, 0, None, barLabels)).render
    val same = MessageFilterView.buildBarFrag(BarModel(enabled = true, 1, Some((100, 100)), barLabels)).render
    val off  = MessageFilterView.buildBarFrag(BarModel(enabled = false, 2, Some((100, 50)), barLabels)).render
    Result.all(
      List(
        Result.assert(none.contains("nothing")).log(none),
        Result.assert(same.contains("nothing")).log(same),
        Result
          .assert(off.contains("msg-filter-bar off") && off.contains("off</span>") && !off.contains("removed"))
          .log(off),
        Result.assert(off.contains("""aria-checked="false"""")).log(off),
      )
    )
  }

  def testNoScriptLeak: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val labels  = MenuLabels(payload, payload, payload, payload, payload, payload, payload)
      val m       = MessageFilterView
        .buildBadgeMenuFrag(
          BadgeMenuModel(payload, BadgeFilter.Item(FilterCategory.Rules, payload), BadgeState.Live, false, labels)
        )
        .render
      val s       = MessageFilterView
        .buildSelectionMenuFrag(SelectionMenuModel(TextRuleKind.Tag, Some(payload), selLabels.copy(title = payload)))
        .render
      val b       = MessageFilterView
        .buildBarFrag(BarModel(true, 1, None, barLabels.copy(title = payload, removedNone = payload)))
        .render
      Result.assert(!m.contains("<script>") && !s.contains("<script>") && !b.contains("<script>")).log(m + s + b)
    }
}
