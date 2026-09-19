package claudeproxymate.core.filter

import hedgehog.*
import hedgehog.runner.*

import FilterConfigEdits.*

object FilterConfigEditsSpec extends Properties {

  override def tests: List[Test] = List(
    example("setEnabled", testSetEnabled),
    example("setMode changes only that category", testSetMode),
    example("toggleKey adds then removes", testToggleKey),
    property("toggleKey twice is identity", testToggleTwice),
    example(
      "removeKey switches KeepAll to RemoveSelected with that key and appends under RemoveSelected",
      testRemoveKey
    ),
    property("removeKey twice equals once", testRemoveKeyTwice),
    example("removeKey under RemoveAll is a no-op", testRemoveKeyRemoveAll),
    example("addRule appends a default rule of that kind", testAddRule),
    example("appendRule appends an enabled rule, a different scope is a second rule", testAppendRule),
    property("appendRule twice equals once", testAppendRuleTwice),
    example("removeRule removes by index, out of range is a no-op", testRemoveRule),
    example("setRuleKind / Scope / Pattern / Enabled update one rule", testSetRuleFields),
    example("rule edits with an out-of-range index are no-ops", testOutOfRange),
  )

  private val base: FilterConfig =
    FilterConfig.default.copy(textRules = List(TextRule.default, TextRule.default.copy(kind = TextRuleKind.Tag)))

  def testSetEnabled: Result =
    (setEnabled(false)(base).enabled ==== false).log("enabled should be false")

  def testSetMode: Result = {
    val out = setMode(FilterCategory.Docs, CategoryMode.RemoveAll)(base)
    Result.all(
      List(
        (out.docs.mode ==== CategoryMode.RemoveAll).log("docs mode"),
        (out.rules ==== base.rules).log("rules untouched"),
        (out.skills ==== base.skills).log("skills untouched"),
      )
    )
  }

  def testToggleKey: Result = {
    val added   = toggleKey(FilterCategory.Skills, "graphify")(base)
    val removed = toggleKey(FilterCategory.Skills, "graphify")(added)
    Result.all(
      List(
        (added.skills.keys ==== List("graphify")).log("added"),
        (removed.skills.keys ==== Nil).log("removed"),
      )
    )
  }

  def testToggleTwice: Property =
    for {
      key <- Gen.string(Gen.alphaNum, Range.linear(1, 10)).log("key")
      cat <- Gen.element1(FilterCategory.Rules, FilterCategory.Docs, FilterCategory.Skills).log("cat")
    } yield {
      val twice = toggleKey(cat, key)(toggleKey(cat, key)(base))
      (twice ==== base).log("toggle twice should be identity")
    }

  def testRemoveKey: Result = {
    val first  = removeKey(FilterCategory.Rules, "a")(base)
    val second = removeKey(FilterCategory.Rules, "b")(first)
    Result.all(
      List(
        (first.rules ==== CategoryFilter(CategoryMode.RemoveSelected, List("a"))).log("from KeepAll"),
        (second.rules ==== CategoryFilter(CategoryMode.RemoveSelected, List("a", "b"))).log("appended"),
        (first.docs ==== base.docs).log("docs untouched"),
      )
    )
  }

  def testRemoveKeyTwice: Property =
    for {
      key <- Gen.string(Gen.alphaNum, Range.linear(1, 10)).log("key")
      cat <- Gen.element1(FilterCategory.Rules, FilterCategory.Docs, FilterCategory.Skills).log("cat")
    } yield {
      val once = removeKey(cat, key)(base)
      (removeKey(cat, key)(once) ==== once).log("removeKey should be idempotent")
    }

  def testRemoveKeyRemoveAll: Result = {
    val all = setMode(FilterCategory.Skills, CategoryMode.RemoveAll)(base)
    (removeKey(FilterCategory.Skills, "graphify")(all) ==== all).log("RemoveAll should stay as it is")
  }

  def testAppendRule: Result = {
    val one = appendRule(TextRuleKind.Tag, "my-tag", TextRuleScope.Both)(base)
    val two = appendRule(TextRuleKind.Tag, "my-tag", TextRuleScope.UserTyped)(one)
    Result.all(
      List(
        (one.textRules.lastOption ==== Some(TextRule(TextRuleKind.Tag, "my-tag", TextRuleScope.Both, enabled = true)))
          .log("appended enabled rule"),
        (two.textRules.length ==== base.textRules.length + 2).log("different scope is a second rule"),
      )
    )
  }

  def testAppendRuleTwice: Property =
    for {
      pattern <- Gen.string(Gen.alphaNum, Range.linear(1, 10)).log("pattern")
      kind    <- Gen.element1(TextRuleKind.Text, TextRuleKind.Regex, TextRuleKind.Tag).log("kind")
    } yield {
      val once = appendRule(kind, pattern, TextRuleScope.Both)(base)
      (appendRule(kind, pattern, TextRuleScope.Both)(once) ==== once).log("appendRule should be idempotent")
    }

  def testAddRule: Result = {
    val out = addRule(TextRuleKind.Regex)(base)
    (out.textRules.lastOption ==== Some(TextRule.default.copy(kind = TextRuleKind.Regex))).log("appended rule")
  }

  def testRemoveRule: Result =
    Result.all(
      List(
        (removeRule(0)(base).textRules ==== List(TextRule.default.copy(kind = TextRuleKind.Tag))).log("remove first"),
        (removeRule(5)(base) ==== base).log("out of range"),
        (removeRule(-1)(base) ==== base).log("negative"),
      )
    )

  def testSetRuleFields: Result = {
    val configUpdater = setRuleKind(1, TextRuleKind.Regex)
      .andThen(setRuleScope(1, TextRuleScope.Assistant))
      .andThen(setRulePattern(1, "x+"))
      .andThen(setRuleEnabled(1, false))

    val out = configUpdater(base)
    Result.all(
      List(
        (out.textRules(1) ==== TextRule(TextRuleKind.Regex, "x+", TextRuleScope.Assistant, enabled = false))
          .log("rule 1"),
        (out.textRules.head ==== base.textRules.head).log("rule 0 untouched"),
      )
    )
  }

  def testOutOfRange: Result =
    Result.all(
      List(
        (setRuleKind(2, TextRuleKind.Regex)(base) ==== base).log("kind"),
        (setRuleScope(2, TextRuleScope.Assistant)(base) ==== base).log("scope"),
        (setRulePattern(2, "x")(base) ==== base).log("pattern"),
        (setRuleEnabled(2, false)(base) ==== base).log("enabled"),
      )
    )
}
