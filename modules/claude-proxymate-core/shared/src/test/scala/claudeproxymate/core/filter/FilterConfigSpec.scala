package claudeproxymate.core.filter

import hedgehog.*
import hedgehog.runner.*
import io.circe.parser.decode
import io.circe.syntax.*

object FilterConfigSpec extends Properties {

  override def tests: List[Test] = List(
    property("encode then decode round-trips", testRoundTrip),
    example("empty object decodes to the default config", testEmptyObject),
    example("config missing textRules decodes with no text rules", testMissingTextRules),
    example("unknown category mode fails to decode", testUnknownMode),
    example("unknown text rule scope fails to decode", testUnknownScope),
    example("activeRuleCount counts selected keys, remove-all once, enabled non-empty text rules", testActiveRuleCount),
    example("isNoOp when disabled or when nothing is active", testIsNoOp),
    property("category / withCategory round-trip", testCategoryAccessors),
  )

  private def genMode: Gen[CategoryMode] =
    Gen.element1(CategoryMode.KeepAll, CategoryMode.RemoveSelected, CategoryMode.RemoveAll)

  private def genKey: Gen[String] =
    Gen.string(Gen.alphaNum, Range.linear(1, 12)).map(s => s"/u/.claude/rules/$s.md")

  private def genCategory: Gen[CategoryFilter] =
    for {
      mode <- genMode
      keys <- Gen.list(genKey, Range.linear(0, 4))
    } yield CategoryFilter(mode, keys)

  private def genRule: Gen[TextRule] =
    for {
      kind    <- Gen.element1(TextRuleKind.Text, TextRuleKind.Regex, TextRuleKind.Tag)
      pattern <- Gen.string(Gen.alphaNum, Range.linear(0, 10))
      scope   <- Gen.element1(TextRuleScope.UserTyped, TextRuleScope.Assistant, TextRuleScope.Both)
      enabled <- Gen.boolean
    } yield TextRule(kind, pattern, scope, enabled)

  def genConfig: Gen[FilterConfig] =
    for {
      enabled <- Gen.boolean
      rules   <- genCategory
      docs    <- genCategory
      skills  <- genCategory
      text    <- Gen.list(genRule, Range.linear(0, 4))
    } yield FilterConfig(FilterConfig.CurrentVersion, enabled, rules, docs, skills, text)

  def testRoundTrip: Property =
    for {
      cfg <- genConfig.log("cfg")
    } yield {
      val json    = cfg.asJson.noSpaces
      val decoded = decode[FilterConfig](json)
      (decoded ==== Right(cfg)).log(s"round-trip mismatch: json=$json")
    }

  def testEmptyObject: Result =
    (decode[FilterConfig]("{}") ==== Right(FilterConfig.default)).log("{} should decode to the default config")

  def testMissingTextRules: Result = {
    val json = """{"version":1,"enabled":true,"rules":{"mode":"removeAll","keys":[]}}"""
    (decode[FilterConfig](json) ==== Right(FilterConfig.default.copy(rules = FilterFixtures.removeAll)))
      .log("missing docs/skills/textRules should fall back to their defaults")
  }

  def testUnknownMode: Result = {
    val json = """{"rules":{"mode":"bogus","keys":[]}}"""
    Result.assert(decode[FilterConfig](json).isLeft).log("an unknown mode must fail decoding")
  }

  def testUnknownScope: Result = {
    val json = """{"textRules":[{"kind":"text","pattern":"x","scope":"everywhere","enabled":true}]}"""
    Result.assert(decode[FilterConfig](json).isLeft).log("an unknown scope must fail decoding")
  }

  def testActiveRuleCount: Result = {
    val cfg = FilterConfig
      .default
      .copy(
        rules = FilterFixtures.removeSelected("a", "b"),
        docs = FilterFixtures.removeAll,
        skills = CategoryFilter.keepAll,
        textRules = List(
          FilterFixtures.rule(TextRuleKind.Text, "x", TextRuleScope.Both),
          FilterFixtures.rule(TextRuleKind.Text, "", TextRuleScope.Both),
          FilterFixtures.rule(TextRuleKind.Tag, "t", TextRuleScope.Both).copy(enabled = false),
        ),
      )
    (cfg.activeRuleCount ==== 4).log(s"expected 2 keys + 1 remove-all + 1 text rule, got ${cfg.activeRuleCount}")
  }

  def testIsNoOp: Result =
    Result.all(
      List(
        Result.assert(FilterConfig.default.isNoOp).log("default (keep all) should be a no-op"),
        Result.assert(FilterConfig.disabled.copy(rules = FilterFixtures.removeAll).isNoOp).log("disabled is a no-op"),
        Result
          .assert(!FilterConfig.default.copy(skills = FilterFixtures.removeAll).isNoOp)
          .log("remove-all skills is not a no-op"),
      )
    )

  def testCategoryAccessors: Property =
    for {
      cfg <- genConfig.log("cfg")
      cat <- Gen.element1(FilterCategory.Rules, FilterCategory.Docs, FilterCategory.Skills).log("cat")
      f   <- genCategory.log("f")
    } yield {
      val updated = cfg.withCategory(cat, f)
      (updated.category(cat) ==== f).log("withCategory then category should return the new filter")
    }
}
