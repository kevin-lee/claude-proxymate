package claudeproxymate.core.filter

import hedgehog.*
import hedgehog.runner.*

import FilterFixtures.*

object FilterInventorySpec extends Properties {

  override def tests: List[Test] = List(
    example("fromBody finds every section and skill with labels and bytes", testFromBody),
    example("fromBody dedupes repeated turns", testFromBodyDedupe),
    example("fromBody ignores assistant turns and non-reminder text", testFromBodyIgnores),
    example("merge flags sources, dedupes, and keeps config-only keys", testMerge),
    example("merge orders traffic, then disk-only, then config-only", testMergeOrder),
  )

  def testFromBody: Result = {
    val items = FilterInventory.fromBody(TypicalBody)
    Result.all(
      List(
        (items.map(i => (i.category, i.key)) ==== List(
          (FilterCategory.Docs, GlobalClaudeMd),
          (FilterCategory.Rules, GlobalRule),
          (FilterCategory.Docs, ProjectClaudeMd),
          (FilterCategory.Docs, Memory),
          (FilterCategory.Skills, "graphify"),
          (FilterCategory.Skills, "skill-creator:skill-creator"),
          (FilterCategory.Skills, "claude-api"),
          (FilterCategory.Skills, "design"),
        )).log(s"items: $items"),
        (items.find(_.key == GlobalRule).map(_.label) ==== Some("📜 Global Rule: Simple-Global-Rules.md"))
          .log("rule label"),
        (items.find(_.key == "graphify").map(_.label) ==== Some("🔧 graphify")).log("skill label"),
        Result.assert(items.forall(_.bytes > 0)).log("bytes"),
      )
    )
  }

  def testFromBodyDedupe: Result = {
    val body  = FilterFixtures.body(userMsg(SkillsReminder), assistantMsg("x"), userMsg(SkillsReminder))
    val items = FilterInventory.fromBody(body)
    (items.map(_.key) ==== SkillNames).log(s"duplicated: $items")
  }

  def testFromBodyIgnores: Result = {
    val body  = FilterFixtures.body(userMsg("plain text with Contents of nothing"), assistantMsg(SkillsReminder))
    val items = FilterInventory.fromBody(body)
    Result.assert(items.isEmpty).log(s"unexpected items: $items")
  }

  private val ruleTraffic =
    InventoryItem(FilterCategory.Rules, GlobalRule, "📜 Global Rule: Simple-Global-Rules.md", 100)
  private val ruleDisk    = InventoryItem(FilterCategory.Rules, GlobalRule, "Simple-Global-Rules.md", 0)
  private val otherDisk   = InventoryItem(FilterCategory.Rules, "/u/.claude/rules/Other.md", "Other.md", 0)
  private val skillDisk   = InventoryItem(FilterCategory.Skills, "archify", "archify", 0)

  def testMerge: Result = {
    val cfg    = FilterConfig
      .default
      .copy(
        rules = removeSelected(GlobalRule, "/old/project/.claude/rules/Gone.md"),
        skills = removeSelected("archify"),
      )
    val merged = FilterInventory.merge(List(ruleDisk, otherDisk, skillDisk), List(ruleTraffic), cfg)
    val byKey  = merged.map(m => m.item.key -> m).toMap
    Result.all(
      List(
        (merged.length ==== 4).log(s"expected 4 merged items, got $merged"),
        (byKey.get(GlobalRule).map(m => (m.onDisk, m.inTraffic, m.item.bytes, m.item.label)) ==== Some(
          (true, true, 100, "📜 Global Rule: Simple-Global-Rules.md")
        )).log("traffic item wins and is flagged on disk"),
        (byKey.get(otherDisk.key).map(m => (m.onDisk, m.inTraffic)) ==== Some((true, false))).log("disk-only"),
        (byKey.get(skillDisk.key).map(m => (m.onDisk, m.inTraffic)) ==== Some((true, false))).log("disk-only skill"),
        (byKey
          .get("/old/project/.claude/rules/Gone.md")
          .map(m => (m.onDisk, m.inTraffic, m.absent, m.item.label)) ==== Some(
          (false, false, true, "Gone.md")
        )).log("config-only key kept and marked absent"),
      )
    )
  }

  def testMergeOrder: Result = {
    val cfg    = FilterConfig.default.copy(rules = removeSelected("/gone/rules/Z.md"))
    val merged = FilterInventory.merge(List(otherDisk), List(ruleTraffic), cfg)
    (merged.map(_.item.key) ==== List(GlobalRule, otherDisk.key, "/gone/rules/Z.md")).log(s"order: $merged")
  }
}
