package claudeproxymate.core.filter

import hedgehog.*
import hedgehog.runner.*

import FilterFixtures.*

object ContentsSectionsSpec extends Properties {

  override def tests: List[Test] = List(
    example("locate finds the four sections with paths, categories and labels", testLocate),
    example("no section range reaches into the reminder trailer", testBoundsExcludeTrailer),
    example("removing the last section keeps the trailer", testRemoveLastKeepsTrailer),
    example("removing a middle section keeps its neighbours byte-identical", testRemoveMiddle),
    example("removing every section keeps the intro and the trailer", testRemoveAll),
    example("categoryOf: /rules/ paths are Rules, everything else Docs", testCategoryOf),
    example("isContentsReminder", testIsContentsReminder),
    property("generated section bodies: ranges are contiguous and non-overlapping", testGeneratedBounds),
  )

  def testLocate: Result = {
    val located = ContentsSections.locate(ClaudeMdInner)
    Result.all(
      List(
        (located.map(_.path) ==== List(GlobalClaudeMd, GlobalRule, ProjectClaudeMd, Memory)).log("paths"),
        (located.map(_.category) ==== List(
          SectionCategory.Docs,
          SectionCategory.Rules,
          SectionCategory.Docs,
          SectionCategory.Docs,
        )).log("categories"),
        (located.map(_.label) ==== List(
          "📋 Global CLAUDE.md",
          "📜 Global Rule: Simple-Global-Rules.md",
          "📋 Local CLAUDE.md",
          "🧠 Memory: MEMORY.md",
        )).log("labels"),
      )
    )
  }

  def testBoundsExcludeTrailer: Result = {
    val texts = ContentsSections.locate(ClaudeMdInner).map(s => ClaudeMdInner.substring(s.start, s.end))
    Result.all(
      texts.map { t =>
        Result
          .assert(!t.contains("# userEmail") && !t.contains("# currentDate") && !t.contains("IMPORTANT: this context"))
          .log(s"section text reaches the trailer: $t")
      }
    )
  }

  def testRemoveLastKeepsTrailer: Result = {
    val (out, removed) = ContentsSections.remove(ClaudeMdInner, _.path == Memory)
    Result.all(
      List(
        (removed.map(_.path) ==== List(Memory)).log("only the memory section should be removed"),
        Result.assert(!out.contains("MEMORY.md")).log(s"memory header still present: $out"),
        Result.assert(!out.contains(MemoryBody)).log(s"memory body still present: $out"),
        Result.assert(out.contains("# userEmail\nThe user's email address")).log(s"userEmail lost: $out"),
        Result.assert(out.contains("# currentDate\nToday's date")).log(s"currentDate lost: $out"),
        Result
          .assert(out.contains("IMPORTANT: this context may or may not be relevant"))
          .log(s"closing line lost: $out"),
        Result.assert(out.contains(ProjectClaudeMdSection)).log("project section damaged"),
      )
    )
  }

  def testRemoveMiddle: Result = {
    val (out, removed) = ContentsSections.remove(ClaudeMdInner, _.path == GlobalRule)
    Result.all(
      List(
        (removed.map(_.path) ==== List(GlobalRule)).log("only the rule section should be removed"),
        Result.assert(!out.contains(GlobalRule)).log("rule header still present"),
        Result.assert(!out.contains(GlobalRuleBody)).log("rule body still present"),
        Result.assert(out.contains(GlobalClaudeMdSection)).log("global CLAUDE.md section damaged"),
        Result.assert(out.contains(ProjectClaudeMdSection)).log("project section damaged"),
        Result.assert(out.contains(MemorySection)).log("memory section damaged"),
        Result.assert(out.startsWith(Intro)).log("intro damaged"),
        Result.assert(out.endsWith(Trailer)).log("trailer damaged"),
      )
    )
  }

  def testRemoveAll: Result = {
    val (out, removed) = ContentsSections.remove(ClaudeMdInner, _ => true)
    Result.all(
      List(
        (removed.length ==== 4).log("all four sections should be removed"),
        Result.assert(!out.contains("Contents of ")).log(s"a section survived: $out"),
        Result.assert(out.startsWith(Intro)).log("intro damaged"),
        Result.assert(out.endsWith(Trailer)).log("trailer damaged"),
      )
    )
  }

  def testCategoryOf: Result =
    Result.all(
      List(
        (ContentsSections.categoryOf("/u/.claude/rules/x.md") ==== SectionCategory.Rules).log("global rule"),
        (ContentsSections.categoryOf("/proj/.claude/rules/x.md") ==== SectionCategory.Rules).log("project rule"),
        (ContentsSections.categoryOf("/u/.claude/CLAUDE.md") ==== SectionCategory.Docs).log("CLAUDE.md"),
        (ContentsSections.categoryOf("/proj/README.md") ==== SectionCategory.Docs).log("README.md"),
      )
    )

  def testIsContentsReminder: Result =
    Result.all(
      List(
        Result.assert(ContentsSections.isContentsReminder(ClaudeMdInner)).log("claudeMd reminder"),
        Result.assert(!ContentsSections.isContentsReminder(SkillsInner)).log("skills reminder"),
      )
    )

  def testGeneratedBounds: Property =
    for {
      bodies <- Gen.list(Gen.string(Gen.alphaNum, Range.linear(0, 20)), Range.linear(1, 5)).log("bodies")
    } yield {
      val inner   = (Intro :: bodies.zipWithIndex.map {
        case (b, i) =>
          section(s"/u/.claude/rules/r$i.md", GlobalDesc, b)
      }).mkString("\n\n") + Trailer
      val located = ContentsSections.locate(inner)
      val ranges  = located.map(s => (s.start, s.end))
      val ordered = ranges.zip(ranges.drop(1)).forall { case ((_, e1), (s2, _)) => e1 == s2 }
      Result.all(
        List(
          (located.length ==== bodies.length).log(s"expected ${bodies.length} sections, got ${located.length}"),
          Result.assert(ordered).log(s"ranges not contiguous: $ranges"),
          Result.assert(located.forall(s => s.start < s.end)).log(s"empty range: $ranges"),
          /* The last section ends at the newline that opens the trailer, so
           * the intro, its separator and the whole trailer remain. */
          (ContentsSections.remove(inner, _ => true)._1 ==== (Intro + "\n\n" + Trailer))
            .log("removing everything should leave intro, separator and trailer"),
        )
      )
    }
}
