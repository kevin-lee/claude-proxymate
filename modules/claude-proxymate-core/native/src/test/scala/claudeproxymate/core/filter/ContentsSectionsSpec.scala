package claudeproxymate.core.filter

/* munit port of the shared hedgehog ContentsSectionsSpec for Scala Native,
 * where the hedgehog runner hangs. Examples port 1:1; the generated-bounds
 * property is replaced by examples at the generator's bounds (one section,
 * five sections, empty bodies).
 */
class ContentsSectionsSpec extends munit.FunSuite {

  import FilterFixtures.*

  test("locate finds the four sections with paths, categories and labels") {
    val located = ContentsSections.locate(ClaudeMdInner)
    assertEquals(located.map(_.path), List(GlobalClaudeMd, GlobalRule, ProjectClaudeMd, Memory))
    assertEquals(
      located.map(_.category),
      List(SectionCategory.Docs, SectionCategory.Rules, SectionCategory.Docs, SectionCategory.Docs),
    )
    assertEquals(
      located.map(_.label),
      List(
        "📋 Global CLAUDE.md",
        "📜 Global Rule: Simple-Global-Rules.md",
        "📋 Local CLAUDE.md",
        "🧠 Memory: MEMORY.md"
      ),
    )
  }

  test("no section range reaches into the reminder trailer") {
    ContentsSections.locate(ClaudeMdInner).map(s => ClaudeMdInner.substring(s.start, s.end)).foreach { t =>
      assert(!t.contains("# userEmail") && !t.contains("# currentDate") && !t.contains("IMPORTANT: this context"), t)
    }
  }

  test("removing the last section keeps the trailer") {
    val (out, removed) = ContentsSections.remove(ClaudeMdInner, _.path == Memory)
    assertEquals(removed.map(_.path), List(Memory))
    assert(!out.contains("MEMORY.md"), out)
    assert(!out.contains(MemoryBody), out)
    assert(out.contains("# userEmail\nThe user's email address"), out)
    assert(out.contains("# currentDate\nToday's date"), out)
    assert(out.contains("IMPORTANT: this context may or may not be relevant"), out)
    assert(out.contains(ProjectClaudeMdSection))
  }

  test("removing a middle section keeps its neighbours byte-identical") {
    val (out, removed) = ContentsSections.remove(ClaudeMdInner, _.path == GlobalRule)
    assertEquals(removed.map(_.path), List(GlobalRule))
    assert(!out.contains(GlobalRule))
    assert(!out.contains(GlobalRuleBody))
    assert(out.contains(GlobalClaudeMdSection))
    assert(out.contains(ProjectClaudeMdSection))
    assert(out.contains(MemorySection))
    assert(out.startsWith(Intro))
    assert(out.endsWith(Trailer))
  }

  test("removing every section keeps the intro and the trailer") {
    val (out, removed) = ContentsSections.remove(ClaudeMdInner, _ => true)
    assertEquals(removed.length, 4)
    assert(!out.contains("Contents of "), out)
    assert(out.startsWith(Intro))
    assert(out.endsWith(Trailer))
  }

  test("categoryOf: /rules/ paths are Rules, everything else Docs") {
    assertEquals(ContentsSections.categoryOf("/u/.claude/rules/x.md"), SectionCategory.Rules)
    assertEquals(ContentsSections.categoryOf("/proj/.claude/rules/x.md"), SectionCategory.Rules)
    assertEquals(ContentsSections.categoryOf("/u/.claude/CLAUDE.md"), SectionCategory.Docs)
    assertEquals(ContentsSections.categoryOf("/proj/README.md"), SectionCategory.Docs)
  }

  test("isContentsReminder") {
    assert(ContentsSections.isContentsReminder(ClaudeMdInner))
    assert(!ContentsSections.isContentsReminder(SkillsInner))
  }

  private def assertGeneratedBounds(bodies: List[String]): Unit = {
    val inner   = (Intro :: bodies.zipWithIndex.map {
      case (b, i) =>
        section(s"/u/.claude/rules/r$i.md", GlobalDesc, b)
    }).mkString("\n\n") + Trailer
    val located = ContentsSections.locate(inner)
    val ranges  = located.map(s => (s.start, s.end))
    assertEquals(located.length, bodies.length)
    assert(ranges.zip(ranges.drop(1)).forall { case ((_, e1), (s2, _)) => e1 == s2 }, ranges.toString)
    assert(located.forall(s => s.start < s.end), ranges.toString)
    /* The last section ends at the newline that opens the trailer, so the
     * intro, its separator and the whole trailer remain. */
    assertEquals(ContentsSections.remove(inner, _ => true)._1, Intro + "\n\n" + Trailer)
  }

  test("generated section bodies: ranges are contiguous and non-overlapping (example: one section)") {
    assertGeneratedBounds(List("body"))
  }

  test("generated section bodies: ranges are contiguous and non-overlapping (example: five sections, one empty)") {
    assertGeneratedBounds(List("a", "", "ccc", "dd dd", "e"))
  }
}
