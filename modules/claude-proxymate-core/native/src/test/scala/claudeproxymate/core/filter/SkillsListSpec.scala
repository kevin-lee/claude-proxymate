package claudeproxymate.core.filter

/* munit port of the shared hedgehog SkillsListSpec for Scala Native, where
 * the hedgehog runner hangs. Examples port 1:1; the generated-names property
 * is replaced by examples at the generator's bounds (1-char name, name with
 * hyphens and underscores, empty description).
 */
class SkillsListSpec extends munit.FunSuite {

  import FilterFixtures.*

  test("entries parses names including plugin-style a:b names") {
    assertEquals(SkillsList.entries(SkillsInner).map(_.name), SkillNames)
  }

  test("a multi-line description belongs to its entry") {
    val entry = SkillsList.entries(SkillsInner).find(_.name == "claude-api")
    val text  = entry.map(e => SkillsInner.substring(e.start, e.end)).getOrElse("")
    assert(text.contains("TRIGGER - read BEFORE"), text)
    assert(text.contains("SKIP only when"), text)
    assert(!text.contains("- design:"), text)
    assert(text.endsWith("\n"), text)
  }

  test("remove keeps the other entries byte-identical") {
    val (out, removed) = SkillsList.remove(SkillsInner, e => e.name == "graphify" || e.name == "claude-api")
    val expected       =
      "The following skills are available for use with the Skill tool:\n\n" +
        "- skill-creator:skill-creator: Create new skills, modify and improve existing skills.\n" +
        "- design: Create a design canvas.\n"
    assertEquals(removed.map(_.name), List("graphify", "claude-api"))
    assertEquals(out, expected)
    assertEquals(SkillsList.entries(out).map(_.name), List("skill-creator:skill-creator", "design"))
  }

  test("removing every entry leaves the header only") {
    val (out, removed) = SkillsList.remove(SkillsInner, _ => true)
    assertEquals(removed.length, 4)
    assert(SkillsList.entries(out).isEmpty, out)
    assert(out.contains(SkillsList.Marker))
  }

  test("no marker means no entries") {
    assert(SkillsList.entries("- not: a skills list\n").isEmpty)
  }

  test("isSkillsReminder") {
    assert(SkillsList.isSkillsReminder(SkillsInner))
    assert(!SkillsList.isSkillsReminder(ClaudeMdInner))
  }

  private def assertNamesRoundTrip(names: List[String], descs: List[String]): Unit = {
    val inner = "The following skills are available for use with the Skill tool:\n\n" +
      names.zip(descs).map { case (n, d) => s"- $n: $d\n" }.mkString
    assertEquals(SkillsList.entries(inner).map(_.name), names, inner)
  }

  test("generated names round-trip through entries (example: 1-char name, empty description)") {
    assertNamesRoundTrip(List("a"), List(""))
  }

  test("generated names round-trip through entries (example: hyphen / underscore names)") {
    assertNamesRoundTrip(List("abc-d_e", "Zz--", "q_"), List("one", "", "three words here"))
  }

  test("generated names round-trip through entries (example: no entries)") {
    assertNamesRoundTrip(Nil, Nil)
  }
}
