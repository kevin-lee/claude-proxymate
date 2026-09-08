package claudeproxymate.core.filter

import hedgehog.*
import hedgehog.runner.*

import FilterFixtures.*

object SkillsListSpec extends Properties {

  override def tests: List[Test] = List(
    example("entries parses names including plugin-style a:b names", testNames),
    example("a multi-line description belongs to its entry", testMultiLineEntry),
    example("remove keeps the other entries byte-identical", testRemoveKeepsOthers),
    example("removing every entry leaves the header only", testRemoveAll),
    example("no marker means no entries", testNoMarker),
    example("isSkillsReminder", testIsSkillsReminder),
    property("generated names round-trip through entries", testGeneratedNames),
  )

  def testNames: Result =
    (SkillsList.entries(SkillsInner).map(_.name) ==== SkillNames).log("skill names")

  def testMultiLineEntry: Result = {
    val entry = SkillsList.entries(SkillsInner).find(_.name == "claude-api")
    val text  = entry.map(e => SkillsInner.substring(e.start, e.end)).getOrElse("")
    Result.all(
      List(
        Result.assert(text.contains("TRIGGER - read BEFORE")).log(s"continuation line 1 missing: $text"),
        Result.assert(text.contains("SKIP only when")).log(s"continuation line 2 missing: $text"),
        Result.assert(!text.contains("- design:")).log(s"entry swallowed the next one: $text"),
        Result.assert(text.endsWith("\n")).log(s"entry should end with its newline: $text"),
      )
    )
  }

  def testRemoveKeepsOthers: Result = {
    val (out, removed) = SkillsList.remove(SkillsInner, e => e.name == "graphify" || e.name == "claude-api")
    val expected       =
      "The following skills are available for use with the Skill tool:\n\n" +
        "- skill-creator:skill-creator: Create new skills, modify and improve existing skills.\n" +
        "- design: Create a design canvas.\n"
    Result.all(
      List(
        (removed.map(_.name) ==== List("graphify", "claude-api")).log("removed names"),
        (out ==== expected).log("remaining text"),
        (SkillsList.entries(out).map(_.name) ==== List("skill-creator:skill-creator", "design")).log("remaining names"),
      )
    )
  }

  def testRemoveAll: Result = {
    val (out, removed) = SkillsList.remove(SkillsInner, _ => true)
    Result.all(
      List(
        (removed.length ==== 4).log("all four removed"),
        Result.assert(SkillsList.entries(out).isEmpty).log(s"entries remain: $out"),
        Result.assert(out.contains(SkillsList.Marker)).log("header should remain"),
      )
    )
  }

  def testNoMarker: Result =
    Result.assert(SkillsList.entries("- not: a skills list\n").isEmpty).log("entries without the marker")

  def testIsSkillsReminder: Result =
    Result.all(
      List(
        Result.assert(SkillsList.isSkillsReminder(SkillsInner)).log("skills"),
        Result.assert(!SkillsList.isSkillsReminder(ClaudeMdInner)).log("claudeMd"),
      )
    )

  private def genName: Gen[String] =
    for {
      head <- Gen.string(Gen.alpha, Range.linear(1, 6))
      tail <- Gen.string(Gen.element1('a', 'b', '-', '_'), Range.linear(0, 6))
    } yield head + tail

  def testGeneratedNames: Property =
    for {
      names <- Gen.list(genName, Range.linear(0, 6)).log("names")
      descs <- Gen.list(Gen.string(Gen.alphaNum, Range.linear(0, 12)), Range.linear(6, 6)).log("descs")
    } yield {
      val inner = "The following skills are available for use with the Skill tool:\n\n" +
        names.zip(descs).map { case (n, d) => s"- $n: $d\n" }.mkString
      (SkillsList.entries(inner).map(_.name) ==== names).log(s"names round-trip: $inner")
    }
}
