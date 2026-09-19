package claudeproxymate.renderer.messages

import claudeproxymate.core.filter.{ContentsSections, FilterCategory}
import claudeproxymate.renderer.messages.MessageParser.Part
import hedgehog.*
import hedgehog.runner.*

object MessageParserSpec extends Properties {

  override def tests: List[Test] = List(
    example("Contents sections carry Item keys with the category of their path", testSectionKeys),
    example("the skills reminder carries SkillsReminder", testSkillsKey),
    example("plain reminders and slash commands carry no key", testNoKey),
    example("typed text between blocks stays a TextPart", testTypedText),
  )

  private val rulePath = "/u/.claude/rules/Simple-Global-Rules.md"
  private val docPath  = "/u/proj/CLAUDE.md"

  private val contentsInner: String =
    s"Contents of $rulePath (user's private global instructions for all projects):\n\n# Simple\n1. Do it\n\n" +
      s"Contents of $docPath (project instructions, checked into the codebase):\n\n## graphify\nRules.\n\n" +
      "# userEmail\nkevin@example.com\n" +
      "IMPORTANT: this context may or may not be relevant to your tasks.\n"

  private val skillsInner: String =
    "The following skills are available for use with the Skill tool:\n\n- graphify: Graphs.\n- archify: Diagrams.\n"

  private def injected(parts: List[Part]): List[Part.InjectedPart] =
    parts.collect { case p: Part.InjectedPart => p }

  def testSectionKeys: Result = {
    val parts   = injected(MessageParser.parseUserText(s"<system-reminder>$contentsInner</system-reminder>"))
    val located = ContentsSections.locate(contentsInner).map(_.path)
    Result.all(
      List(
        (parts.map(_.filter) ==== List(
          Some(BadgeFilter.Item(FilterCategory.Rules, rulePath)),
          Some(BadgeFilter.Item(FilterCategory.Docs, docPath)),
        )).log("keys"),
        (located ==== List(rulePath, docPath)).log("same paths as ContentsSections.locate"),
      )
    )
  }

  def testSkillsKey: Result = {
    val parts = injected(MessageParser.parseUserText(s"<system-reminder>$skillsInner</system-reminder>"))
    (parts.map(_.filter) ==== List(Some(BadgeFilter.SkillsReminder))).log(parts.toString)
  }

  def testNoKey: Result = {
    val text  = "<system-reminder>Just a note.</system-reminder><command-message>init</command-message>"
    val parts = injected(MessageParser.parseUserText(text))
    (parts.map(_.filter) ==== List(None, None)).log(parts.toString)
  }

  def testTypedText: Result = {
    val parts = MessageParser.parseUserText(s"<system-reminder>$skillsInner</system-reminder>\nhello there")
    Result.all(
      List(
        (parts.length ==== 2).log(parts.toString),
        (parts.lastOption ==== Some(Part.TextPart("hello there"))).log(parts.toString),
      )
    )
  }
}
