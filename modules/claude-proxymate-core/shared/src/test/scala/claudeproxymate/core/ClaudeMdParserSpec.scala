package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object ClaudeMdParserSpec extends Properties {

  override def tests: List[Test] = List(
    property("global CLAUDE.md single section", testGlobalClaudeMd),
    property("local CLAUDE.md single section", testLocalClaudeMd),
    property("global + local multiple sections", testMultipleSections),
    property("global rule file", testGlobalRule),
    property("memory file", testMemoryFile),
    property("4-section real system-reminder format", testFourSections),
    example("empty input returns empty list", testEmptyInput),
  )

  def testGlobalClaudeMd: Property =
    for {
      username <- Gen.string(Gen.alphaNum, Range.linear(3, 10)).log("username")
    } yield {
      val input    =
        s"Contents of /Users/$username/.claude/CLAUDE.md (user's private global instructions for all projects):\n\n# Global Rules\ncontent here"
      val sections = ClaudeMdParser.parseClaudeMdSections(input)

      Result.all(
        List(
          (sections.length ==== 1)
            .log(
              s"sections.length should be 1 but got sections.length=${sections.length}"
            ),
          (sections.head.label ==== "\uD83D\uDCCB Global CLAUDE.md")
            .log(
              s"sections.head.label should be \uD83D\uDCCB Global CLAUDE.md. sections.head.label=${sections.head.label}"
            ),
          (sections.head.scope ==== "global").log(
            s"""sections.head.scope should be "global" but got sections.head.scope=${sections.head.scope} """
          ),
          (sections.head.cls ==== "green").log(
            s"""sections.head.cls should be "green" but got sections.head.cls=${sections.head.cls} """
          ),
          Result
            .diffNamed(
              """sections.head.content should contain "# Global Rules"""",
              sections.head.content,
              "# Global Rules"
            )(_.contains(_))
            .log(
              s"sections.head.content should contain '# Global Rules' but got sections.head.content=${sections.head.content}"
            ),
        )
      )
    }

  def testLocalClaudeMd: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
      val input    =
        "Contents of /project/CLAUDE.md (project instructions, checked into the codebase):\n\n# Project Rules\ncontent"
      val sections = ClaudeMdParser.parseClaudeMdSections(input)

      Result.all(
        List(
          (sections.length ==== 1)
            .log(
              s"sections.length should be 1 but got sections.length=${sections.length}"
            ),
          (sections.head.label ==== "\uD83D\uDCCB Local CLAUDE.md").log(
            s"sections.head.label should be \uD83D\uDCCB Local CLAUDE.md but got sections.head.label=${sections.head.label}"
          ),
          (sections.head.scope ==== "local").log(
            s"""sections.head.scope should be "local" but got sections.head.scope=${sections.head.scope}"""
          ),
          (sections.head.cls ==== "cyan").log(
            s"""sections.head.cls should be "cyan" but got sections.head.cls=${sections.head.cls}"""
          )
        )
      )
    }

  def testMultipleSections: Property =
    for {
      username <- Gen.string(Gen.alphaNum, Range.linear(3, 10)).log("username")
    } yield {
      val input    = List(
        s"Contents of /Users/$username/.claude/CLAUDE.md (user's private global instructions for all projects):\n\n# Global Rules\nglobal content",
        "Contents of /project/CLAUDE.md (project instructions, checked into the codebase):\n\n# Local Rules\nlocal content",
      ).mkString("\n\n")
      val sections = ClaudeMdParser.parseClaudeMdSections(input)
      Result
        .assert(sections.length == 2)
        .and(Result.assert(sections(0).scope == "global"))
        .and(Result.assert(sections(1).scope == "local"))
    }

  def testGlobalRule: Property =
    for {
      username <- Gen.string(Gen.alphaNum, Range.linear(3, 10)).log("username")
    } yield {
      val input    =
        s"Contents of /Users/$username/.claude/rules/git-rules.md (user's private global instructions for all projects):\n\n# Git Rules\ncontent"
      val sections = ClaudeMdParser.parseClaudeMdSections(input)
      Result
        .assert(sections.length == 1)
        .and(
          Result.assert(
            sections.head.label == "\uD83D\uDCDC Global Rule: git-rules.md"
          )
        )
        .and(Result.assert(sections.head.scope == "global"))
    }

  def testMemoryFile: Property =
    for {
      username <- Gen.string(Gen.alphaNum, Range.linear(3, 10)).log("username")
    } yield {
      val input    =
        s"Contents of /Users/$username/.claude/projects/foo/memory/MEMORY.md (user's auto-memory, persists across conversations):\n\n# Memory\ncontent"
      val sections = ClaudeMdParser.parseClaudeMdSections(input)
      Result.assert(sections.length == 1).and(Result.assert(sections.head.label == "\uD83E\uDDE0 Memory: MEMORY.md"))
    }

  def testFourSections: Property =
    for {
      username <- Gen.string(Gen.alphaNum, Range.linear(3, 10)).log("username")
    } yield {
      val input    = List(
        s"Contents of /Users/$username/.claude/CLAUDE.md (user's private global instructions for all projects):\n\n# Global Rules\ncontent",
        s"Contents of /Users/$username/.claude/rules/git-rules.md (user's private global instructions for all projects):\n\n# Git Rules\ncontent",
        "Contents of /project/CLAUDE.md (project instructions, checked into the codebase):\n\n# Claude Proxymate\ncontent",
        s"Contents of /Users/$username/.claude/projects/foo/memory/MEMORY.md (user's auto-memory, persists across conversations):\n\n# Memory\ncontent",
      ).mkString("\n\n")
      val sections = ClaudeMdParser.parseClaudeMdSections(input)
      Result
        .assert(sections.length == 4)
        .and(Result.assert(sections(0).label == "\uD83D\uDCCB Global CLAUDE.md"))
        .and(
          Result.assert(
            sections(1).label == "\uD83D\uDCDC Global Rule: git-rules.md"
          )
        )
        .and(Result.assert(sections(2).label == "\uD83D\uDCCB Local CLAUDE.md"))
        .and(Result.assert(sections(3).label == "\uD83E\uDDE0 Memory: MEMORY.md"))
    }

  def testEmptyInput: Result = {
    val sections = ClaudeMdParser.parseClaudeMdSections("no sections here")
    Result
      .assert(sections.isEmpty)
      .log(s"sections should be empty but got => sections: $sections")
  }
}
