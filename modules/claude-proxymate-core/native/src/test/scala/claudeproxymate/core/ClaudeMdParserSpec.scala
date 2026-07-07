package claudeproxymate.core

/* munit port of the shared hedgehog ClaudeMdParserSpec for Scala Native,
 * where the hedgehog runner hangs. Each hedgehog property is replaced by
 * deterministic examples at the boundaries its generator stresses (the
 * generators only vary an alphanumeric username of length 3 to 10 in the
 * path); the single hedgehog example is ported 1:1.
 */
class ClaudeMdParserSpec extends munit.FunSuite {

  private def assertSingleGlobalClaudeMd(username: String): Unit = {
    val input    =
      s"Contents of /Users/$username/.claude/CLAUDE.md (user's private global instructions for all projects):\n\n# Global Rules\ncontent here"
    val sections = ClaudeMdParser.parseClaudeMdSections(input)
    sections match {
      case section :: Nil =>
        assertEquals(section.label, "\uD83D\uDCCB Global CLAUDE.md")
        assertEquals(section.scope, "global")
        assertEquals(section.cls, "green")
        assert(
          section.content.contains("# Global Rules"),
          s"content should contain '# Global Rules' but got content=${section.content}",
        )
      case other =>
        fail(s"expected 1 section, got $other")
    }
  }

  test("global CLAUDE.md single section (example: 3-char username, generator lower bound)") {
    assertSingleGlobalClaudeMd("abc")
  }

  test("global CLAUDE.md single section (example: 10-char username, generator upper bound)") {
    assertSingleGlobalClaudeMd("abcdefgh42")
  }

  private def assertSingleLocalClaudeMd(path: String): Unit = {
    val input    =
      s"Contents of $path (project instructions, checked into the codebase):\n\n# Project Rules\ncontent"
    val sections = ClaudeMdParser.parseClaudeMdSections(input)
    sections match {
      case section :: Nil =>
        assertEquals(section.label, "\uD83D\uDCCB Local CLAUDE.md")
        assertEquals(section.scope, "local")
        assertEquals(section.cls, "cyan")
      case other =>
        fail(s"expected 1 section, got $other")
    }
  }

  test("local CLAUDE.md single section (example: short project path)") {
    assertSingleLocalClaudeMd("/project/CLAUDE.md")
  }

  test("local CLAUDE.md single section (example: deeply nested project path)") {
    assertSingleLocalClaudeMd("/home/dev/work/some/deeply/nested/project/CLAUDE.md")
  }

  private def assertGlobalThenLocal(username: String): Unit = {
    val input    = List(
      s"Contents of /Users/$username/.claude/CLAUDE.md (user's private global instructions for all projects):\n\n# Global Rules\nglobal content",
      "Contents of /project/CLAUDE.md (project instructions, checked into the codebase):\n\n# Local Rules\nlocal content",
    ).mkString("\n\n")
    val sections = ClaudeMdParser.parseClaudeMdSections(input)
    assertEquals(sections.map(_.scope), List("global", "local"))
  }

  test("global + local multiple sections (example: 3-char username, generator lower bound)") {
    assertGlobalThenLocal("abc")
  }

  test("global + local multiple sections (example: 10-char username, generator upper bound)") {
    assertGlobalThenLocal("abcdefgh42")
  }

  private def assertGlobalRule(username: String, ruleFileName: String): Unit = {
    val input    =
      s"Contents of /Users/$username/.claude/rules/$ruleFileName (user's private global instructions for all projects):\n\n# Rules\ncontent"
    val sections = ClaudeMdParser.parseClaudeMdSections(input)
    sections match {
      case section :: Nil =>
        assertEquals(section.label, s"\uD83D\uDCDC Global Rule: $ruleFileName")
        assertEquals(section.scope, "global")
      case other =>
        fail(s"expected 1 section, got $other")
    }
  }

  test("global rule file (example: git-rules.md)") {
    assertGlobalRule("abc", "git-rules.md")
  }

  test("global rule file (example: scala-rules.md, 10-char username)") {
    assertGlobalRule("abcdefgh42", "scala-rules.md")
  }

  private def assertMemoryFile(username: String, memoryFileName: String): Unit = {
    val input    =
      s"Contents of /Users/$username/.claude/projects/foo/memory/$memoryFileName (user's auto-memory, persists across conversations):\n\n# Memory\ncontent"
    val sections = ClaudeMdParser.parseClaudeMdSections(input)
    sections match {
      case section :: Nil =>
        assertEquals(section.label, s"\uD83E\uDDE0 Memory: $memoryFileName")
      case other =>
        fail(s"expected 1 section, got $other")
    }
  }

  test("memory file (example: MEMORY.md)") {
    assertMemoryFile("abc", "MEMORY.md")
  }

  test("memory file (example: NOTES.md, 10-char username)") {
    assertMemoryFile("abcdefgh42", "NOTES.md")
  }

  private def assertFourSections(username: String): Unit = {
    val input    = List(
      s"Contents of /Users/$username/.claude/CLAUDE.md (user's private global instructions for all projects):\n\n# Global Rules\ncontent",
      s"Contents of /Users/$username/.claude/rules/git-rules.md (user's private global instructions for all projects):\n\n# Git Rules\ncontent",
      "Contents of /project/CLAUDE.md (project instructions, checked into the codebase):\n\n# Claude Proxymate\ncontent",
      s"Contents of /Users/$username/.claude/projects/foo/memory/MEMORY.md (user's auto-memory, persists across conversations):\n\n# Memory\ncontent",
    ).mkString("\n\n")
    val sections = ClaudeMdParser.parseClaudeMdSections(input)
    assertEquals(
      sections.map(_.label),
      List(
        "\uD83D\uDCCB Global CLAUDE.md",
        "\uD83D\uDCDC Global Rule: git-rules.md",
        "\uD83D\uDCCB Local CLAUDE.md",
        "\uD83E\uDDE0 Memory: MEMORY.md",
      ),
    )
  }

  test("4-section real system-reminder format (example: 3-char username, generator lower bound)") {
    assertFourSections("abc")
  }

  test("4-section real system-reminder format (example: 10-char username, generator upper bound)") {
    assertFourSections("abcdefgh42")
  }

  test("empty input returns empty list") {
    val sections = ClaudeMdParser.parseClaudeMdSections("no sections here")
    assert(sections.isEmpty, s"sections should be empty but got => sections: $sections")
  }
}
