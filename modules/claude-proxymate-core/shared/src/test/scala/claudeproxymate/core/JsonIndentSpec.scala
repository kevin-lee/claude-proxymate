package claudeproxymate.core

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object JsonIndentSpec extends Properties {

  override def tests: List[Test] = List(
    property("a space-indented document detects its own width", testSpacesDetected),
    property("the smallest width present wins", testSmallestWidthWins),
    property("a tab-indented document detects Tabs", testTabsDetected),
    property("a document with no indented line detects nothing", testFlatDetectsNothing),
    example("issue #44: a 2-space file with an empty env object", testIssue44Input),
    example("a file already damaged by the old fixed tab size detects 2", testDamagedFileDetectsTwo),
    example("CRLF line endings do not affect detection", testCrlfDetected),
    example("empty, blank and single-line documents detect nothing", testNothingToDetect),
    example("an indented comment line counts as indentation", testCommentLineCounts),
    example("fromEditorSettings maps editor.insertSpaces and editor.tabSize", testFromEditorSettings),
    example("fromEditorSettings rejects a tab size outside the sane range", testFromEditorSettingsRejects),
    example("the extension methods map to jsonc formatting options", testFormattingOptionValues),
    property("reindentLine sets exactly one level whatever the damage", testReindentAnyDamage),
    example("reindentLine repairs the issue #44 file", testReindentIssue44File),
    example("reindentLine keeps CRLF line endings", testReindentCrlf),
    example("reindentLine uses a tab for Tabs", testReindentTabs),
    example("reindentLine leaves a correctly indented line unchanged", testReindentNoOp),
    example("reindentLine leaves a line alone when content precedes the token", testReindentContentBefore),
    example("reindentLine ignores an out-of-range offset", testReindentOutOfRange),
    example("unit is the whitespace for one level", testUnit),
  )

  private def genWidth: Gen[Int] = Gen.int(Range.linear(1, 8))

  private def genPort: Gen[Int] = Gen.int(Range.linear(1024, 65535))

  private def doc(lines: String*): String = lines.mkString("", "\n", "\n")

  private def crlfDoc(lines: String*): String = lines.mkString("", "\r\n", "\r\n")

  def testSpacesDetected: Property =
    for {
      width <- genWidth.log("width")
    } yield {
      val one = " " * width
      val two = " " * (width * 2)
      val text = doc(
        "{",
        s"""$one"env": {""",
        s"""$two"ANTHROPIC_BASE_URL": "http://localhost:8888"""",
        s"$one}",
        "}",
      )
      JsonIndent.detect(text) ==== JsonIndent.Spaces(width).some
    }

  def testSmallestWidthWins: Property =
    for {
      width <- genWidth.log("width")
    } yield {
      /* The first indented line sits two levels deep, a later one at a
       * single level. The single level is the file's real indentation. */
      val one   = " " * width
      val two   = " " * (width * 2)
      val three = " " * (width * 3)
      val text  = doc(
        "{",
        s"""$two"deep": {""",
        s"""$three"deeper": 1""",
        s"$two},",
        s"""$one"env": {}""",
        "}",
      )
      JsonIndent.detect(text) ==== JsonIndent.Spaces(width).some
    }

  def testTabsDetected: Property =
    for {
      depth <- genWidth.log("depth")
    } yield {
      val inner = "\t" * depth
      val text  = doc(
        "{",
        "\t\"env\": {",
        s"""$inner"ANTHROPIC_BASE_URL": "http://localhost:8888"""",
        "\t}",
        "}",
      )
      JsonIndent.detect(text) ==== JsonIndent.Tabs.some
    }

  def testFlatDetectsNothing: Property =
    for {
      port <- genPort.log("port")
    } yield {
      val text = s"""{"env":{"ANTHROPIC_BASE_URL":"http://localhost:$port"}}"""
      JsonIndent.detect(text) ==== none[JsonIndent]
    }

  def testIssue44Input: Result = {
    val text = doc("{", """  "env": {""", "  }", "}")
    JsonIndent.detect(text) ==== JsonIndent.Spaces(2).some
  }

  def testDamagedFileDetectsTwo: Result = {
    /* What the fixed tab size of 4 left behind: "env" pushed to column 0,
     * the key we inserted at 4 spaces, the user's own keys still at 2. */
    val text = doc(
      "{",
      """"env": {""",
      """    "ANTHROPIC_BASE_URL": "http://localhost:8888"""",
      "  },",
      """  "cleanupPeriodDays": 36500""",
      "}",
    )
    JsonIndent.detect(text) ==== JsonIndent.Spaces(2).some
  }

  def testCrlfDetected: Result = {
    val text = crlfDoc("{", """  "env": {""", "  }", "}")
    JsonIndent.detect(text) ==== JsonIndent.Spaces(2).some
  }

  def testNothingToDetect: Result =
    Result.all(
      List(
        JsonIndent.detect("") ==== none[JsonIndent],
        JsonIndent.detect("   ") ==== none[JsonIndent],
        JsonIndent.detect("{}") ==== none[JsonIndent],
        JsonIndent.detect("""{"env":{"ANTHROPIC_BASE_URL":"http://localhost:8888"}}""") ==== none[JsonIndent],
      )
    )

  def testCommentLineCounts: Result = {
    val text = doc("{", "  // the proxy manages the entry below", """  "a": 1""", "}")
    JsonIndent.detect(text) ==== JsonIndent.Spaces(2).some
  }

  def testFromEditorSettings: Result =
    Result.all(
      List(
        JsonIndent.fromEditorSettings(false.some, 2.0.some) ==== JsonIndent.Tabs.some,
        JsonIndent.fromEditorSettings(false.some, none[Double]) ==== JsonIndent.Tabs.some,
        JsonIndent.fromEditorSettings(true.some, 2.0.some) ==== JsonIndent.Spaces(2).some,
        JsonIndent.fromEditorSettings(none[Boolean], 4.0.some) ==== JsonIndent.Spaces(4).some,
        JsonIndent.fromEditorSettings(true.some, none[Double]) ==== none[JsonIndent],
        JsonIndent.fromEditorSettings(none[Boolean], none[Double]) ==== none[JsonIndent],
      )
    )

  def testFromEditorSettingsRejects: Result =
    Result.all(
      List(0.0, -1.0, 2.5, 17.0, Double.PositiveInfinity, Double.NaN).map { tabSize =>
        JsonIndent.fromEditorSettings(none[Boolean], tabSize.some) ==== none[JsonIndent]
      }
    )

  def testFormattingOptionValues: Result =
    Result.all(
      List(
        JsonIndent.Spaces(2).insertSpaces ==== true,
        JsonIndent.Spaces(2).tabSize ==== 2,
        JsonIndent.Tabs.insertSpaces ==== false,
        JsonIndent.Tabs.tabSize ==== JsonIndent.TabsTabSize,
      )
    )

  def testReindentAnyDamage: Property =
    for {
      width  <- genWidth.log("width")
      damage <- Gen.int(Range.linear(0, 12)).log("damage")
    } yield {
      val text     = doc("{", """  "a": 1,""", s"""${" " * damage}"env": {""", "  },", "}")
      val expected = doc("{", """  "a": 1,""", s"""${" " * width}"env": {""", "  },", "}")
      JsonIndent.reindentLine(text, text.indexOf("\"env\""), JsonIndent.Spaces(width), level = 1) ==== expected
    }

  def testReindentIssue44File: Result = {
    val text     = doc("{", """  "cleanupPeriodDays": 36500,""", """"env": {""", "  },", "}")
    val expected = doc("{", """  "cleanupPeriodDays": 36500,""", """  "env": {""", "  },", "}")
    JsonIndent.reindentLine(text, text.indexOf("\"env\""), JsonIndent.Spaces(2), level = 1) ==== expected
  }

  def testReindentCrlf: Result = {
    val text     = crlfDoc("{", """  "cleanupPeriodDays": 36500,""", """"env": {""", "  },", "}")
    val expected = crlfDoc("{", """  "cleanupPeriodDays": 36500,""", """  "env": {""", "  },", "}")
    JsonIndent.reindentLine(text, text.indexOf("\"env\""), JsonIndent.Spaces(2), level = 1) ==== expected
  }

  def testReindentTabs: Result = {
    val text     = doc("{", """  "cleanupPeriodDays": 36500,""", """"env": {""", "  },", "}")
    val expected = doc("{", """  "cleanupPeriodDays": 36500,""", "\t\"env\": {", "  },", "}")
    JsonIndent.reindentLine(text, text.indexOf("\"env\""), JsonIndent.Tabs, level = 1) ==== expected
  }

  def testReindentNoOp: Result = {
    val text = doc("{", """  "cleanupPeriodDays": 36500,""", """  "env": {""", "  },", "}")
    JsonIndent.reindentLine(text, text.indexOf("\"env\""), JsonIndent.Spaces(2), level = 1) ==== text
  }

  def testReindentContentBefore: Result = {
    val text = """{"a": 1, "env": {}}"""
    JsonIndent.reindentLine(text, text.indexOf("\"env\""), JsonIndent.Spaces(2), level = 1) ==== text
  }

  def testReindentOutOfRange: Result = {
    val text = doc("{", """"env": {""", "  },", "}")
    Result.all(
      List(-1, text.length + 1).map { offset =>
        JsonIndent.reindentLine(text, offset, JsonIndent.Spaces(2), level = 1) ==== text
      }
    )
  }

  def testUnit: Result =
    Result.all(
      List(
        JsonIndent.Spaces(2).unit ==== "  ",
        JsonIndent.Spaces(4).unit ==== "    ",
        JsonIndent.Tabs.unit ==== "\t",
      )
    )
}
