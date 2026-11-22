package claudeproxymate.renderer.analysis

import cats.syntax.all.*
import claudeproxymate.renderer.json.JsonTreeViewer
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*
import org.scalajs.dom

import scala.scalajs.js

/** [[MechHighlight]] in jsdom, on JSON trees made by
  * `JsonTreeViewer.renderJsonTree`.
  */
object MechHighlightSpec extends Properties {

  override def tests: List[Test] = List(
    /* highlightMechInJsonTree: early exits */
    example("a null mechanism key highlights nothing", testNullKey),
    example("an empty mechanism key highlights nothing", testEmptyKey),
    example("cm with no CLAUDE.md highlights nothing", testCmNoClaudeMd),
    example("cm with a non-number index highlights nothing", testCmBadIndex),
    example("cm with an out-of-range index highlights nothing", testCmOutOfRange),
    example("cm with a negative index highlights nothing", testCmNegative),
    example("sc with an out-of-range index highlights nothing", testScOutOfRange),
    example("sk with no skills highlights nothing", testSkNone),
    example("sa with no sub-agents highlights nothing", testSaNone),
    example("mc with no MCP tools highlights nothing", testMcNone),
    /* highlightMechInJsonTree: highlights */
    example("cm_0 highlights the lines of the first CLAUDE.md section", testCmFirstSection),
    example("cm_1 highlights the lines of the second CLAUDE.md section", testCmSecondSection),
    example("sc_0 highlights the first command message", testScFirst),
    example("sc_1 highlights the second command message with the same tag", testScSecond),
    example("sk highlights the Skill tool_use row", testSk),
    example("sa highlights the sub-agent tool_use row", testSa),
    example("mc_1 highlights the second MCP tool_use row", testMcSecond),
    example("mc with an out-of-range index highlights the first MCP tool_use row", testMcOutOfRange),
    /* highlightToolUseById */
    example("highlightToolUseById highlights only the first row with the id", testToolUseFirstOnly),
    example("highlightToolUseById with an unknown id highlights nothing", testToolUseUnknown),
    example("highlightToolUseById skips a string that is not a tool_use id", testToolUseNotARow),
    /* applyDetailHighlight */
    example("applyDetailHighlight with a null query marks nothing", testHighlightNull),
    example("applyDetailHighlight with an empty query marks nothing", testHighlightEmpty),
    example("applyDetailHighlight marks every match, ignoring case", testHighlightMatches),
    example("applyDetailHighlight matches regex characters as text", testHighlightRegexChars),
  )

  private val Padding: String = "p" * 300

  /* jsdom gives the tree no width, so lines longer than 40 characters are
   * split: the header lines here are shorter. */
  private def claudeMdText: String =
    s"<system-reminder>\\nContents of /a/C.md (project):\\n\\nline a1\\nline a2\\nContents of /b/C.md (user):\\n\\nline b1\\n$Padding</system-reminder>"

  private def userMessage(text: String): String =
    s"""{"role":"user","content":"$text"}"""

  private def toolUse(id: String, name: String): String =
    s"""{"type":"tool_use","id":"$id","name":"$name","input":{"skill":"x"}}"""

  private def assistant(blocks: String*): String =
    s"""{"role":"assistant","content":[${blocks.mkString(",")}]}"""

  private def bodyOf(messages: String*): js.Dynamic =
    js.JSON.parse(s"""{"messages":[${messages.mkString(",")}]}""")

  private def render(data: js.Dynamic): dom.html.Element = {
    DomTestKit.setUp("""<div id="tree"></div>""")
    val container = DomTestKit.html("#tree")
    JsonTreeViewer.renderJsonTree(container, data)
    container
  }

  private def highlightedLines(container: dom.Element): List[String] = {
    val nl = container.querySelectorAll(".jt-exp-line.mech-hl-text")
    (0 until nl.length).toList.map(i => nl(i).textContent)
  }

  private def highlightedRows(container: dom.Element): Int =
    container.querySelectorAll(".mech-hl-row").length

  private def textHighlights(container: dom.Element): List[String] = {
    val nl = container.querySelectorAll("span.mech-hl-text")
    (0 until nl.length).toList.map(i => nl(i).textContent)
  }

  private def nothingHighlighted(container: dom.Element): Result =
    Result.all(
      List(
        container.querySelectorAll(".mech-hl-text, .mech-hl-row").length ==== 0,
        DomTestKit.runAnimationFrames() ==== 0,
      )
    )

  private def cmBody: js.Dynamic = bodyOf(userMessage(claudeMdText))

  private def scBody: js.Dynamic =
    bodyOf(
      userMessage("<command-message>review</command-message> first"),
      userMessage("<command-message>review</command-message> second"),
    )

  private def toolBody: js.Dynamic =
    bodyOf(
      assistant(
        toolUse("tu_sk", "Skill"),
        toolUse("tu_sa", "Task"),
        toolUse("tu_m0", "mcp__a__one"),
        toolUse("tu_m1", "mcp__b__two"),
      )
    )

  private def rowWithId(container: dom.Element, id: String): Boolean = {
    val rows = container.querySelectorAll(".mech-hl-row")
    (0 until rows.length).exists(i => rows(i).textContent.contains(s"\"$id\""))
  }

  // ── early exits ─────────────────────────────────────────────────────────

  def testNullKey: Result = {
    val c = render(cmBody)
    MechHighlight.highlightMechInJsonTree(c, cmBody, null) // scalafix:ok DisableSyntax.null
    nothingHighlighted(c)
  }

  def testEmptyKey: Result = {
    val c = render(cmBody)
    MechHighlight.highlightMechInJsonTree(c, cmBody, "")
    nothingHighlighted(c)
  }

  def testCmNoClaudeMd: Result = {
    val c = render(scBody)
    MechHighlight.highlightMechInJsonTree(c, scBody, "cm_0")
    nothingHighlighted(c)
  }

  def testCmBadIndex: Result = {
    val c = render(cmBody)
    MechHighlight.highlightMechInJsonTree(c, cmBody, "cm_x")
    nothingHighlighted(c)
  }

  def testCmOutOfRange: Result = {
    val c = render(cmBody)
    MechHighlight.highlightMechInJsonTree(c, cmBody, "cm_2")
    nothingHighlighted(c)
  }

  def testCmNegative: Result = {
    val c = render(cmBody)
    MechHighlight.highlightMechInJsonTree(c, cmBody, "cm_-1")
    nothingHighlighted(c)
  }

  def testScOutOfRange: Result = {
    val c = render(scBody)
    MechHighlight.highlightMechInJsonTree(c, scBody, "sc_2")
    nothingHighlighted(c)
  }

  def testSkNone: Result = {
    val c = render(scBody)
    MechHighlight.highlightMechInJsonTree(c, scBody, "sk_0")
    nothingHighlighted(c)
  }

  def testSaNone: Result = {
    val c = render(scBody)
    MechHighlight.highlightMechInJsonTree(c, scBody, "sa")
    nothingHighlighted(c)
  }

  def testMcNone: Result = {
    val c = render(scBody)
    MechHighlight.highlightMechInJsonTree(c, scBody, "mc_0")
    nothingHighlighted(c)
  }

  // ── highlights ──────────────────────────────────────────────────────────

  def testCmFirstSection: Result = {
    val c = render(cmBody)
    MechHighlight.highlightMechInJsonTree(c, cmBody, "cm_0")
    Result.all(
      List(
        highlightedLines(c) ==== List("Contents of /a/C.md (project):", "", "line a1", "line a2"),
        Result.assert(DomTestKit.runAnimationFrames() >= 1).log("a scroll is scheduled"),
      )
    )
  }

  def testCmSecondSection: Result = {
    val c = render(cmBody)
    MechHighlight.highlightMechInJsonTree(c, cmBody, "cm_1")
    highlightedLines(c).take(3) ==== List("Contents of /b/C.md (user):", "", "line b1")
  }

  def testScFirst: Result = {
    val c  = render(scBody)
    MechHighlight.highlightMechInJsonTree(c, scBody, "sc_0")
    val hl = c.querySelectorAll("span.mech-hl-text")
    Result.all(
      List(
        textHighlights(c) ==== List("<command-message>review</command-message>"),
        Result.assert(hl.length === 1 && hl(0).parentNode.textContent.contains("first")).log("in the first message"),
        DomTestKit.runAnimationFrames() ==== 1,
      )
    )
  }

  def testScSecond: Result = {
    val c  = render(scBody)
    MechHighlight.highlightMechInJsonTree(c, scBody, "sc_1")
    val hl = c.querySelectorAll("span.mech-hl-text")
    Result.all(
      List(
        textHighlights(c) ==== List("<command-message>review</command-message>"),
        Result.assert(hl.length === 1 && hl(0).parentNode.textContent.contains("second")).log("in the second message"),
      )
    )
  }

  def testSk: Result = {
    val c = render(toolBody)
    MechHighlight.highlightMechInJsonTree(c, toolBody, "sk_0")
    Result.all(
      List(
        highlightedRows(c) ==== 1,
        Result.assert(rowWithId(c, "tu_sk")).log("the Skill row"),
        DomTestKit.runAnimationFrames() ==== 1,
      )
    )
  }

  def testSa: Result = {
    val c = render(toolBody)
    MechHighlight.highlightMechInJsonTree(c, toolBody, "sa")
    Result.all(List(highlightedRows(c) ==== 1, Result.assert(rowWithId(c, "tu_sa")).log("the sub-agent row")))
  }

  def testMcSecond: Result = {
    val c = render(toolBody)
    MechHighlight.highlightMechInJsonTree(c, toolBody, "mc_1")
    Result.all(List(highlightedRows(c) ==== 1, Result.assert(rowWithId(c, "tu_m1")).log("the second MCP row")))
  }

  def testMcOutOfRange: Result = {
    val c = render(toolBody)
    MechHighlight.highlightMechInJsonTree(c, toolBody, "mc_9")
    Result.all(List(highlightedRows(c) ==== 1, Result.assert(rowWithId(c, "tu_m0")).log("the first MCP row")))
  }

  // ── highlightToolUseById ────────────────────────────────────────────────

  def testToolUseFirstOnly: Result = {
    val data = bodyOf(assistant(toolUse("tu_dup", "Task"), toolUse("tu_dup", "Task")))
    val c    = render(data)
    MechHighlight.highlightToolUseById(c, "tu_dup")
    Result.all(List(highlightedRows(c) ==== 1, DomTestKit.runAnimationFrames() ==== 1))
  }

  def testToolUseUnknown: Result = {
    val c = render(toolBody)
    MechHighlight.highlightToolUseById(c, "tu_nope")
    nothingHighlighted(c)
  }

  def testToolUseNotARow: Result = {
    /* "tu_x" is first a plain string value (not in a tool_use block), then
     * the id of a tool_use block. */
    val data = js.JSON.parse(s"""{"note":"tu_x","messages":[${assistant(toolUse("tu_x", "Task"))}]}""")
    val c    = render(data)
    MechHighlight.highlightToolUseById(c, "tu_x")
    Result.all(List(highlightedRows(c) ==== 1, Result.assert(rowWithId(c, "tu_x")).log("the tool_use row")))
  }

  // ── applyDetailHighlight ────────────────────────────────────────────────

  private def marks(container: dom.Element): List[String] = {
    val nl = container.querySelectorAll("mark.search-hl")
    (0 until nl.length).toList.map(i => nl(i).textContent)
  }

  def testHighlightNull: Result = {
    DomTestKit.setUp("""<div id="c">foo bar</div>""")
    MechHighlight.applyDetailHighlight(DomTestKit.element("#c"), null) // scalafix:ok DisableSyntax.null
    marks(DomTestKit.element("#c")) ==== Nil
  }

  def testHighlightEmpty: Result = {
    DomTestKit.setUp("""<div id="c">foo bar</div>""")
    MechHighlight.applyDetailHighlight(DomTestKit.element("#c"), "")
    marks(DomTestKit.element("#c")) ==== Nil
  }

  def testHighlightMatches: Result = {
    DomTestKit.setUp("""<div id="c">Foo bar <b>foo</b></div>""")
    MechHighlight.applyDetailHighlight(DomTestKit.element("#c"), "foo")
    Result.all(
      List(
        marks(DomTestKit.element("#c")) ==== List("Foo", "foo"),
        DomTestKit.element("#c").textContent ==== "Foo bar foo",
      )
    )
  }

  def testHighlightRegexChars: Result = {
    DomTestKit.setUp("""<div id="c">a.b axb</div>""")
    MechHighlight.applyDetailHighlight(DomTestKit.element("#c"), "a.b")
    marks(DomTestKit.element("#c")) ==== List("a.b")
  }
}
