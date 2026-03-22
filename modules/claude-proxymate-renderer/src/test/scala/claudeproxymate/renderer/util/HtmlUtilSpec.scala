package claudeproxymate.renderer.util

import hedgehog.*
import hedgehog.runner.*

object HtmlUtilSpec extends Properties {

  override def tests: List[Test] = List(
    example("esc replaces & < >", testEscBasic),
    example("esc handles empty string", testEscEmpty),
    property("esc is idempotent on safe strings", testEscSafe),
    example("escAttr also replaces double quotes", testEscAttr),
    example("highlightSearch returns original when query is empty", testHighlightEmpty),
    example("highlightSearch wraps matches in <mark>", testHighlightMatch),
    example("highlightSearch is case-insensitive", testHighlightCaseInsensitive),
    example("highlightSearch escapes regex metacharacters in query", testHighlightRegexMeta),
  )

  def testEscBasic: Result = {
    val input    = """<script>alert("x&y")</script>"""
    val expected = "&lt;script&gt;alert(\"x&amp;y\")&lt;/script&gt;"
    HtmlUtil.esc(input) ==== expected
  }

  def testEscEmpty: Result =
    HtmlUtil.esc("") ==== ""

  def testEscSafe: Property =
    for {
      s <- Gen.string(Gen.alpha, Range.linear(0, 50)).log("s")
    } yield HtmlUtil.esc(s) ==== s

  def testEscAttr: Result = {
    val input    = """He said "hello" & <goodbye>"""
    val expected = "He said &quot;hello&quot; &amp; &lt;goodbye&gt;"
    HtmlUtil.escAttr(input) ==== expected
  }

  def testHighlightEmpty: Result = {
    val html = "<b>test</b>"
    HtmlUtil.highlightSearch(html, "") ==== html
  }

  def testHighlightMatch: Result = {
    val html     = "hello world hello"
    val expected = """<mark class="search-hl">hello</mark> world <mark class="search-hl">hello</mark>"""
    HtmlUtil.highlightSearch(html, "hello") ==== expected
  }

  def testHighlightCaseInsensitive: Result = {
    val html   = "Hello HELLO hello"
    val result = HtmlUtil.highlightSearch(html, "hello")
    Result.all(
      List(
        Result.assert(result.contains("""<mark class="search-hl">Hello</mark>"""))
          .log(s"should wrap Hello, got: $result"),
        Result.assert(result.contains("""<mark class="search-hl">HELLO</mark>"""))
          .log(s"should wrap HELLO, got: $result"),
        Result.assert(result.contains("""<mark class="search-hl">hello</mark>"""))
          .log(s"should wrap hello, got: $result"),
      )
    )
  }

  def testHighlightRegexMeta: Result = {
    val html   = "price is $100.00 (USD)"
    val result = HtmlUtil.highlightSearch(html, "$100.00")
    Result.assert(result.contains("""<mark class="search-hl">$100.00</mark>"""))
      .log(s"should match literal $$100.00, got: $result")
  }
}
