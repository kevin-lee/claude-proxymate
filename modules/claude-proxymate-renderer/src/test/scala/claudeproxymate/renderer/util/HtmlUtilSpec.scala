package claudeproxymate.renderer.util

import hedgehog.*
import hedgehog.runner.*

object HtmlUtilSpec extends Properties {

  override def tests: List[Test] = List(
    example("esc replaces & < >", testEscBasic),
    example("esc handles empty string", testEscEmpty),
    property("esc is idempotent on safe strings", testEscSafe),
    example("esc escapes double quotes to &quot;", testEscDoubleQuote),
    example("esc escapes single quotes to &#39;", testEscSingleQuote),
    example("esc handles apostrophes in mixed content", testEscApostropheMixed),
    example("esc strips control characters below space", testEscControlChars),
    example("esc preserves \\t, \\n, \\r", testEscTabNewlineCR),
    property("esc output never contains literal ' after escape", testEscNoLiteralSingleQuote),
    property("esc output never contains literal \" after escape", testEscNoLiteralDoubleQuote),
    property("esc output never contains literal < after escape", testEscNoLiteralLt),
    example("escAttr output matches original attribute-escape expectation", testEscAttrOutput),
    example("escAttr is equivalent to esc", testEscAttrEquivalentToEsc),
    example("highlightSearch returns original when query is empty", testHighlightEmpty),
    example("highlightSearch wraps matches in <mark>", testHighlightMatch),
    example("highlightSearch is case-insensitive", testHighlightCaseInsensitive),
    example("highlightSearch escapes regex metacharacters in query", testHighlightRegexMeta),
  )

  def testEscBasic: Result = {
    val input    = """<script>alert("x&y")</script>"""
    val expected = "&lt;script&gt;alert(&quot;x&amp;y&quot;)&lt;/script&gt;"
    HtmlUtil.esc(input) ==== expected
  }

  def testEscEmpty: Result =
    HtmlUtil.esc("") ==== ""

  def testEscSafe: Property =
    for {
      s <- Gen.string(Gen.alpha, Range.linear(0, 50)).log("s")
    } yield HtmlUtil.esc(s) ==== s

  def testEscDoubleQuote: Result =
    HtmlUtil.esc("\"") ==== "&quot;"

  def testEscSingleQuote: Result =
    HtmlUtil.esc("'") ==== "&#39;"

  def testEscApostropheMixed: Result =
    HtmlUtil.esc("it's a \"test\"") ==== "it&#39;s a &quot;test&quot;"

  def testEscControlChars: Result = {
    // \u0000 through \u001F except \t(\u0009) \n(\u000A) \r(\u000D) are stripped
    val input    = "a\u0000b\u0001c\u001Fd"
    val expected = "abcd"
    HtmlUtil.esc(input) ==== expected
  }

  def testEscTabNewlineCR: Result = {
    val input = "a\tb\nc\rd"
    HtmlUtil.esc(input) ==== input
  }

  def testEscNoLiteralSingleQuote: Property =
    for {
      s <- Gen
             .string(Gen.frequency1(4 -> Gen.alpha, 1 -> Gen.constant('\'')), Range.linear(0, 50))
             .log("s")
    } yield {
      val out = HtmlUtil.esc(s)
      Result.assert(!out.contains('\'')).log(s"out contained ': $out")
    }

  def testEscNoLiteralDoubleQuote: Property =
    for {
      s <- Gen
             .string(Gen.frequency1(4 -> Gen.alpha, 1 -> Gen.constant('"')), Range.linear(0, 50))
             .log("s")
    } yield {
      val out = HtmlUtil.esc(s)
      Result.assert(!out.contains('"')).log(s"out contained \": $out")
    }

  def testEscNoLiteralLt: Property =
    for {
      s <- Gen
             .string(Gen.frequency1(4 -> Gen.alpha, 1 -> Gen.constant('<')), Range.linear(0, 50))
             .log("s")
    } yield {
      val out = HtmlUtil.esc(s)
      Result.assert(!out.contains('<')).log(s"out contained <: $out")
    }

  def testEscAttrOutput: Result = {
    val input    = """He said "hello" & <goodbye>"""
    val expected = "He said &quot;hello&quot; &amp; &lt;goodbye&gt;"
    HtmlUtil.escAttr(input) ==== expected
  }

  def testEscAttrEquivalentToEsc: Result = {
    val input = """He said "hello" & <goodbye>"""
    HtmlUtil.escAttr(input) ==== HtmlUtil.esc(input)
  }

  def testHighlightEmpty: Result = {
    val text = "plain text"
    HtmlUtil.highlightSearchFrag(text, "").render ==== text
  }

  def testHighlightMatch: Result = {
    val text     = "hello world hello"
    val expected = """<mark class="search-hl">hello</mark> world <mark class="search-hl">hello</mark>"""
    HtmlUtil.highlightSearchFrag(text, "hello").render ==== expected
  }

  def testHighlightCaseInsensitive: Result = {
    val text   = "Hello HELLO hello"
    val result = HtmlUtil.highlightSearchFrag(text, "hello").render
    Result.all(
      List(
        Result
          .assert(result.contains("""<mark class="search-hl">Hello</mark>"""))
          .log(s"should wrap Hello, got: $result"),
        Result
          .assert(result.contains("""<mark class="search-hl">HELLO</mark>"""))
          .log(s"should wrap HELLO, got: $result"),
        Result
          .assert(result.contains("""<mark class="search-hl">hello</mark>"""))
          .log(s"should wrap hello, got: $result"),
      )
    )
  }

  def testHighlightRegexMeta: Result = {
    val text   = "price is $100.00 (USD)"
    val result = HtmlUtil.highlightSearchFrag(text, "$100.00").render
    Result
      .assert(result.contains("""<mark class="search-hl">$100.00</mark>"""))
      .log(s"should match literal $$100.00, got: $result")
  }
}
