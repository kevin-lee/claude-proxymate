package claudeproxymate.renderer.view

import hedgehog.*
import hedgehog.runner.*

object I18nTemplateSpec extends Properties {

  override def tests: List[Test] = List(
    example("plain string with no tokens renders as escaped text", testPlain),
    example("br token renders as <br>", testBr),
    example("paired b token renders as <b>...</b>", testB),
    example("all paired tags render correctly", testAllPaired),
    example("nested paired tags render correctly", testNested),
    example("unknown token renders as literal text", testUnknownToken),
    example("raw <script> in input is escaped", testRawScriptEscaped),
    example("unknown void-looking {{script}} renders as literal text", testScriptLookalike),
    example("unmatched open tag keeps literal tail", testUnmatchedOpen),
    example("empty string renders empty frag", testEmpty),
    example("mismatched close renders as literal", testMismatchedClose),
    example("br at start/middle/end renders three <br>s", testBrPositions),
    example("adjacent paired tags render back-to-back", testAdjacentPaired),
    property("random text between known tokens never leaks <script>", testNoScriptLeak),
    property("every whitelist tag wraps plain text correctly", testWhitelistRoundTrip),
  )

  private def r(s: String): String = I18nTemplate.render(s).render

  def testPlain: Result =
    r("hello world") ==== "hello world"

  def testBr: Result =
    r("a{{br}}b") ==== "a<br />b"

  def testB: Result =
    r("{{b}}bold{{/b}}") ==== "<b>bold</b>"

  def testAllPaired: Result = {
    Result.all(
      List(
        r("{{b}}x{{/b}}") ==== "<b>x</b>",
        r("{{strong}}x{{/strong}}") ==== "<strong>x</strong>",
        r("{{i}}x{{/i}}") ==== "<i>x</i>",
        r("{{small}}x{{/small}}") ==== "<small>x</small>",
        r("{{code}}x{{/code}}") ==== "<code>x</code>",
      )
    )
  }

  def testNested: Result =
    r("{{b}}{{i}}bi{{/i}}{{/b}}") ==== "<b><i>bi</i></b>"

  def testUnknownToken: Result = {
    val out = r("{{foo}}x{{/foo}}")
    Result.all(
      List(
        Result.assert(out.contains("{{foo}}")).log(s"opening not literal: $out"),
        Result.assert(out.contains("{{/foo}}")).log(s"closing not literal: $out"),
        Result.assert(!out.contains("<foo")).log(s"leaked tag: $out"),
      )
    )
  }

  def testRawScriptEscaped: Result = {
    val out = r("<script>alert(1)</script>")
    Result.all(
      List(
        Result.assert(!out.contains("<script>")).log(s"raw leaked: $out"),
        Result.assert(out.contains("&lt;script&gt;")).log(s"not escaped: $out"),
      )
    )
  }

  def testScriptLookalike: Result = {
    val out = r("{{script}}alert(1){{/script}}")
    Result.all(
      List(
        Result.assert(out.contains("{{script}}")).log(s"not literal: $out"),
        Result.assert(!out.contains("<script>")).log(s"leaked tag: $out"),
      )
    )
  }

  def testUnmatchedOpen: Result = {
    val out = r("{{b}}no close here")
    // Scanner consumes until EOS without finding {{/b}} — emits <b>...</b>
    // containing the literal tail. Contract: no raw <script> leaks; output
    // must include the inner text.
    Result.assert(out.contains("no close here")).log(out)
  }

  def testEmpty: Result =
    r("") ==== ""

  def testMismatchedClose: Result = {
    val out = r("{{b}}x{{/i}}y{{/b}}")
    // Inner {{/i}} doesn't match the open {{b}}; renders as literal.
    Result.all(
      List(
        Result.assert(out.contains("{{/i}}")).log(s"missing literal close: $out"),
        Result.assert(out.startsWith("<b>")).log(s"missing b open: $out"),
        Result.assert(out.endsWith("</b>")).log(s"missing b close: $out"),
      )
    )
  }

  def testBrPositions: Result = {
    val out     = r("{{br}}a{{br}}b{{br}}")
    val brCount = out.split("<br />", -1).length - 1
    Result.all(
      List(
        brCount ==== 3,
        Result.assert(out.contains("a")).log(s"a missing: $out"),
        Result.assert(out.contains("b")).log(s"b missing: $out"),
      )
    )
  }

  def testAdjacentPaired: Result =
    r("{{b}}a{{/b}}{{i}}b{{/i}}") ==== "<b>a</b><i>b</i>"

  def testNoScriptLeak: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val out     = r(s"{{b}}$payload{{/b}}{{br}}$payload")
      Result
        .assert(!out.contains("<script>"))
        .log(s"leaked for chunk=$chunk out=$out")
    }

  def testWhitelistRoundTrip: Property =
    for {
      inner <- Gen.string(Gen.alpha, Range.linear(0, 8)).log("inner")
      tag   <- Gen.element1("b", "strong", "i", "small", "code").log("tag")
    } yield {
      val out      = r(s"{{$tag}}$inner{{/$tag}}")
      val expected = s"<$tag>$inner</$tag>"
      out ==== expected
    }
}
