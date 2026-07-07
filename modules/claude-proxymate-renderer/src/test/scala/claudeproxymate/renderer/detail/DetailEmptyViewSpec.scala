package claudeproxymate.renderer.detail

import hedgehog.*
import hedgehog.runner.*

object DetailEmptyViewSpec extends Properties {

  override def tests: List[Test] = List(
    example("buildFrag contains proxy-empty class", testProxyEmptyClass),
    example("buildFrag renders title text", testTitleText),
    example("buildFrag renders {{br}} hint as <br />", testHintBrToken),
    example("buildFrag with no tokens renders hint as plain text", testHintPlain),
    property("<script> in title never leaks raw", testNoScriptLeakInTitle),
    property("<script> in hint never leaks raw", testNoScriptLeakInHint),
  )

  private def render(title: String, hint: String): String =
    DetailEmptyView.buildFrag(DetailEmptyLabels(title, hint)).render

  def testProxyEmptyClass: Result =
    Result
      .assert(render("t", "h").contains("class=\"proxy-empty\""))
      .log(render("t", "h"))

  def testTitleText: Result =
    Result
      .assert(render("Select a request", "h").contains("Select a request"))
      .log(render("Select a request", "h"))

  def testHintBrToken: Result = {
    val out = render("t", "line one{{br}}line two")
    Result.all(
      List(
        Result.assert(out.contains("<br />")).log(s"<br /> missing: $out"),
        Result.assert(!out.contains("{{br}}")).log(s"literal {{br}} leaked: $out"),
        Result.assert(out.contains("line one")).log(s"line one missing: $out"),
        Result.assert(out.contains("line two")).log(s"line two missing: $out"),
      )
    )
  }

  def testHintPlain: Result = {
    val out = render("t", "plain hint")
    Result.all(
      List(
        Result.assert(out.contains("plain hint")).log("hint missing"),
        Result.assert(!out.contains("<br />")).log("unexpected <br />"),
      )
    )
  }

  def testNoScriptLeakInTitle: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val out     = render(payload, "h")
      Result
        .assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakInHint: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val out     = render("t", payload)
      Result
        .assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }
}
