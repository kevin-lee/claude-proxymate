package claudeproxymate.renderer.json

import claudeproxymate.renderer.state.AppState
import hedgehog.*
import hedgehog.runner.*

import scala.scalajs.js

object JsonTreeViewSpec extends Properties {

  override def tests: List[Test] = List(
    // Primitive rendering
    example("null renders as jt-null span", testNull),
    example("boolean renders as jt-bool span", testBool),
    example("integer renders as jt-num span", testIntNum),
    example("float renders as jt-num span", testFloatNum),
    example("short string renders as jt-str span with literal quotes", testShortStr),
    // String escaping & long-string structure
    example("short string escapes <script>", testShortStrEscape),
    example("long string builds jt-str-long envelope", testLongStrEnvelope),
    example("long string preview is 80 chars + ellipsis", testLongStrPreviewLength),
    example("long string expanded splits on newlines", testLongStrExpandedLines),
    example("long string carries data-jt-str-id on all three carriers", testLongStrDataAttr),
    example("long string with <script> escapes properly in expanded", testLongStrExpandedEscape),
    // Container rendering
    example("empty array renders []", testEmptyArr),
    example("empty object renders {}", testEmptyObj),
    example("single-element array renders one jt-row", testSingleArr),
    example("single-key object renders jt-key + colon", testSingleObj),
    example("multi-element array has comma between non-last entries", testMultiArrComma),
    example("multi-key object has comma between non-last entries", testMultiObjComma),
    example("nested object/array recursion produces unique data-jt-id values", testNestedUniqueIds),
    example("container has data-jt-id matching toggle button id", testContainerIdConsistency),
    // XSS regression
    property("<script> in object key never leaks raw", testNoScriptLeakInKey),
    property("<script> in string value never leaks raw", testNoScriptLeakInValue),
    property("<script> deeply nested never leaks raw", testNoScriptLeakDeep),
    // Counter behaviour
    example("AppState.jtLine after rendering 5-element array equals 6", testJtLineCounter),
    example("AppState.jtId increments per non-empty container and per long-string", testJtIdCounter),
    // Token / percentage display
    example("jt-tok span renders ~Xtok form", testJtTokSpanShape),
    example("tokens >= 1000 render as K form", testJtTokKForm),
    example("tokens >= 1000000 render as M form", testJtTokMForm),
    // Click-attr presence regression
    example("container output has no inline onclick", testNoContainerInlineOnclick),
    example("long-string output has no inline onclick", testNoStringInlineOnclick),
  )

  private def reset(): Unit = {
    AppState.jtId = 0
    AppState.jtLine = 0
  }

  private def render(value: js.Dynamic, totalBytes: Int = 0): String = {
    reset()
    val effectiveTotal = if (totalBytes == 0) js.JSON.stringify(value).length else totalBytes
    JsonTreeView.buildJsonFrag(value, effectiveTotal).render
  }

  private def parse(json: String): js.Dynamic = js.JSON.parse(json)

  // ── Primitive rendering ────────────────────────────────────────────────

  def testNull: Result = {
    val out = render(parse("null"))
    Result.all(
      List(
        Result.assert(out.contains("class=\"jt-null\"")).log(out),
        Result.assert(out.contains(">null<")).log(out),
      )
    )
  }

  def testBool: Result = {
    val outT = render(parse("true"))
    val outF = render(parse("false"))
    Result.all(
      List(
        Result.assert(outT.contains("class=\"jt-bool\"")).log(outT),
        Result.assert(outT.contains(">true<")).log(outT),
        Result.assert(outF.contains(">false<")).log(outF),
      )
    )
  }

  def testIntNum: Result = {
    val out = render(parse("42"))
    Result.all(
      List(
        Result.assert(out.contains("class=\"jt-num\"")).log(out),
        Result.assert(out.contains(">42<")).log(out),
      )
    )
  }

  def testFloatNum: Result = {
    val outFloat = render(parse("3.14"))
    val outZero  = render(parse("0"))
    val outNeg   = render(parse("-7"))
    Result.all(
      List(
        Result.assert(outFloat.contains(">3.14<")).log(outFloat),
        Result.assert(outZero.contains(">0<")).log(outZero),
        Result.assert(outNeg.contains(">-7<")).log(outNeg),
      )
    )
  }

  def testShortStr: Result = {
    val out = render(parse("\"hello\""))
    // Scalatags escapes literal " as &quot; in text content; that's correct
    // — browsers render &quot; as " on screen.
    Result.all(
      List(
        Result.assert(out.contains("class=\"jt-str\"")).log(out),
        Result.assert(out.contains("&quot;hello&quot;")).log(out),
      )
    )
  }

  // ── String escaping & long-string structure ────────────────────────────

  def testShortStrEscape: Result = {
    val payload = "<script>alert(1)</script>"
    val out     = render(parse(js.JSON.stringify(payload)))
    Result.all(
      List(
        Result.assert(!out.contains("<script>")).log(s"raw <script> leaked: $out"),
        Result.assert(out.contains("&lt;script&gt;")).log(s"not escaped: $out"),
      )
    )
  }

  def testLongStrEnvelope: Result = {
    val long = "a" * 301
    val out  = render(parse(js.JSON.stringify(long)))
    Result.all(
      List(
        Result.assert(out.contains("class=\"jt-str-long\"")).log("envelope missing"),
        Result.assert(out.contains("class=\"jt-str-toggle\"")).log("toggle missing"),
        Result.assert(out.contains("class=\"jt-str-preview\"")).log("preview missing"),
        Result.assert(out.contains("class=\"jt-str-expanded\"")).log("expanded missing"),
      )
    )
  }

  def testLongStrPreviewLength: Result = {
    val long = ("x" * 80) + ("y" * 250)
    val out  = render(parse(js.JSON.stringify(long)))
    val previewExpected = ("x" * 80) + "…"
    Result.all(
      List(
        Result.assert(out.contains(previewExpected)).log(s"preview not '$previewExpected' in: $out"),
        Result.assert(out.contains(s"(${long.length} chars)")).log(s"chars count missing: $out"),
      )
    )
  }

  def testLongStrExpandedLines: Result = {
    // Build a JSON string containing literal \n newline escapes, total length > threshold
    val pad   = "z" * 305
    val value = "line1\\nline2\\n" + pad
    val out   = render(parse(js.JSON.stringify(value)))
    val expCount = out.split("class=\"jt-exp-line\"", -1).length - 1
    Result.all(
      List(
        Result.assert(expCount >= 3).log(s"expected >= 3 jt-exp-line, got $expCount in: $out"),
        Result.assert(out.contains("line1")).log("line1 missing"),
        Result.assert(out.contains("line2")).log("line2 missing"),
      )
    )
  }

  def testLongStrDataAttr: Result = {
    val long = "a" * 301
    val out  = render(parse(js.JSON.stringify(long)))
    val occurrences = out.split("data-jt-str-id=\"jts1\"", -1).length - 1
    // Three carriers: -btn span, -s preview, expanded toggle
    Result.assert(occurrences == 3)
      .log(s"expected 3 data-jt-str-id occurrences for 'jts1', got $occurrences in: $out")
  }

  def testLongStrExpandedEscape: Result = {
    val payload = ("<script>" * 50) // length 400, well over threshold
    val out     = render(parse(js.JSON.stringify(payload)))
    Result.all(
      List(
        Result.assert(!out.contains("<script>")).log(s"raw <script> leaked: $out"),
        Result.assert(out.contains("&lt;script&gt;")).log(s"not escaped: $out"),
      )
    )
  }

  // ── Container rendering ────────────────────────────────────────────────

  def testEmptyArr: Result = {
    val out = render(parse("[]"))
    Result.all(
      List(
        Result.assert(out.contains("[]")).log(s"missing []: $out"),
        Result.assert(!out.contains("data-jt-id")).log(s"unexpected data-jt-id: $out"),
      )
    )
  }

  def testEmptyObj: Result = {
    val out = render(parse("{}"))
    Result.all(
      List(
        Result.assert(out.contains("{}")).log(s"missing {}: $out"),
        Result.assert(!out.contains("data-jt-id")).log(s"unexpected data-jt-id: $out"),
      )
    )
  }

  def testSingleArr: Result = {
    val out = render(parse("[1]"))
    val rowCount = out.split("class=\"jt-row\"", -1).length - 1
    Result.all(
      List(
        // One row for the element + one row for the closing bracket = 2
        Result.assert(rowCount == 2).log(s"expected 2 jt-row, got $rowCount: $out"),
        Result.assert(out.contains(">1<")).log(s"value 1 missing: $out"),
      )
    )
  }

  def testSingleObj: Result = {
    val out = render(parse("""{"a":1}"""))
    Result.all(
      List(
        Result.assert(out.contains("class=\"jt-key\"")).log(s"jt-key missing: $out"),
        Result.assert(out.contains("&quot;a&quot;")).log(s"key 'a' missing: $out"),
        Result.assert(out.contains(": ")).log(s"colon-space missing: $out"),
      )
    )
  }

  def testMultiArrComma: Result = {
    val out = render(parse("[1,2,3]"))
    val commaCount = out.split(">,<", -1).length - 1
    // Two non-last entries (positions 0,1) emit commas; closing bracket has no comma at root.
    Result.assert(commaCount == 2)
      .log(s"expected 2 commas, got $commaCount: $out")
  }

  def testMultiObjComma: Result = {
    val out = render(parse("""{"a":1,"b":2,"c":3}"""))
    val commaCount = out.split(">,<", -1).length - 1
    Result.assert(commaCount == 2)
      .log(s"expected 2 commas, got $commaCount: $out")
  }

  def testNestedUniqueIds: Result = {
    val out = render(parse("""[{"a":[1]},{"b":2}]"""))
    val ids = "data-jt-id=\"(jt\\d+)\"".r.findAllMatchIn(out).map(_.group(1)).toSet
    // outer array + first inner object + first inner array + second inner object = 4
    Result.assert(ids.size == 4).log(s"expected 4 unique container ids, got ${ids.size} ($ids): $out")
  }

  def testContainerIdConsistency: Result = {
    val out = render(parse("[1]"))
    Result.all(
      List(
        Result.assert(out.contains("data-jt-id=\"jt1\"")).log(s"data-jt-id missing: $out"),
        Result.assert(out.contains("id=\"jt1-btn\"")).log(s"toggle btn id missing: $out"),
        Result.assert(out.contains("id=\"jt1-s\"")).log(s"summary id missing: $out"),
        Result.assert(out.contains("id=\"jt1-b\"")).log(s"body id missing: $out"),
      )
    )
  }

  // ── XSS regression ─────────────────────────────────────────────────────

  def testNoScriptLeakInKey: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val key  = s"<script>$chunk</script>"
      val json = js.Dynamic.literal()
      json.updateDynamic(key)("v")
      val out = render(json)
      Result.assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakInValue: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val json    = js.Dynamic.literal(k = payload)
      val out     = render(json)
      Result.assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakDeep: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val deep    = js.Dynamic.literal(a = js.Dynamic.literal(b = js.Array(payload)))
      val out     = render(deep)
      Result.assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  // ── Counter behaviour ──────────────────────────────────────────────────

  def testJtLineCounter: Result = {
    val _ = render(parse("[1,2,3,4,5]"))
    // 5 element rows + 1 closing bracket row = 6
    AppState.jtLine ==== 6
  }

  def testJtIdCounter: Result = {
    // One container + one long string within it
    val long = "x" * 301
    val _    = render(js.Dynamic.literal(s = long))
    AppState.jtId ==== 2
  }

  // ── Token / percentage display ─────────────────────────────────────────

  def testJtTokSpanShape: Result = {
    val out = render(parse("[1,2,3]"))
    Result.all(
      List(
        Result.assert(out.contains("class=\"jt-tok\"")).log(s"jt-tok missing: $out"),
        Result.assert(out.contains("tok ·")).log(s"tok separator missing: $out"),
      )
    )
  }

  def testJtTokKForm: Result = {
    // jt-tok rendering only happens for containers; wrap the bulky payload
    // in a single-key object. A ~3500-byte object yields ~1000 tokens.
    val big = "a" * 3490
    val out = render(js.Dynamic.literal(s = big))
    Result.assert(out.contains("K tok")).log(s"K form missing: ${out.take(800)}…")
  }

  def testJtTokMForm: Result = {
    // 3.5M bytes / 3.5 = 1M tokens -> "1.0M"
    // But: for a single string (not container), there's no jt-tok display.
    // Wrap in a container to force jt-tok rendering.
    val big = "a" * 3500000
    val out = render(js.Dynamic.literal(s = big))
    Result.assert(out.contains("M tok")).log(s"M form missing: ${out.take(500)}…")
  }

  // ── Click-attr presence regression ─────────────────────────────────────

  def testNoContainerInlineOnclick: Result = {
    val out = render(parse("[1,2,3]"))
    Result.assert(!out.contains("onclick="))
      .log(s"unexpected inline onclick in container output: $out")
  }

  def testNoStringInlineOnclick: Result = {
    val long = "a" * 301
    val out  = render(parse(js.JSON.stringify(long)))
    Result.assert(!out.contains("onclick="))
      .log(s"unexpected inline onclick in long-string output: $out")
  }
}
