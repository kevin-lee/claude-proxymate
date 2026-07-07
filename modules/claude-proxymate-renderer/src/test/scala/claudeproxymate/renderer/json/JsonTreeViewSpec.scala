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
    example("short string with inner double-quotes JSON-escapes them", testShortStrInnerDoubleQuotes),
    example("long string builds jt-str-long envelope", testLongStrEnvelope),
    example("long string preview is 80 chars + ellipsis", testLongStrPreviewLength),
    example("long string expanded splits on real newlines", testLongStrExpandedLines),
    example(
      "long string with literal backslash-n does NOT split (legacy dropped)",
      testLongStrLiteralBackslashNDoesNotSplit
    ),
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
    // Field-name masking (C3)
    example("api_key value renders as masked placeholder, not raw", testMaskApiKeyHidesValue),
    example("api_key value reveals when maskOverrides contains the id (presenterMaskAll=true)", testMaskApiKeyRevealed),
    example("nested object under sensitive key is masked as a whole", testMaskNestedObject),
    example("input_tokens does NOT render the masked placeholder", testMaskNoFalsePositiveOnTokenCount),
    example("mask data-mask-id is the dot-path of the field (stable across re-renders)", testMaskIdIsDotPath),
    example("masked placeholder uses provided maskLabel", testMaskUsesProvidedLabel),
    // Token-shape masking (C3 PR2)
    example("string containing sk-ant-… renders as a token-mask span", testTokenMaskAnthropic),
    example("masked token renders the fingerprint, not the raw value", testTokenMaskFingerprint),
    example("token-mask reveals when maskOverrides contains the path#offset id", testTokenMaskRevealed),
    example("plain string with no tokens renders without token-mask", testTokenMaskNoFalsePositive),
    example("sensitive key wins over inner token (no inner mask rendered)", testTokenMaskSensitiveKeyWins),
    // Correlation-id masking (C3 PR3)
    example("msg_… value renders as a corr-mask span", testCorrMaskMsg),
    example("corr-mask renders prefix + … + last-4 (msg_…XYZ4)", testCorrMaskFingerprint),
    example("corr-mask reveals when maskOverrides contains corr:path#offset", testCorrMaskReveal),
    example("plain integer id does NOT render a corr-mask", testCorrMaskNoFalsePositive),
    example("sensitive key wins over inner correlation id (no corr-mask)", testCorrMaskSensitiveKeyWins),
    // Long-string token masking (C3 PR2c)
    example("long string with token renders token-mask in expanded view", testLongStrTokenInExpanded),
    example("long string with token does not leak the raw token", testLongStrTokenNoLeak),
    example("long string token at start renders mask in collapsed preview", testLongStrTokenInPreview),
    example("long string token across 80-char preview boundary skipped from preview", testLongStrTokenCrossingPreview),
    example("long string with token uses raw-string offset in token id", testLongStrTokenIdOffset),
    // Path walker (used by JsonTreeViewer.toggleMaskReveal for in-place swap)
    example("resolvePath: $ returns the root value", testResolvePathRoot),
    example("resolvePath: $.key returns nested object value", testResolvePathDotKey),
    example("resolvePath: $.parent.child returns deeply nested value", testResolvePathDeepObject),
    example("resolvePath: $[0] returns array element", testResolvePathArrayIndex),
    example("resolvePath: $.arr[2].leaf returns mixed nesting", testResolvePathMixed),
    example("resolvePath: missing key returns null", testResolvePathMissingKey),
    example("resolvePath: out-of-range array index returns null", testResolvePathOutOfRange),
  )

  private def reset(): Unit = {
    AppState.jtId = 0
    AppState.jtLine = 0
    AppState.maskOverrides.clear()
    // Existing tests were written for the "default = all masked"
    // baseline. Pin it explicitly so a test that flips the flag
    // (e.g. PresenterModeSpec running in the same suite) can't
    // bleed state into a later test.
    AppState.presenterMaskAll = true
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

  def testShortStrInnerDoubleQuotes: Result = {
    // The metadata.user_id case: a string value that itself contains
    // JSON. On screen we want the inner `"`s to display as `\"` to
    // match the clipboard / standard JSON viewer convention.
    val payload = """{"device_id":"abc","account_uuid":"def"}"""
    val out     = render(parse(js.JSON.stringify(payload)))
    Result.all(
      List(
        // `\"` in source is rendered to `\&quot;` in the output
        // (Scalatags HTML-escapes the `"`, the backslash stays).
        Result
          .assert(out.contains("\\&quot;device_id\\&quot;"))
          .log(s"inner double-quote not JSON-escaped: $out"),
        Result
          .assert(!out.contains("\"device_id\""))
          .log(s"raw inner literal `\"device_id\"` leaked: $out"),
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
    val long            = ("x" * 80) + ("y" * 250)
    val out             = render(parse(js.JSON.stringify(long)))
    val previewExpected = ("x" * 80) + "…"
    Result.all(
      List(
        Result.assert(out.contains(previewExpected)).log(s"preview not '$previewExpected' in: $out"),
        Result.assert(out.contains(s"(${long.length} chars)")).log(s"chars count missing: $out"),
      )
    )
  }

  def testLongStrExpandedLines: Result = {
    // Build a long string with REAL newlines; expanded view splits
    // on real `\n` chars per JSON viewer convention.
    val pad      = "z" * 305
    val value    = "line1\nline2\n" + pad
    val out      = render(parse(js.JSON.stringify(value)))
    val expCount = out.split("class=\"jt-exp-line\"", -1).length - 1
    Result.all(
      List(
        Result.assert(expCount >= 3).log(s"expected >= 3 jt-exp-line, got $expCount in: $out"),
        Result.assert(out.contains("line1")).log("line1 missing"),
        Result.assert(out.contains("line2")).log("line2 missing"),
      )
    )
  }

  def testLongStrLiteralBackslashNDoesNotSplit: Result = {
    // Regression guard for the dropped `\\n` legacy split: a long
    // string containing the literal two-char sequence `\` + `n`
    // (NOT a real newline) renders as a SINGLE expanded line.
    // Embedded `\n` escape sequences appear in the rendered text
    // per JSON.stringify convention.
    val pad      = "z" * 305
    val value    = "line1\\nline2\\n" + pad
    val out      = render(parse(js.JSON.stringify(value)))
    val expCount = out.split("class=\"jt-exp-line\"", -1).length - 1
    Result
      .assert(expCount == 1)
      .log(s"expected exactly 1 jt-exp-line for literal-backslash-n value, got $expCount: $out")
  }

  def testLongStrDataAttr: Result = {
    val long        = "a" * 301
    val out         = render(parse(js.JSON.stringify(long)))
    val occurrences = out.split("data-jt-str-id=\"jts1\"", -1).length - 1
    // Three carriers: -btn span, -s preview, expanded toggle
    Result
      .assert(occurrences == 3)
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
    val out      = render(parse("[1]"))
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
    val out        = render(parse("[1,2,3]"))
    val commaCount = out.split(">,<", -1).length - 1
    // Two non-last entries (positions 0,1) emit commas; closing bracket has no comma at root.
    Result
      .assert(commaCount == 2)
      .log(s"expected 2 commas, got $commaCount: $out")
  }

  def testMultiObjComma: Result = {
    val out        = render(parse("""{"a":1,"b":2,"c":3}"""))
    val commaCount = out.split(">,<", -1).length - 1
    Result
      .assert(commaCount == 2)
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
      val out  = render(json)
      Result
        .assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakInValue: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val json    = js.Dynamic.literal(k = payload)
      val out     = render(json)
      Result
        .assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakDeep: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val deep    = js.Dynamic.literal(a = js.Dynamic.literal(b = js.Array(payload)))
      val out     = render(deep)
      Result
        .assert(!out.contains("<script>"))
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
    Result
      .assert(!out.contains("onclick="))
      .log(s"unexpected inline onclick in container output: $out")
  }

  def testNoStringInlineOnclick: Result = {
    val long = "a" * 301
    val out  = render(parse(js.JSON.stringify(long)))
    Result
      .assert(!out.contains("onclick="))
      .log(s"unexpected inline onclick in long-string output: $out")
  }

  // ── Field-name masking (C3) ────────────────────────────────────────────

  private def renderWithLabel(value: js.Dynamic, maskLabel: String): String = {
    reset()
    val totalBytes = js.JSON.stringify(value).length
    JsonTreeView.buildJsonFrag(value, totalBytes, maskLabel).render
  }

  def testMaskApiKeyHidesValue: Result = {
    val out = renderWithLabel(parse("""{"api_key":"sk-secret-value-123"}"""), "hidden")
    Result.all(
      List(
        Result.assert(out.contains(s"""class="${JsonTreeView.MaskClass}"""")).log(s"jt-mask span missing: $out"),
        Result.assert(!out.contains("sk-secret-value-123")).log(s"raw value leaked: $out"),
      )
    )
  }

  def testMaskApiKeyRevealed: Result = {
    val raw        = "sk-secret-value-123"
    val value      = parse(s"""{"api_key":"$raw"}""")
    reset()
    val totalBytes = js.JSON.stringify(value).length
    // Mask id is the dot-path; pre-add it to the reveal set, then render.
    val _          = AppState.maskOverrides.add("$.api_key")
    val out        = JsonTreeView.buildJsonFrag(value, totalBytes, "hidden").render
    Result.all(
      List(
        Result
          .assert(out.contains(s"""class="${JsonTreeView.MaskRevealedClass}""""))
          .log(s"jt-mask-revealed missing: $out"),
        Result.assert(out.contains(raw)).log(s"raw value missing when revealed: $out"),
      )
    )
  }

  def testMaskNestedObject: Result = {
    val out = renderWithLabel(parse("""{"authorization":{"type":"bearer","token":"x.y.z"}}"""), "hidden")
    Result.all(
      List(
        Result.assert(out.contains(s"""class="${JsonTreeView.MaskClass}"""")).log(s"jt-mask span missing: $out"),
        Result.assert(!out.contains("x.y.z")).log(s"inner token leaked: $out"),
        Result.assert(!out.contains("\"type\"")).log(s"inner key leaked: $out"),
      )
    )
  }

  def testMaskNoFalsePositiveOnTokenCount: Result = {
    val out = renderWithLabel(parse("""{"input_tokens":1234,"output_tokens":56}"""), "hidden")
    Result.all(
      List(
        Result
          .assert(!out.contains(s"""class="${JsonTreeView.MaskClass}""""))
          .log(s"jt-mask span unexpectedly present: $out"),
        Result.assert(out.contains("1234")).log(s"input_tokens value missing: $out"),
        Result.assert(out.contains("56")).log(s"output_tokens value missing: $out"),
      )
    )
  }

  def testMaskIdIsDotPath: Result = {
    val value = parse("""{"metadata":{"user_id":"u","api_key":"a"}}""")
    val out1  = renderWithLabel(value, "hidden")
    val out2  = renderWithLabel(value, "hidden")
    Result.all(
      List(
        Result
          .assert(out1.contains(s"""${JsonTreeView.MaskDataAttr}="$$.metadata.user_id""""))
          .log(s"expected stable dot-path id `$$.metadata.user_id`: $out1"),
        Result
          .assert(out1.contains(s"""${JsonTreeView.MaskDataAttr}="$$.metadata.api_key""""))
          .log(s"expected stable dot-path id `$$.metadata.api_key`: $out1"),
        Result
          .assert(out1 == out2)
          .log(s"renders should be identical across calls — masking is deterministic on the data"),
      )
    )
  }

  def testMaskUsesProvidedLabel: Result = {
    val out = renderWithLabel(parse("""{"api_key":"v"}"""), "REDACTED-LABEL")
    Result
      .assert(out.contains("REDACTED-LABEL"))
      .log(s"provided maskLabel not in output: $out")
  }

  // ── Correlation-id masking (C3 PR3) ───────────────────────────────────

  private val FakeMsgId = "msg_01ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def testCorrMaskMsg: Result = {
    val out = renderWithLabel(parse(s"""{"id":"$FakeMsgId"}"""), "hidden")
    Result.all(
      List(
        Result
          .assert(out.contains(s"""class="${JsonTreeView.CorrMaskClass}""""))
          .log(s"jt-corr-mask span missing: $out"),
        Result.assert(!out.contains(FakeMsgId)).log(s"raw id leaked: $out"),
      )
    )
  }

  def testCorrMaskFingerprint: Result = {
    val out = renderWithLabel(parse(s"""{"id":"$FakeMsgId"}"""), "hidden")
    // Fingerprint is `msg_…<last-4>` where last-4 of `msg_01ABCDEFGHIJKLMNOPQRSTUVWXYZ` is `WXYZ`.
    Result
      .assert(out.contains("msg_") && out.contains("…") && out.contains("WXYZ"))
      .log(s"fingerprint shape missing: $out")
  }

  def testCorrMaskReveal: Result = {
    val data       = parse(s"""{"id":"$FakeMsgId"}""")
    reset()
    val totalBytes = js.JSON.stringify(data).length
    val _          = AppState.maskOverrides.add(s"corr:$$.id#0")
    val out        = JsonTreeView.buildJsonFrag(data, totalBytes, "hidden").render
    Result.all(
      List(
        Result
          .assert(out.contains(s"""class="${JsonTreeView.CorrMaskRevealedClass}""""))
          .log(s"jt-corr-mask-revealed missing: $out"),
        Result.assert(out.contains(FakeMsgId)).log(s"raw id missing when revealed: $out"),
      )
    )
  }

  def testCorrMaskNoFalsePositive: Result = {
    val out = renderWithLabel(parse("""{"id":42,"count":100}"""), "hidden")
    Result
      .assert(!out.contains(s"""class="${JsonTreeView.CorrMaskClass}""""))
      .log(s"unexpected corr-mask on integer id: $out")
  }

  def testCorrMaskSensitiveKeyWins: Result = {
    // `id_token` is a sensitive key (PR 1). The whole value should
    // be field-masked; no inner corr-mask should be emitted.
    val out = renderWithLabel(parse(s"""{"id_token":"$FakeMsgId"}"""), "hidden")
    Result.all(
      List(
        Result.assert(out.contains(s"""class="${JsonTreeView.MaskClass}"""")).log(s"field-name mask missing: $out"),
        Result
          .assert(!out.contains(s"""class="${JsonTreeView.CorrMaskClass}""""))
          .log(s"unexpected inner corr-mask: $out"),
        Result.assert(!out.contains(FakeMsgId)).log(s"raw id leaked: $out"),
      )
    )
  }

  // ── Long-string token masking (C3 PR2c) ───────────────────────────────

  private val LongPad = "z" * 305 // pushes value past CollapseLenThreshold

  def testLongStrTokenInExpanded: Result = {
    val key      = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    val value    = LongPad + " " + key + " trailing"
    val out      = render(parse(js.JSON.stringify(value)))
    val expanded = out.substring(out.indexOf("class=\"jt-str-expanded\""))
    Result.all(
      List(
        Result
          .assert(expanded.contains(s"""class="${JsonTreeView.TokenMaskClass}""""))
          .log(s"jt-token-mask span missing from expanded view: $expanded"),
        Result.assert(!out.contains(key)).log(s"raw token leaked: $out"),
      )
    )
  }

  def testLongStrTokenNoLeak: Result = {
    val key   = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    val value = LongPad + " " + key
    val out   = render(parse(js.JSON.stringify(value)))
    Result
      .assert(!out.contains(key))
      .log(s"raw token leaked from long-string render: $out")
  }

  def testLongStrTokenInPreview: Result = {
    // Token at offset 0 — fully inside the first 80 chars of preview.
    val key     = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    val value   = key + " " + LongPad
    val out     = render(parse(js.JSON.stringify(value)))
    // The preview span exists; it should contain the mask.
    val preview = out.substring(out.indexOf("class=\"jt-str-preview\""), out.indexOf("class=\"jt-str-expanded\""))
    Result
      .assert(preview.contains(s"""class="${JsonTreeView.TokenMaskClass}""""))
      .log(s"jt-token-mask span missing from preview: $preview")
  }

  def testLongStrTokenCrossingPreview: Result = {
    // Token starts at offset 60, length 38 → ends at 98, crossing
    // the 80-char preview boundary. Preview should NOT render a
    // mask span for it; expanded view DOES.
    val key      = "sk-ant-abcdefghijklmnopqrstuvwxyz12345" // length 38
    val value    = ("a" * 60) + key + LongPad
    val out      = render(parse(js.JSON.stringify(value)))
    val preview  = out.substring(out.indexOf("class=\"jt-str-preview\""), out.indexOf("class=\"jt-str-expanded\""))
    val expanded = out.substring(out.indexOf("class=\"jt-str-expanded\""))
    Result.all(
      List(
        Result
          .assert(!preview.contains(s"""class="${JsonTreeView.TokenMaskClass}""""))
          .log(s"unexpected token mask in preview: $preview"),
        Result
          .assert(expanded.contains(s"""class="${JsonTreeView.TokenMaskClass}""""))
          .log(s"token mask missing from expanded view: $expanded"),
      )
    )
  }

  def testLongStrTokenIdOffset: Result = {
    // Token at known offset 5 within the raw value.
    val key   = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    val value = "abcd " + key + " " + LongPad
    val out   = render(parse(js.JSON.stringify(value)))
    Result
      .assert(out.contains(s"""${JsonTreeView.TokenMaskDataAttr}="$$#5""""))
      .log(s"expected `${JsonTreeView.TokenMaskDataAttr}=\"$$#5\"` in: $out")
  }

  // ── Token-shape masking (C3 PR2) ───────────────────────────────────────

  def testTokenMaskAnthropic: Result = {
    val key = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    val out = renderWithLabel(parse(s"""{"text":"prefix $key suffix"}"""), "hidden")
    Result.all(
      List(
        Result
          .assert(out.contains(s"""class="${JsonTreeView.TokenMaskClass}""""))
          .log(s"jt-token-mask span missing: $out"),
        Result
          .assert(!out.contains(key))
          .log(s"raw token leaked: $out"),
        Result.assert(out.contains("prefix ")).log(s"surrounding text lost: $out"),
        Result.assert(out.contains(" suffix")).log(s"surrounding text lost: $out"),
      )
    )
  }

  def testTokenMaskFingerprint: Result = {
    val key = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    val out = renderWithLabel(parse(s"""{"text":"$key"}"""), "hidden")
    // first-4 = "sk-a", last-4 = "2345"
    Result
      .assert(out.contains("sk-a") && out.contains("2345") && out.contains("…"))
      .log(s"fingerprint shape missing: $out")
  }

  def testTokenMaskRevealed: Result = {
    val key        = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    val data       = parse(s"""{"text":"prefix $key suffix"}""")
    reset()
    val totalBytes = js.JSON.stringify(data).length
    // The token offset within the value `prefix sk-ant-... suffix` is 7
    // (length of "prefix "). Token id = `$.text#7`.
    val _          = AppState.maskOverrides.add("$.text#7")
    val out        = JsonTreeView.buildJsonFrag(data, totalBytes, "hidden").render
    Result.all(
      List(
        Result
          .assert(out.contains(s"""class="${JsonTreeView.TokenMaskRevealedClass}""""))
          .log(s"jt-token-mask-revealed missing: $out"),
        Result
          .assert(out.contains(key))
          .log(s"raw token missing when revealed: $out"),
      )
    )
  }

  def testTokenMaskNoFalsePositive: Result = {
    val out = renderWithLabel(parse("""{"text":"plain claude-3-5-sonnet model name"}"""), "hidden")
    Result
      .assert(!out.contains(s"""class="${JsonTreeView.TokenMaskClass}""""))
      .log(s"unexpected token-mask span: $out")
  }

  def testTokenMaskSensitiveKeyWins: Result = {
    // Sensitive key `api_key` masks the whole value; no inner
    // token-mask should be emitted (the value isn't visible at all).
    val key = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    val out = renderWithLabel(parse(s"""{"api_key":"$key"}"""), "hidden")
    Result.all(
      List(
        Result.assert(out.contains(s"""class="${JsonTreeView.MaskClass}"""")).log(s"field-name mask missing: $out"),
        Result
          .assert(!out.contains(s"""class="${JsonTreeView.TokenMaskClass}""""))
          .log(s"unexpected inner token-mask: $out"),
        Result.assert(!out.contains(key)).log(s"raw token leaked: $out"),
      )
    )
  }

  // ── Path walker ────────────────────────────────────────────────────────

  def testResolvePathRoot: Result = {
    val root = parse("""{"a":1}""")
    val v    = JsonTreeView.resolvePath(root, "$")
    Result.assert(v == root).log(s"`$$` should return root, got $v")
  }

  def testResolvePathDotKey: Result = {
    val root = parse("""{"a":42}""")
    val v    = JsonTreeView.resolvePath(root, "$.a")
    Result.assert(v.asInstanceOf[Int] == 42).log(s"`$$.a` should return 42, got $v")
  }

  def testResolvePathDeepObject: Result = {
    val root = parse("""{"metadata":{"user_id":"u-7"}}""")
    val v    = JsonTreeView.resolvePath(root, "$.metadata.user_id")
    Result.assert(v.asInstanceOf[String] == "u-7").log(s"deep nested missing: $v")
  }

  def testResolvePathArrayIndex: Result = {
    val root = parse("""[10,20,30]""")
    val v    = JsonTreeView.resolvePath(root, "$[1]")
    Result.assert(v.asInstanceOf[Int] == 20).log(s"`$$[1]` should return 20, got $v")
  }

  def testResolvePathMixed: Result = {
    val root = parse("""{"arr":[{"k":"x"},{"k":"y"},{"k":"z","leaf":"L"}]}""")
    val v    = JsonTreeView.resolvePath(root, "$.arr[2].leaf")
    Result.assert(v.asInstanceOf[String] == "L").log(s"mixed-nesting walk failed: $v")
  }

  def testResolvePathMissingKey: Result = {
    val root = parse("""{"a":1}""")
    val v    = JsonTreeView.resolvePath(root, "$.nope")
    Result
      .assert(v == null || js.isUndefined(v))
      .log(s"missing key should return null/undefined, got $v")
  }

  def testResolvePathOutOfRange: Result = {
    val root = parse("""[1,2]""")
    val v    = JsonTreeView.resolvePath(root, "$[10]")
    Result
      .assert(v == null || js.isUndefined(v))
      .log(s"out-of-range index should return null, got $v")
  }
}
