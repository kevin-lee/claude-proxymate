package claudeproxymate.renderer.copy

import claudeproxymate.renderer.json.JsonTreeView
import claudeproxymate.renderer.state.AppState
import hedgehog.*
import hedgehog.runner.*

import scala.scalajs.js

object MaskedCopySpec extends Properties {

  override def tests: List[Test] = List(
    example("primitive returns unchanged", testPrimitive),
    example("null returns unchanged", testNull),
    example("flat object: sensitive key replaced with sentinel", testFlatSensitive),
    example("flat object: non-sensitive key preserved verbatim", testFlatNonSensitive),
    example("nested object: nested sensitive key replaced", testNestedSensitive),
    example("array of objects: sensitive keys replaced inside elements", testArrayOfObjects),
    example("sensitive object value replaced as a whole (not recursed into)", testSensitiveContainerValue),
    example("does not mutate the input object", testNoInputMutation),
    example("input_tokens is NOT replaced (false-positive guard)", testInputTokensNotReplaced),
    example("Authorization (mixed case) is replaced", testCaseInsensitive),
    // Token-shape regex pass (PR2)
    example("regex token in non-sensitive string field is redacted with sentinel", testRegexTokenRedacted),
    example("multiple regex tokens in one string are all redacted", testMultipleRegexTokens),
    example("non-token string preserved verbatim", testNonTokenStringPreserved),
    example(
      "regex token inside sensitive-key value still field-replaced (no double mask)",
      testRegexInsideSensitiveKey
    ),
    // WYSIWYG: correlation ids follow the on-screen mask state
    example("masked correlation id (msg_…) copies as sentinel", testCorrIdMasked),
    example("revealed correlation id copies verbatim", testCorrIdRevealed),
    // WYSIWYG per-span reveals
    example("revealed token copies verbatim with counts", testRevealedToken),
    example("mixed reveal: one of two tokens revealed", testMixedReveal),
    example("revealed sensitive key recurses; interior token keeps its own span", testRevealedSensitiveKeyRecurses),
    example("root string body honors $#offset ids", testRootStringBody),
    example("masked sensitive key counts one span; interior never queried", testMaskedSensitiveInteriorNotQueried),
    example("copy walker queries exactly the ids the JSON tree renders", testIdAgreementWithJsonTree),
  )

  private def parse(s: String): js.Dynamic     = js.JSON.parse(s)
  private def stringify(d: js.Dynamic): String = js.JSON.stringify(d)

  /* All-masked predicate: the screen-share-safe default baseline. */
  private val maskAll: String => Boolean = _ => false

  private val FakeAnthropic = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
  private val FakeMsgId     = "msg_01ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def testPrimitive: Result = {
    val v   = "plain".asInstanceOf[js.Dynamic]
    val out = MaskedCopy.maskBody(v, maskAll)
    Result.all(
      List(
        Result
          .assert(out.body.asInstanceOf[String] == "plain")
          .log("primitive should be returned unchanged"),
        Result.assert(out.total == 0).log(s"unexpected spans: total=${out.total}"),
      )
    )
  }

  def testNull: Result = {
    val v = null.asInstanceOf[js.Dynamic]
    Result
      .assert(MaskedCopy.maskBody(v, maskAll).body == null)
      .log("null should be returned as null")
  }

  def testFlatSensitive: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""{"api_key":"sk-secret"}"""), maskAll).body)
    Result.all(
      List(
        Result.assert(!out.contains("sk-secret")).log(s"raw leaked: $out"),
        Result.assert(out.contains("\"" + MaskedCopy.Sentinel + "\"")).log(s"sentinel missing: $out"),
      )
    )
  }

  def testFlatNonSensitive: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""{"model":"claude","input_tokens":42}"""), maskAll).body)
    Result.all(
      List(
        Result.assert(out.contains("\"claude\"")).log(s"non-sensitive value lost: $out"),
        Result.assert(out.contains("42")).log(s"non-sensitive number lost: $out"),
        Result.assert(!out.contains(MaskedCopy.Sentinel)).log(s"unexpected sentinel: $out"),
      )
    )
  }

  def testNestedSensitive: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""{"outer":{"api_key":"sk-x","model":"claude"}}"""), maskAll).body)
    Result.all(
      List(
        Result.assert(!out.contains("sk-x")).log(s"raw leaked: $out"),
        Result.assert(out.contains("\"claude\"")).log(s"non-sensitive sibling lost: $out"),
      )
    )
  }

  def testArrayOfObjects: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""[{"api_key":"a"},{"api_key":"b"}]"""), maskAll).body)
    Result
      .assert(!out.contains("\"a\"") && !out.contains("\"b\""))
      .log(s"raw leaked from array element: $out")
  }

  def testSensitiveContainerValue: Result = {
    // The value of `authorization` is itself an object. The whole subtree
    // should be replaced by the sentinel (no recursion, so inner non-
    // sensitive keys also disappear).
    val out = stringify(
      MaskedCopy.maskBody(parse("""{"authorization":{"scheme":"bearer","value":"x.y.z"}}"""), maskAll).body
    )
    Result.all(
      List(
        Result.assert(!out.contains("bearer")).log(s"inner non-sensitive key leaked: $out"),
        Result.assert(!out.contains("x.y.z")).log(s"inner sensitive value leaked: $out"),
        Result.assert(out.contains("\"" + MaskedCopy.Sentinel + "\"")).log(s"sentinel missing: $out"),
      )
    )
  }

  def testNoInputMutation: Result = {
    val input  = parse("""{"api_key":"sk-immutable"}""")
    val before = stringify(input)
    val _      = MaskedCopy.maskBody(input, maskAll)
    val after  = stringify(input)
    Result
      .assert(before == after)
      .log(s"input was mutated: before=$before after=$after")
  }

  def testInputTokensNotReplaced: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""{"input_tokens":1234,"output_tokens":56}"""), maskAll).body)
    Result.all(
      List(
        Result.assert(out.contains("1234")).log(s"input_tokens value lost: $out"),
        Result.assert(out.contains("56")).log(s"output_tokens value lost: $out"),
        Result.assert(!out.contains(MaskedCopy.Sentinel)).log(s"unexpected sentinel: $out"),
      )
    )
  }

  def testCaseInsensitive: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""{"Authorization":"Bearer xyz"}"""), maskAll).body)
    Result
      .assert(!out.contains("Bearer xyz"))
      .log(s"mixed-case sensitive key not masked: $out")
  }

  // ── Token-shape regex pass (PR2) ───────────────────────────────────────

  def testRegexTokenRedacted: Result = {
    val out = stringify(MaskedCopy.maskBody(parse(s"""{"text":"prefix $FakeAnthropic suffix"}"""), maskAll).body)
    Result.all(
      List(
        Result.assert(!out.contains(FakeAnthropic)).log(s"raw token leaked: $out"),
        Result.assert(out.contains(MaskedCopy.Sentinel)).log(s"sentinel missing: $out"),
        Result.assert(out.contains("prefix ")).log(s"surrounding text lost: $out"),
        Result.assert(out.contains(" suffix")).log(s"surrounding text lost: $out"),
      )
    )
  }

  def testMultipleRegexTokens: Result = {
    val k1  = "sk-ant-abcdefghijklmnopqrstuvwxyz11111"
    val k2  = "AKIA0123456789ABCDEF"
    val out = stringify(MaskedCopy.maskBody(parse(s"""{"text":"$k1 and $k2"}"""), maskAll).body)
    Result.all(
      List(
        Result.assert(!out.contains(k1)).log(s"first token leaked: $out"),
        Result.assert(!out.contains(k2)).log(s"second token leaked: $out"),
      )
    )
  }

  def testNonTokenStringPreserved: Result = {
    val out = stringify(
      MaskedCopy.maskBody(parse("""{"text":"plain claude-3-5-sonnet model","model":"claude"}"""), maskAll).body
    )
    Result.all(
      List(
        Result.assert(out.contains("plain claude-3-5-sonnet model")).log(s"non-token text lost: $out"),
        Result.assert(out.contains("\"claude\"")).log(s"non-token model lost: $out"),
        Result.assert(!out.contains(MaskedCopy.Sentinel)).log(s"unexpected sentinel: $out"),
      )
    )
  }

  def testRegexInsideSensitiveKey: Result = {
    val res = MaskedCopy.maskBody(parse(s"""{"api_key":"$FakeAnthropic"}"""), maskAll)
    val out = stringify(res.body)
    // `api_key` triggers field-level replacement; no double-redaction
    // — the value is fully replaced before the string-token pass
    // ever sees it, so the field counts as exactly one span.
    Result.all(
      List(
        Result.assert(!out.contains(FakeAnthropic)).log(s"raw token leaked: $out"),
        Result.assert(out.contains(MaskedCopy.Sentinel)).log(s"sentinel missing: $out"),
        Result.assert(res.total == 1).log(s"masked sensitive key should be 1 span, got ${res.total}"),
      )
    )
  }

  // ── WYSIWYG: correlation ids follow the on-screen mask state ──────────

  def testCorrIdMasked: Result = {
    val res = MaskedCopy.maskBody(parse(s"""{"id":"$FakeMsgId","model":"claude"}"""), maskAll)
    val out = stringify(res.body)
    Result.all(
      List(
        Result.assert(!out.contains(FakeMsgId)).log(s"masked correlation id leaked: $out"),
        Result.assert(out.contains(MaskedCopy.Sentinel)).log(s"sentinel missing: $out"),
        Result
          .assert(res.total == 1 && res.revealed == 0)
          .log(s"counts wrong: revealed=${res.revealed} total=${res.total}"),
      )
    )
  }

  def testCorrIdRevealed: Result = {
    val res = MaskedCopy.maskBody(parse(s"""{"id":"$FakeMsgId"}"""), Set("corr:$.id#0"))
    val out = stringify(res.body)
    Result.all(
      List(
        Result.assert(out.contains(FakeMsgId)).log(s"revealed correlation id missing: $out"),
        Result
          .assert(res.total == 1 && res.revealed == 1)
          .log(s"counts wrong: revealed=${res.revealed} total=${res.total}"),
      )
    )
  }

  // ── WYSIWYG per-span reveals ───────────────────────────────────────────

  def testRevealedToken: Result = {
    val res = MaskedCopy.maskBody(parse(s"""{"text":"prefix $FakeAnthropic suffix"}"""), Set("$.text#7"))
    val out = stringify(res.body)
    Result.all(
      List(
        Result.assert(out.contains(FakeAnthropic)).log(s"revealed token missing: $out"),
        Result
          .assert(res.total == 1 && res.revealed == 1)
          .log(s"counts wrong: revealed=${res.revealed} total=${res.total}"),
      )
    )
  }

  def testMixedReveal: Result = {
    val k1  = "sk-ant-abcdefghijklmnopqrstuvwxyz11111"
    val k2  = "AKIA0123456789ABCDEF"
    val res = MaskedCopy.maskBody(parse(s"""{"text":"$k1 and $k2"}"""), Set("$.text#0"))
    val out = stringify(res.body)
    Result.all(
      List(
        Result.assert(out.contains(k1)).log(s"revealed first token missing: $out"),
        Result.assert(!out.contains(k2)).log(s"masked second token leaked: $out"),
        Result.assert(out.contains(MaskedCopy.Sentinel)).log(s"sentinel missing: $out"),
        Result
          .assert(res.total == 2 && res.revealed == 1)
          .log(s"counts wrong: revealed=${res.revealed} total=${res.total}"),
      )
    )
  }

  def testRevealedSensitiveKeyRecurses: Result = {
    val json = s"""{"api_key":"$FakeAnthropic"}"""

    /* Field revealed, interior token still masked: mirrors the screen,
     * where a revealed sensitive value re-renders its own token spans. */
    val partial    = MaskedCopy.maskBody(parse(json), Set("$.api_key"))
    val partialOut = stringify(partial.body)

    val full    = MaskedCopy.maskBody(parse(json), Set("$.api_key", "$.api_key#0"))
    val fullOut = stringify(full.body)

    Result.all(
      List(
        Result.assert(!partialOut.contains(FakeAnthropic)).log(s"interior token leaked: $partialOut"),
        Result.assert(partialOut.contains(MaskedCopy.Sentinel)).log(s"interior sentinel missing: $partialOut"),
        Result
          .assert(partial.total == 2 && partial.revealed == 1)
          .log(s"partial counts wrong: revealed=${partial.revealed} total=${partial.total}"),
        Result.assert(fullOut.contains(FakeAnthropic)).log(s"fully revealed token missing: $fullOut"),
        Result
          .assert(full.total == 2 && full.revealed == 2)
          .log(s"full counts wrong: revealed=${full.revealed} total=${full.total}"),
      )
    )
  }

  def testRootStringBody: Result = {
    val v        = FakeAnthropic.asInstanceOf[js.Dynamic]
    val masked   = MaskedCopy.maskBody(v, maskAll)
    val revealed = MaskedCopy.maskBody(v, Set("$#0"))
    Result.all(
      List(
        Result
          .assert(masked.body.asInstanceOf[String] == MaskedCopy.Sentinel)
          .log(s"masked root string should be sentinel: ${stringify(masked.body)}"),
        Result
          .assert(masked.total == 1 && masked.revealed == 0)
          .log(s"masked counts wrong: revealed=${masked.revealed} total=${masked.total}"),
        Result
          .assert(revealed.body.asInstanceOf[String] == FakeAnthropic)
          .log(s"revealed root string should be verbatim: ${stringify(revealed.body)}"),
        Result
          .assert(revealed.total == 1 && revealed.revealed == 1)
          .log(s"revealed counts wrong: revealed=${revealed.revealed} total=${revealed.total}"),
      )
    )
  }

  def testMaskedSensitiveInteriorNotQueried: Result = {
    val queried                     = scala.collection.mutable.Set.empty[String]
    val recorder: String => Boolean = { id => queried += id; false }
    val _                           = MaskedCopy.maskBody(parse(s"""{"api_key":"$FakeAnthropic"}"""), recorder)
    Result.all(
      List(
        Result.assert(queried.contains("$.api_key")).log(s"field id not queried: $queried"),
        Result
          .assert(!queried.contains("$.api_key#0"))
          .log(s"interior token id queried despite masked field: $queried"),
      )
    )
  }

  def testIdAgreementWithJsonTree: Result = {
    /* Fixture covers: nested object, array, sensitive key, in-string
     * token, correlation id, and a >300-char (long) string with an
     * embedded token. No newlines, so the long string renders as a
     * single expanded slice with absolute offsets. */
    val longStr = ("x" * 150) + FakeAnthropic + ("y" * 200)
    val json    =
      s"""{"a":{"api_key":"$FakeAnthropic","note":"has $FakeAnthropic inside"},""" +
        s""""arr":[{"id":"$FakeMsgId"},"plain"],"long":"$longStr","n":42}"""
    val value   = parse(json)

    AppState.jtId = 0
    AppState.jtLine = 0
    AppState.maskOverrides.clear()
    AppState.presenterMaskAll = true
    val html        = JsonTreeView.buildJsonFrag(value, stringify(value).length).render
    val attrRe      = ("data-(?:mask|token|corr)-id=\"([^\"]+)\"").r
    val renderedIds = attrRe.findAllMatchIn(html).map(_.group(1)).toSet

    val queried = scala.collection.mutable.Set.empty[String]
    val _       = MaskedCopy.maskBody(value, { id => queried += id; false })

    Result
      .assert(queried.toSet == renderedIds)
      .log(s"render ids: $renderedIds\ncopy ids: ${queried.toSet}")
  }
}
