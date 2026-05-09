package claudeproxymate.renderer.copy

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
    example("regex token inside sensitive-key value still field-replaced (no double mask)", testRegexInsideSensitiveKey),
    // PR3: correlation IDs are NOT redacted in copy
    example("correlation id (msg_…) is preserved verbatim in copy", testCorrIdNotRedacted),
  )

  private def parse(s: String): js.Dynamic = js.JSON.parse(s)
  private def stringify(d: js.Dynamic): String = js.JSON.stringify(d)

  def testPrimitive: Result = {
    val v = "plain".asInstanceOf[js.Dynamic]
    Result.assert(MaskedCopy.maskBody(v).asInstanceOf[String] == "plain")
      .log("primitive should be returned unchanged")
  }

  def testNull: Result = {
    val v = null.asInstanceOf[js.Dynamic]
    Result.assert(MaskedCopy.maskBody(v) == null)
      .log("null should be returned as null")
  }

  def testFlatSensitive: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""{"api_key":"sk-secret"}""")))
    Result.all(
      List(
        Result.assert(!out.contains("sk-secret")).log(s"raw leaked: $out"),
        Result.assert(out.contains("\"" + MaskedCopy.Sentinel + "\"")).log(s"sentinel missing: $out"),
      )
    )
  }

  def testFlatNonSensitive: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""{"model":"claude","input_tokens":42}""")))
    Result.all(
      List(
        Result.assert(out.contains("\"claude\"")).log(s"non-sensitive value lost: $out"),
        Result.assert(out.contains("42")).log(s"non-sensitive number lost: $out"),
        Result.assert(!out.contains(MaskedCopy.Sentinel)).log(s"unexpected sentinel: $out"),
      )
    )
  }

  def testNestedSensitive: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""{"outer":{"api_key":"sk-x","model":"claude"}}""")))
    Result.all(
      List(
        Result.assert(!out.contains("sk-x")).log(s"raw leaked: $out"),
        Result.assert(out.contains("\"claude\"")).log(s"non-sensitive sibling lost: $out"),
      )
    )
  }

  def testArrayOfObjects: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""[{"api_key":"a"},{"api_key":"b"}]""")))
    Result.assert(!out.contains("\"a\"") && !out.contains("\"b\""))
      .log(s"raw leaked from array element: $out")
  }

  def testSensitiveContainerValue: Result = {
    // The value of `authorization` is itself an object. The whole subtree
    // should be replaced by the sentinel (no recursion, so inner non-
    // sensitive keys also disappear).
    val out = stringify(MaskedCopy.maskBody(parse("""{"authorization":{"scheme":"bearer","value":"x.y.z"}}""")))
    Result.all(
      List(
        Result.assert(!out.contains("bearer")).log(s"inner non-sensitive key leaked: $out"),
        Result.assert(!out.contains("x.y.z")).log(s"inner sensitive value leaked: $out"),
        Result.assert(out.contains("\"" + MaskedCopy.Sentinel + "\"")).log(s"sentinel missing: $out"),
      )
    )
  }

  def testNoInputMutation: Result = {
    val input = parse("""{"api_key":"sk-immutable"}""")
    val before = stringify(input)
    val _ = MaskedCopy.maskBody(input)
    val after = stringify(input)
    Result.assert(before == after)
      .log(s"input was mutated: before=$before after=$after")
  }

  def testInputTokensNotReplaced: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""{"input_tokens":1234,"output_tokens":56}""")))
    Result.all(
      List(
        Result.assert(out.contains("1234")).log(s"input_tokens value lost: $out"),
        Result.assert(out.contains("56")).log(s"output_tokens value lost: $out"),
        Result.assert(!out.contains(MaskedCopy.Sentinel)).log(s"unexpected sentinel: $out"),
      )
    )
  }

  def testCaseInsensitive: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""{"Authorization":"Bearer xyz"}""")))
    Result.assert(!out.contains("Bearer xyz"))
      .log(s"mixed-case sensitive key not masked: $out")
  }

  // ── Token-shape regex pass (PR2) ───────────────────────────────────────

  def testRegexTokenRedacted: Result = {
    val key = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    val out = stringify(MaskedCopy.maskBody(parse(s"""{"text":"prefix $key suffix"}""")))
    Result.all(
      List(
        Result.assert(!out.contains(key)).log(s"raw token leaked: $out"),
        Result.assert(out.contains(MaskedCopy.Sentinel)).log(s"sentinel missing: $out"),
        Result.assert(out.contains("prefix ")).log(s"surrounding text lost: $out"),
        Result.assert(out.contains(" suffix")).log(s"surrounding text lost: $out"),
      )
    )
  }

  def testMultipleRegexTokens: Result = {
    val k1  = "sk-ant-abcdefghijklmnopqrstuvwxyz11111"
    val k2  = "AKIA0123456789ABCDEF"
    val out = stringify(MaskedCopy.maskBody(parse(s"""{"text":"$k1 and $k2"}""")))
    Result.all(
      List(
        Result.assert(!out.contains(k1)).log(s"first token leaked: $out"),
        Result.assert(!out.contains(k2)).log(s"second token leaked: $out"),
      )
    )
  }

  def testNonTokenStringPreserved: Result = {
    val out = stringify(MaskedCopy.maskBody(parse("""{"text":"plain claude-3-5-sonnet model","model":"claude"}""")))
    Result.all(
      List(
        Result.assert(out.contains("plain claude-3-5-sonnet model")).log(s"non-token text lost: $out"),
        Result.assert(out.contains("\"claude\"")).log(s"non-token model lost: $out"),
        Result.assert(!out.contains(MaskedCopy.Sentinel)).log(s"unexpected sentinel: $out"),
      )
    )
  }

  def testRegexInsideSensitiveKey: Result = {
    val key = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    val out = stringify(MaskedCopy.maskBody(parse(s"""{"api_key":"$key"}""")))
    // `api_key` triggers field-level replacement; no double-redaction
    // — the value is fully replaced before the string-token pass
    // ever sees it.
    Result.all(
      List(
        Result.assert(!out.contains(key)).log(s"raw token leaked: $out"),
        Result.assert(out.contains(MaskedCopy.Sentinel)).log(s"sentinel missing: $out"),
      )
    )
  }

  // ── PR3: correlation IDs are NOT redacted ─────────────────────────────

  def testCorrIdNotRedacted: Result = {
    // `msg_…` is a correlation id (PR 3 visual mask only). Copy
    // emits it verbatim so cross-capture correlation stays useful
    // when the output is pasted into another tool.
    val msgId = "msg_01ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val out   = stringify(MaskedCopy.maskBody(parse(s"""{"id":"$msgId","model":"claude"}""")))
    Result.assert(out.contains(msgId))
      .log(s"correlation id was unexpectedly redacted: $out")
  }
}
