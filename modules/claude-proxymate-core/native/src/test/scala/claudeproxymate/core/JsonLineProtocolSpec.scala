package claudeproxymate.core

import claudeproxymate.core.filter.{FilterReport, RemovedCategory, RemovedItem}
import io.circe.Json

/* munit port of the shared hedgehog JsonLineProtocolSpec for Scala
 * Native (the hedgehog runner hangs on Native). All cases are
 * examples; they port 1:1. */
class JsonLineProtocolSpec extends munit.FunSuite {

  private def roundTrip(event: ProxyEvent): Unit = {
    val line    = JsonLineProtocol.encode(event)
    val decoded = JsonLineProtocol.decode(line)
    assertEquals(decoded, Right(event), s"round-trip mismatch: encoded=$line decoded=$decoded")
  }

  /* ── encode => decode round-trips, one per ProxyEvent case ── */

  test("RequestCaptured round-trips through encode/decode") {
    roundTrip(
      ProxyEvent.RequestCaptured(
        ProxyRequest(
          id = CaptureId(1234L),
          capturedAt = EpochMillis(1794741364123L),
          method = "POST",
          path = "/v1/messages",
          body = Some(Json.obj("model" -> Json.fromString("claude"))),
          filter = None,
        )
      )
    )
  }

  test("RequestCaptured with a filter report round-trips") {
    roundTrip(
      ProxyEvent.RequestCaptured(
        ProxyRequest(
          id = CaptureId(1234L),
          capturedAt = EpochMillis(1794741364123L),
          method = "POST",
          path = "/v1/messages",
          body = Some(Json.obj("model" -> Json.fromString("claude"))),
          filter = Some(
            FilterReport(
              originalBytes = 120,
              filteredBytes = 80,
              removed = List(
                RemovedItem(RemovedCategory.Rule, "/u/.claude/rules/a.md", "📜 Global Rule: a.md", 0, 30),
                RemovedItem(RemovedCategory.Text, "secret", "Text secret", 2, 10),
              ),
              skipped = List("regex (?=x: Unknown inline modifier"),
            )
          ),
        )
      )
    )
  }

  test("RequestCaptured line without a filter field decodes to filter = None") {
    val line    =
      """{"type":"request_captured","request":{"id":1,"capturedAt":0,"method":"POST","path":"/v1/messages","body":null}}"""
    val decoded = JsonLineProtocol.decode(line)
    assertEquals(
      decoded,
      Right(
        ProxyEvent.RequestCaptured(ProxyRequest(CaptureId(1L), EpochMillis(0L), "POST", "/v1/messages", None, None))
      ),
      s"a line predating the filter field should decode with filter = None, got $decoded",
    )
  }

  test("RequestCaptured encodes capturedAt as a JSON number") {
    val line = JsonLineProtocol.encode(
      ProxyEvent.RequestCaptured(
        ProxyRequest(
          id = CaptureId(1L),
          capturedAt = EpochMillis(1794741364123L),
          method = "POST",
          path = "/v1/messages",
          body = None,
          filter = None,
        )
      )
    )
    assert(line.contains("\"capturedAt\":1794741364123"), s"capturedAt is not a number: $line")
    assert(!line.contains("\"ts\""), s"the old ts field is still there: $line")
  }

  test("ResponseCaptured (Right body) round-trips") {
    roundTrip(
      ProxyEvent.ResponseCaptured(
        ProxyResponse(
          id = CaptureId(1234L),
          status = 200,
          body = Right(Json.obj("id" -> Json.fromString("msg_01ABCDEFGHIJKLMNOP"))),
          error = None,
        )
      )
    )
  }

  test("ResponseCaptured (Left raw body) round-trips") {
    /* Left(rawString) is encoded as a JSON string and decoded back to
     * Left by the `bodyJson.isString` branch in the codec — verify
     * that asymmetry-preserving path survives the round-trip. */
    roundTrip(
      ProxyEvent.ResponseCaptured(
        ProxyResponse(
          id = CaptureId(7L),
          status = 502,
          body = Left("upstream exploded: not json"),
          error = Some("upstream exploded: not json"),
        )
      )
    )
  }

  test("ProxyStarted round-trips") {
    roundTrip(ProxyEvent.ProxyStarted(8888))
  }

  test("ProxyStopped round-trips") {
    roundTrip(ProxyEvent.ProxyStopped)
  }

  test("ProxyError round-trips") {
    roundTrip(ProxyEvent.ProxyError("boom"))
  }

  /* ── decode failure surfaces ── */

  test("malformed JSON line decodes to Left") {
    assert(JsonLineProtocol.decode("not json at all").isLeft, "malformed line should decode to Left")
  }

  test("valid JSON with unknown type decodes to Left") {
    val decoded = JsonLineProtocol.decode("""{"type":"totally_unknown"}""")
    assert(decoded.isLeft, s"unknown event type should decode to Left, got $decoded")
  }

  test("valid JSON missing the type field decodes to Left") {
    assert(JsonLineProtocol.decode("""{"port":8888}""").isLeft, "line without a type field should decode to Left")
  }

  /* ── the encoded form is single-line (JSON Lines invariant) ── */

  test("encoded event contains no newline") {
    /* A raw body containing a newline must not leak a literal newline
     * into the encoded line (it would split into two protocol lines on
     * the consumer side). circe escapes it as \n inside the JSON
     * string, so the encoded line stays single-line. */
    val line = JsonLineProtocol.encode(
      ProxyEvent.ResponseCaptured(ProxyResponse(CaptureId(1L), 200, Left("line1\nline2"), None))
    )
    assert(!line.contains("\n"), s"encoded line leaked a newline: $line")
  }
}
