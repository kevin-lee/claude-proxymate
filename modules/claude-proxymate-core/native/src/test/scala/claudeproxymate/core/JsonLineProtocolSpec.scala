package claudeproxymate.core

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
          id = 1234L,
          ts = "12:34:56",
          method = "POST",
          path = "/v1/messages",
          body = Some(Json.obj("model" -> Json.fromString("claude"))),
        )
      )
    )
  }

  test("ResponseCaptured (Right body) round-trips") {
    roundTrip(
      ProxyEvent.ResponseCaptured(
        ProxyResponse(
          id = 1234L,
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
          id = 7L,
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
      ProxyEvent.ResponseCaptured(ProxyResponse(1L, 200, Left("line1\nline2"), None))
    )
    assert(!line.contains("\n"), s"encoded line leaked a newline: $line")
  }
}
