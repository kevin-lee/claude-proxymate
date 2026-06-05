package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*
import io.circe.Json

object JsonLineProtocolSpec extends Properties {

  override def tests: List[Test] = List(
    /* encode => decode round-trips, one per ProxyEvent case */
    example("RequestCaptured round-trips through encode/decode", testRoundTripRequest),
    example("ResponseCaptured (Right body) round-trips", testRoundTripResponseJson),
    example("ResponseCaptured (Left raw body) round-trips", testRoundTripResponseRaw),
    example("ProxyStarted round-trips", testRoundTripStarted),
    example("ProxyStopped round-trips", testRoundTripStopped),
    example("ProxyError round-trips", testRoundTripError),
    /* decode failure surfaces (the behavior J-c relies on) */
    example("malformed JSON line decodes to Left", testMalformedLine),
    example("valid JSON with unknown type decodes to Left", testUnknownType),
    example("valid JSON missing the type field decodes to Left", testMissingType),
    /* the encoded form is single-line (JSON Lines invariant) */
    example("encoded event contains no newline", testNoNewline),
  )

  private def roundTrip(event: ProxyEvent): Result = {
    val line    = JsonLineProtocol.encode(event)
    val decoded = JsonLineProtocol.decode(line)
    Result
      .assert(decoded == Right(event))
      .log(s"round-trip mismatch: encoded=$line decoded=$decoded")
  }

  // region ── Round-trips ──────────────────────────────────────────────────────────

  def testRoundTripRequest: Result =
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

  def testRoundTripResponseJson: Result =
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

  def testRoundTripResponseRaw: Result =
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

  def testRoundTripStarted: Result =
    roundTrip(ProxyEvent.ProxyStarted(8888))

  def testRoundTripStopped: Result =
    roundTrip(ProxyEvent.ProxyStopped)

  def testRoundTripError: Result =
    roundTrip(ProxyEvent.ProxyError("boom"))

  // endregion ── Round-trips ──────────────────────────────────────────────────────────

  // region ── Decode failures ───────────────────────────────────────────────────────

  def testMalformedLine: Result =
    Result
      .assert(JsonLineProtocol.decode("not json at all").isLeft)
      .log("malformed line should decode to Left")

  def testUnknownType: Result = {
    val decoded = JsonLineProtocol.decode("""{"type":"totally_unknown"}""")
    Result
      .assert(decoded.isLeft)
      .log(s"unknown event type should decode to Left, got $decoded")
  }

  def testMissingType: Result =
    Result
      .assert(JsonLineProtocol.decode("""{"port":8888}""").isLeft)
      .log("line without a type field should decode to Left")

  // endregion ── Decode failures ───────────────────────────────────────────────────────

  // region ── JSON Lines invariant ───────────────────────────────────────────────────

  def testNoNewline: Result = {
    /* A raw body containing a newline must not leak a literal newline
     * into the encoded line (it would split into two protocol lines on
     * the consumer side). circe escapes it as \n inside the JSON
     * string, so the encoded line stays single-line. */
    val line = JsonLineProtocol.encode(
      ProxyEvent.ResponseCaptured(ProxyResponse(1L, 200, Left("line1\nline2"), None))
    )
    Result
      .assert(!line.contains("\n"))
      .log(s"encoded line leaked a newline: $line")
  }

  // endregion ── JSON Lines invariant ───────────────────────────────────────────────────

}
