package claudeproxymate.core

/* munit port of the shared hedgehog SseParserSpec for Scala Native, where
 * the hedgehog runner hangs. The shared properties all use degenerate
 * generators (Gen.constant), so each becomes 2-3 deterministic examples:
 * the original stream plus variants stressing the parser's edge paths
 * (blank-line handling, trailing data, CRLF, multi-event streams).
 */
class SseParserSpec extends munit.FunSuite {

  /* ── empty string returns None ────────────────────────────────────────── */

  test("empty string returns None (example: empty string)") {
    assertEquals(SseParser.parseSseStream(""), None)
  }

  test("empty string returns None (example: blank lines only)") {
    assertEquals(SseParser.parseSseStream("\n\n"), None)
  }

  test("empty string returns None (example: event line with no data)") {
    assertEquals(SseParser.parseSseStream("event: message_start\n\n"), None)
  }

  /* ── message_start reconstructs message ───────────────────────────────── */

  test("message_start reconstructs message (example: single event with trailing blank line)") {
    val sse = List(
      "event: message_start",
      """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3","content":[],"usage":{"input_tokens":10}}}""",
      "",
    ).mkString("\n")

    val id = SseParser.parseSseStream(sse).flatMap(_.hcursor.get[String]("id").toOption)
    assertEquals(id, Some("msg_1"))
  }

  test("message_start reconstructs message (example: trailing data without final blank line)") {
    val sse = List(
      "event: message_start",
      """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3","content":[],"usage":{"input_tokens":10}}}""",
    ).mkString("\n")

    val id = SseParser.parseSseStream(sse).flatMap(_.hcursor.get[String]("id").toOption)
    assertEquals(id, Some("msg_1"))
  }

  test("message_start reconstructs message (example: CRLF line endings)") {
    val sse = List(
      "event: message_start\r",
      """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3","content":[],"usage":{"input_tokens":10}}}""" + "\r",
      "\r",
    ).mkString("\n")

    val id = SseParser.parseSseStream(sse).flatMap(_.hcursor.get[String]("id").toOption)
    assertEquals(id, Some("msg_1"))
  }

  /* ── content_block_delta accumulates text ─────────────────────────────── */

  test("content_block_delta accumulates text (example: two deltas onto empty block)") {
    val sse = List(
      "event: message_start",
      """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3","content":[]}}""",
      "",
      "event: content_block_start",
      """data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}""",
      "",
      "event: content_block_delta",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hello"}}""",
      "",
      "event: content_block_delta",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":" world"}}""",
      "",
    ).mkString("\n")

    val text = SseParser
      .parseSseStream(sse)
      .flatMap(_.hcursor.downField("content").downN(0).get[String]("text").toOption)
    assertEquals(text, Some("Hello world"))
  }

  test("content_block_delta accumulates text (example: deltas appended to non-empty initial text)") {
    val sse = List(
      "event: message_start",
      """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3","content":[]}}""",
      "",
      "event: content_block_start",
      """data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":"Hi"}}""",
      "",
      "event: content_block_delta",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":","}}""",
      "",
      "event: content_block_delta",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":" there"}}""",
      "",
    ).mkString("\n")

    val text = SseParser
      .parseSseStream(sse)
      .flatMap(_.hcursor.downField("content").downN(0).get[String]("text").toOption)
    assertEquals(text, Some("Hi, there"))
  }

  /* ── thinking_delta accumulates thinking ──────────────────────────────── */

  test("thinking_delta accumulates thinking (example: two thinking deltas)") {
    val sse = List(
      "event: message_start",
      """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3","content":[]}}""",
      "",
      "event: content_block_start",
      """data: {"type":"content_block_start","index":0,"content_block":{"type":"thinking","thinking":""}}""",
      "",
      "event: content_block_delta",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"Let me"}}""",
      "",
      "event: content_block_delta",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":" think"}}""",
      "",
    ).mkString("\n")

    val thinking = SseParser
      .parseSseStream(sse)
      .flatMap(_.hcursor.downField("content").downN(0).get[String]("thinking").toOption)
    assertEquals(thinking, Some("Let me think"))
  }

  test("thinking_delta accumulates thinking (example: single thinking delta)") {
    val sse = List(
      "event: message_start",
      """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3","content":[]}}""",
      "",
      "event: content_block_start",
      """data: {"type":"content_block_start","index":0,"content_block":{"type":"thinking","thinking":""}}""",
      "",
      "event: content_block_delta",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"Reasoning"}}""",
      "",
    ).mkString("\n")

    val thinking = SseParser
      .parseSseStream(sse)
      .flatMap(_.hcursor.downField("content").downN(0).get[String]("thinking").toOption)
    assertEquals(thinking, Some("Reasoning"))
  }

  /* ── message_delta merges usage stats ─────────────────────────────────── */

  test("message_delta merges usage stats (example: end_turn with output tokens)") {
    val sse = List(
      "event: message_start",
      """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3","content":[],"usage":{"input_tokens":10}}}""",
      "",
      "event: message_delta",
      """data: {"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":20}}""",
      "",
    ).mkString("\n")

    val result     = SseParser.parseSseStream(sse)
    val stopReason = result.flatMap(_.hcursor.get[String]("stop_reason").toOption)
    val outTokens  = result.flatMap(_.hcursor.downField("usage").get[Int]("output_tokens").toOption)
    assertEquals(stopReason, Some("end_turn"))
    assertEquals(outTokens, Some(20))
  }

  test("message_delta merges usage stats (example: two message_delta events, input tokens preserved)") {
    val sse = List(
      "event: message_start",
      """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3","content":[],"usage":{"input_tokens":10}}}""",
      "",
      "event: message_delta",
      """data: {"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":20}}""",
      "",
      "event: message_delta",
      """data: {"type":"message_delta","delta":{},"usage":{"output_tokens":25}}""",
      "",
    ).mkString("\n")

    val result    = SseParser.parseSseStream(sse)
    val inTokens  = result.flatMap(_.hcursor.downField("usage").get[Int]("input_tokens").toOption)
    val outTokens = result.flatMap(_.hcursor.downField("usage").get[Int]("output_tokens").toOption)
    assertEquals(inTokens, Some(10))
    assertEquals(outTokens, Some(25))
  }

  /* ── malformed JSON returns None ──────────────────────────────────────── */

  test("malformed JSON returns None (example: unquoted brace payload)") {
    val sse = List(
      "event: message_start",
      "data: {not valid json}",
      "",
    ).mkString("\n")

    assertEquals(SseParser.parseSseStream(sse), None)
  }

  test("malformed JSON returns None (example: truncated JSON object)") {
    val sse = List(
      "event: message_start",
      """data: {"type":"message_start","message":{""",
      "",
    ).mkString("\n")

    assertEquals(SseParser.parseSseStream(sse), None)
  }

  /* ── full stream with multiple events ─────────────────────────────────── */

  test("full stream with multiple events (example: text block plus message_delta)") {
    val sse = List(
      "event: message_start",
      """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3-5-sonnet","content":[],"usage":{"input_tokens":100}}}""",
      "",
      "event: content_block_start",
      """data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}""",
      "",
      "event: content_block_delta",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hi"}}""",
      "",
      "event: content_block_delta",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"!"}}""",
      "",
      "event: message_delta",
      """data: {"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":5}}""",
      "",
    ).mkString("\n")

    val result = SseParser.parseSseStream(sse)
    val text   = result.flatMap(_.hcursor.downField("content").downN(0).get[String]("text").toOption)
    val stop   = result.flatMap(_.hcursor.get[String]("stop_reason").toOption)
    assertEquals(text, Some("Hi!"))
    assertEquals(stop, Some("end_turn"))
  }

  test("full stream with multiple events (example: thinking block followed by text block)") {
    val sse = List(
      "event: message_start",
      """data: {"type":"message_start","message":{"id":"msg_2","model":"claude-3","content":[],"usage":{"input_tokens":7}}}""",
      "",
      "event: content_block_start",
      """data: {"type":"content_block_start","index":0,"content_block":{"type":"thinking","thinking":""}}""",
      "",
      "event: content_block_delta",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"Plan"}}""",
      "",
      "event: content_block_start",
      """data: {"type":"content_block_start","index":1,"content_block":{"type":"text","text":""}}""",
      "",
      "event: content_block_delta",
      """data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"Answer"}}""",
      "",
      "event: message_delta",
      """data: {"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":3}}""",
      "",
    ).mkString("\n")

    val result   = SseParser.parseSseStream(sse)
    val thinking = result.flatMap(_.hcursor.downField("content").downN(0).get[String]("thinking").toOption)
    val text     = result.flatMap(_.hcursor.downField("content").downN(1).get[String]("text").toOption)
    val stop     = result.flatMap(_.hcursor.get[String]("stop_reason").toOption)
    assertEquals(thinking, Some("Plan"))
    assertEquals(text, Some("Answer"))
    assertEquals(stop, Some("end_turn"))
  }
}
