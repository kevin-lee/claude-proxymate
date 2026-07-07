package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object SseParserSpec extends Properties {

  override def tests: List[Test] = List(
    property("empty string returns None", testEmptyString),
    property("message_start reconstructs message", testMessageStart),
    property("content_block_delta accumulates text", testTextDeltaAccumulation),
    property("thinking_delta accumulates thinking", testThinkingDelta),
    property("message_delta merges usage stats", testMessageDeltaUsage),
    property("malformed JSON returns None", testMalformedJson),
    property("full stream with multiple events", testFullStream),
  )

  def testEmptyString: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
      val result = SseParser.parseSseStream("")
      Result.assert(result.isEmpty)
    }

  def testMessageStart: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
      val sse = List(
        "event: message_start",
        """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3","content":[],"usage":{"input_tokens":10}}}""",
        "",
      ).mkString("\n")

      val result = SseParser.parseSseStream(sse)
      Result.assert(result.isDefined).and {
        val json = result.get
        val id   = json.hcursor.get[String]("id").toOption
        Result.assert(id.contains("msg_1"))
      }
    }

  def testTextDeltaAccumulation: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
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

      val result = SseParser.parseSseStream(sse)
      Result.assert(result.isDefined).and {
        val text = result
          .get
          .hcursor
          .downField("content")
          .downN(0)
          .get[String]("text")
          .toOption
        Result.assert(text.contains("Hello world"))
      }
    }

  def testThinkingDelta: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
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

      val result = SseParser.parseSseStream(sse)
      Result.assert(result.isDefined).and {
        val thinking = result
          .get
          .hcursor
          .downField("content")
          .downN(0)
          .get[String]("thinking")
          .toOption
        Result.assert(thinking.contains("Let me think"))
      }
    }

  def testMessageDeltaUsage: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
      val sse = List(
        "event: message_start",
        """data: {"type":"message_start","message":{"id":"msg_1","model":"claude-3","content":[],"usage":{"input_tokens":10}}}""",
        "",
        "event: message_delta",
        """data: {"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":20}}""",
        "",
      ).mkString("\n")

      val result = SseParser.parseSseStream(sse)
      Result.assert(result.isDefined).and {
        val json       = result.get
        val stopReason = json.hcursor.get[String]("stop_reason").toOption
        val outTokens  = json.hcursor.downField("usage").get[Int]("output_tokens").toOption
        Result.assert(stopReason.contains("end_turn")).and(Result.assert(outTokens.contains(20)))
      }
    }

  def testMalformedJson: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
      val sse = List(
        "event: message_start",
        "data: {not valid json}",
        "",
      ).mkString("\n")

      val result = SseParser.parseSseStream(sse)
      Result.assert(result.isEmpty)
    }

  def testFullStream: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
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
      Result.assert(result.isDefined).and {
        val json = result.get
        val text = json.hcursor.downField("content").downN(0).get[String]("text").toOption
        val stop = json.hcursor.get[String]("stop_reason").toOption
        Result.assert(text.contains("Hi!")).and(Result.assert(stop.contains("end_turn")))
      }
    }
}
