package claudeproxymate.core

import io.circe.Json
import io.circe.syntax.*

/* munit port of the shared hedgehog RequestAnatomySpec for Scala
 * Native (the hedgehog runner hangs on SN 0.5, so coreNative runs
 * munit-only tests). All shared properties except the monotonicity one
 * use a constant generator (a single fixed input), so they port 1:1 to
 * deterministic tests; "estTokens is non-decreasing" is exercised over
 * representative input pairs instead of random ones. */
class RequestAnatomySpec extends munit.FunSuite {

  /* Common analysis case: response captured, no stop reason. */
  private def analyzeCaptured(body: Json): RequestAnatomy =
    RequestAnatomy.analyze(body, responseCaptured = true, stopReason = None)

  private def userMsg(text: String): Json =
    Json.obj("role" -> "user".asJson, "content" -> text.asJson)

  private def assistantThinking(sig: String): Json =
    Json.obj(
      "role"    -> "assistant".asJson,
      "content" -> Json.arr(Json.obj("type" -> "thinking".asJson, "thinking" -> "".asJson, "signature" -> sig.asJson)),
    )

  test("estTokens(0) == 0") {
    assertEquals(RequestAnatomy.estTokens(0), 0)
  }

  test("estTokens(7) == 2") {
    assertEquals(RequestAnatomy.estTokens(7), 2)
  }

  /* Property: estTokens is non-decreasing. Representative pairs cover
   * the generator's Range.linear(0, 5000): the zero edge, adjacent
   * inputs across a rounding step, and the upper boundary where the
   * ceil outputs tie. */

  test("estTokens is non-decreasing (example: 0 <= 1)") {
    assert(RequestAnatomy.estTokens(0) <= RequestAnatomy.estTokens(1))
  }

  test("estTokens is non-decreasing (example: adjacent 7 <= 8)") {
    assert(RequestAnatomy.estTokens(7) <= RequestAnatomy.estTokens(8))
  }

  test("estTokens is non-decreasing (example: boundary 4999 <= 5000, equal outputs)") {
    assert(RequestAnatomy.estTokens(4999) <= RequestAnatomy.estTokens(5000))
  }

  test("structure counts roles") {
    val body = Json.obj(
      "messages" -> Json.arr(
        userMsg("a"),
        assistantThinking("s"),
        userMsg("b"),
        assistantThinking("s2"),
        userMsg("c"),
        userMsg("d"),
        assistantThinking("s3"),
      ),
    )
    val s    = analyzeCaptured(body).structure
    assertEquals(s.messageCount, 7)
    assertEquals(s.userTurns, 4)
    assertEquals(s.assistantTurns, 3)
  }

  test("structure counts system blocks + cached") {
    val body = Json.obj(
      "system"   -> Json.arr(
        Json.obj("type" -> "text".asJson, "text" -> "billing".asJson),
        Json.obj(
          "type"          -> "text".asJson,
          "text"          -> "id".asJson,
          "cache_control" -> Json.obj("type" -> "ephemeral".asJson),
        ),
        Json.obj(
          "type"          -> "text".asJson,
          "text"          -> "main".asJson,
          "cache_control" -> Json.obj("type" -> "ephemeral".asJson),
        ),
      ),
      "messages" -> Json.arr(userMsg("hi")),
    )
    val s    = analyzeCaptured(body).structure
    assertEquals(s.systemBlocks, 3)
    assertEquals(s.cachedSystemBlocks, 2)
  }

  test("structure counts content block types") {
    val body = Json.obj(
      "tools"    -> Json.arr(Json.obj("name" -> "T".asJson)),
      "messages" -> Json.arr(
        Json.obj(
          "role"    -> "assistant".asJson,
          "content" -> Json.arr(
            Json.obj("type" -> "thinking".asJson, "signature" -> "x".asJson),
            Json
              .obj("type" -> "tool_use".asJson, "name" -> "Skill".asJson, "id" -> "u1".asJson, "input" -> Json.obj()),
          ),
        ),
        Json.obj(
          "role"    -> "user".asJson,
          "content" -> Json.arr(
            Json.obj("type" -> "tool_result".asJson, "tool_use_id" -> "u1".asJson, "content" -> "r".asJson),
            Json.obj("type" -> "image".asJson, "source"            -> Json.obj()),
          ),
        ),
      ),
    )
    val s    = analyzeCaptured(body).structure
    assertEquals(s.toolsDefined, 1)
    assertEquals(s.toolUseCount, 1)
    assertEquals(s.toolResultCount, 1)
    assertEquals(s.imageCount, 1)
    assertEquals(s.thinkingBlocks, 1)
  }

  test("thinking signatures summed into named row") {
    val body    = Json.obj("messages" -> Json.arr(assistantThinking("SIGNATURE-BLOB-HERE"), userMsg("hi")))
    val anatomy = analyzeCaptured(body)
    val seg     = anatomy.segments.find(_.label == "anatomy.segThinking")
    assert(seg.exists(_.estTokens > 0), s"segments=${anatomy.segments}")
  }

  test("latest user turn isolated; history is non-negative residual") {
    val body    = Json.obj(
      "messages" -> Json.arr(userMsg("first message in history"), assistantThinking("sig"), userMsg("LATEST")),
    )
    val anatomy = analyzeCaptured(body)
    val latest  = anatomy.segments.find(_.label == "anatomy.segLatestUser")
    val history = anatomy.segments.find(_.label == "anatomy.segHistory")
    assert(latest.exists(_.bytes > 0), s"latest=$latest")
    assert(history.forall(_.bytes >= 0), s"history=$history")
  }

  test("segments sorted desc, no zero-byte rows") {
    val body     = Json.obj(
      "system"   -> Json.arr(Json.obj("type" -> "text".asJson, "text" -> ("S" * 500).asJson)),
      "tools"    -> Json.arr(Json.obj("name" -> ("T" * 50).asJson)),
      "messages" -> Json.arr(userMsg("short")),
    )
    val segs     = analyzeCaptured(body).segments
    val sortedOk = segs.sliding(2).forall {
      case Seq(a, b) => a.estTokens >= b.estTokens
      case _ => true
    }
    assert(sortedOk, s"not sorted: $segs")
    assert(segs.forall(_.bytes > 0), s"zero-byte row present: $segs")
  }

  test("inventory CLAUDE.md count == parsed sections") {
    val reminder =
      "<system-reminder>\nContents of /a/CLAUDE.md (global):\n\nc1\n\nContents of /b/CLAUDE.md (project):\n\nc2\n</system-reminder>"
    val body     = Json.obj("messages" -> Json.arr(userMsg(reminder)))
    val row      = analyzeCaptured(body).inventory.find(_.key == "cm")
    assertEquals(row.map(_.count), Some(2))
  }

  test("inventory skills count == parsed list length") {
    val reminder =
      "<system-reminder>\nThe following skills are available for use with the Skill tool:\n\n- one: x\n- two: y\n- three: z\n</system-reminder>"
    val body     = Json.obj("messages" -> Json.arr(userMsg(reminder)))
    val row      = analyzeCaptured(body).inventory.find(_.key == "sk")
    assertEquals(row.map(_.count), Some(3))
  }

  test("security anomaly fires only with credential AND echo") {
    val both     = Json.obj("messages" -> Json.arr(userMsg("please echo back this verbatim: ghp_abcdef")))
    val credOnly = Json.obj("messages" -> Json.arr(userMsg("here is a key ghp_abcdef nothing else")))
    val echoOnly = Json.obj("messages" -> Json.arr(userMsg("please echo back the result")))

    def hasSec(j: Json): Boolean = analyzeCaptured(j).anomalies.exists(_.kind == AnomalyKind.Security)

    assert(hasSec(both), "should fire with both")
    assert(!hasSec(credOnly), "should not fire cred-only")
    assert(!hasSec(echoOnly), "should not fire echo-only")
  }

  test("warn anomalies: no response + max_tokens truncation") {
    val body   = Json.obj("messages" -> Json.arr(userMsg("hi")))
    val noResp = RequestAnatomy.analyze(body, responseCaptured = false, stopReason = None).anomalies
    val trunc  = RequestAnatomy.analyze(body, responseCaptured = true, stopReason = Some("max_tokens")).anomalies
    assert(noResp.exists(a => a.kind == AnomalyKind.Warn && a.key == "anatomy.noResponseNote"), s"noResp=$noResp")
    assert(trunc.exists(a => a.kind == AnomalyKind.Warn && a.key == "anatomy.anomTruncated"), s"trunc=$trunc")
  }

  test("info anomaly: 2+ system blocks base-prompt line") {
    val body = Json.obj(
      "system"   -> Json.arr(
        Json.obj("type" -> "text".asJson, "text" -> "a".asJson),
        Json.obj("type" -> "text".asJson, "text" -> "b".asJson),
      ),
      "messages" -> Json.arr(userMsg("hi")),
    )
    val info = analyzeCaptured(body)
      .anomalies
      .find(a => a.kind == AnomalyKind.Info && a.key == "anatomy.anomBasePrompt")
    assert(info.exists(_.params.get("n").contains("2")), s"info=$info")
  }

  test("empty body -> empty segments, zero counts") {
    val anatomy = analyzeCaptured(Json.obj())
    assertEquals(anatomy.segments, List.empty[SegmentSize])
    assertEquals(anatomy.structure.messageCount, 0)
    assertEquals(anatomy.inventory, List.empty[InventoryRow])
    assertEquals(anatomy.anomalies, List.empty[Anomaly])
  }
}
