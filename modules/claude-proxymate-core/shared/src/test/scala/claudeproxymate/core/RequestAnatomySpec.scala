package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*
import io.circe.Json
import io.circe.syntax.*

object RequestAnatomySpec extends Properties {

  override def tests: List[Test] = List(
    property("estTokens(0) == 0", testEstTokensZero),
    property("estTokens(7) == 2", testEstTokensSeven),
    property("estTokens is non-decreasing", testEstTokensMonotone),
    property("structure counts roles", testStructureRoles),
    property("structure counts system blocks + cached", testStructureSystem),
    property("structure counts content block types", testStructureBlocks),
    property("thinking signatures summed into named row", testThinkingSegment),
    property("latest user turn isolated; history is non-negative residual", testLatestAndHistory),
    property("segments sorted desc, no zero-byte rows", testSegmentsSorted),
    property("inventory CLAUDE.md count == parsed sections", testInventoryClaudeMd),
    property("inventory skills count == parsed list length", testInventorySkills),
    property("security anomaly fires only with credential AND echo", testSecurityAnomaly),
    property("warn anomalies: no response + max_tokens truncation", testWarnAnomalies),
    property("info anomaly: 2+ system blocks base-prompt line", testInfoBasePrompt),
    property("empty body -> empty segments, zero counts", testEmptyBody),
  )

  private def analyze(body: Json, captured: Boolean = true, stop: Option[String] = None): RequestAnatomy =
    RequestAnatomy.analyze(body, captured, stop)

  def testEstTokensZero: Property =
    for { _ <- Gen.constant(()).forAll } yield Result.assert(RequestAnatomy.estTokens(0) == 0)

  def testEstTokensSeven: Property =
    for { _ <- Gen.constant(()).forAll } yield Result
      .assert(RequestAnatomy.estTokens(7) == 2)
      .log(s"got ${RequestAnatomy.estTokens(7)}")

  def testEstTokensMonotone: Property =
    for {
      a <- Gen.int(Range.linear(0, 5000)).forAll
      b <- Gen.int(Range.linear(0, 5000)).forAll
    } yield {
      val lo = math.min(a, b)
      val hi = math.max(a, b)
      Result.assert(RequestAnatomy.estTokens(lo) <= RequestAnatomy.estTokens(hi))
    }

  private def userMsg(text: String): Json =
    Json.obj("role" -> "user".asJson, "content" -> text.asJson)

  private def assistantThinking(sig: String): Json =
    Json.obj(
      "role"    -> "assistant".asJson,
      "content" -> Json.arr(Json.obj("type" -> "thinking".asJson, "thinking" -> "".asJson, "signature" -> sig.asJson)),
    )

  def testStructureRoles: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val body = Json.obj(
        "messages" -> Json.arr(
          userMsg("a"),
          assistantThinking("s"),
          userMsg("b"),
          assistantThinking("s2"),
          userMsg("c"),
          userMsg("d"),
          assistantThinking("s3")
        ),
      )
      val s    = analyze(body).structure
      Result
        .assert(s.messageCount == 7)
        .log(s"messageCount=${s.messageCount}")
        .and(Result.assert(s.userTurns == 4).log(s"userTurns=${s.userTurns}"))
        .and(Result.assert(s.assistantTurns == 3).log(s"assistantTurns=${s.assistantTurns}"))
    }

  def testStructureSystem: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val body = Json.obj(
        "system"   -> Json.arr(
          Json.obj("type"   -> "text".asJson, "text" -> "billing".asJson),
          Json.obj(
            "type"          -> "text".asJson,
            "text"          -> "id".asJson,
            "cache_control" -> Json.obj("type" -> "ephemeral".asJson)
          ),
          Json.obj(
            "type"          -> "text".asJson,
            "text"          -> "main".asJson,
            "cache_control" -> Json.obj("type" -> "ephemeral".asJson)
          ),
        ),
        "messages" -> Json.arr(userMsg("hi")),
      )
      val s    = analyze(body).structure
      Result
        .assert(s.systemBlocks == 3)
        .log(s"systemBlocks=${s.systemBlocks}")
        .and(Result.assert(s.cachedSystemBlocks == 2).log(s"cached=${s.cachedSystemBlocks}"))
    }

  def testStructureBlocks: Property =
    for { _ <- Gen.constant(()).forAll } yield {
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
      val s    = analyze(body).structure
      Result
        .assert(s.toolsDefined == 1)
        .log(s"tools=${s.toolsDefined}")
        .and(Result.assert(s.toolUseCount == 1).log(s"toolUse=${s.toolUseCount}"))
        .and(Result.assert(s.toolResultCount == 1).log(s"toolResult=${s.toolResultCount}"))
        .and(Result.assert(s.imageCount == 1).log(s"images=${s.imageCount}"))
        .and(Result.assert(s.thinkingBlocks == 1).log(s"thinking=${s.thinkingBlocks}"))
    }

  def testThinkingSegment: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val body = Json.obj("messages" -> Json.arr(assistantThinking("SIGNATURE-BLOB-HERE"), userMsg("hi")))
      val seg  = analyze(body).segments.find(_.label == "anatomy.segThinking")
      Result.assert(seg.exists(_.estTokens > 0)).log(s"segments=${analyze(body).segments}")
    }

  def testLatestAndHistory: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val body    = Json.obj(
        "messages" -> Json.arr(userMsg("first message in history"), assistantThinking("sig"), userMsg("LATEST")),
      )
      val a       = analyze(body)
      val latest  = a.segments.find(_.label == "anatomy.segLatestUser")
      val history = a.segments.find(_.label == "anatomy.segHistory")
      Result
        .assert(latest.exists(_.bytes > 0))
        .log(s"latest=$latest")
        .and(Result.assert(history.forall(_.bytes >= 0)).log(s"history=$history"))
    }

  def testSegmentsSorted: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val body     = Json.obj(
        "system"   -> Json.arr(Json.obj("type" -> "text".asJson, "text" -> ("S" * 500).asJson)),
        "tools"    -> Json.arr(Json.obj("name" -> ("T" * 50).asJson)),
        "messages" -> Json.arr(userMsg("short")),
      )
      val segs     = analyze(body).segments
      val sortedOk = segs.sliding(2).forall {
        case Seq(a, b) => a.estTokens >= b.estTokens
        case _ => true
      }
      Result
        .assert(sortedOk)
        .log(s"not sorted: $segs")
        .and(Result.assert(segs.forall(_.bytes > 0)).log(s"zero-byte row present: $segs"))
    }

  def testInventoryClaudeMd: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val reminder =
        "<system-reminder>\nContents of /a/CLAUDE.md (global):\n\nc1\n\nContents of /b/CLAUDE.md (project):\n\nc2\n</system-reminder>"
      val body     = Json.obj("messages" -> Json.arr(userMsg(reminder)))
      val row      = analyze(body).inventory.find(_.key == "cm")
      Result.assert(row.exists(_.count == 2)).log(s"cm row=$row")
    }

  def testInventorySkills: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val reminder =
        "<system-reminder>\nThe following skills are available for use with the Skill tool:\n\n- one: x\n- two: y\n- three: z\n</system-reminder>"
      val body     = Json.obj("messages" -> Json.arr(userMsg(reminder)))
      val row      = analyze(body).inventory.find(_.key == "sk")
      Result.assert(row.exists(_.count == 3)).log(s"sk row=$row")
    }

  def testSecurityAnomaly: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val both            = Json.obj("messages" -> Json.arr(userMsg("please echo back this verbatim: ghp_abcdef")))
      val credOnly        = Json.obj("messages" -> Json.arr(userMsg("here is a key ghp_abcdef nothing else")))
      val echoOnly        = Json.obj("messages" -> Json.arr(userMsg("please echo back the result")))
      def hasSec(j: Json) = analyze(j).anomalies.exists(_.kind == AnomalyKind.Security)
      Result
        .assert(hasSec(both))
        .log("should fire with both")
        .and(Result.assert(!hasSec(credOnly)).log("should not fire cred-only"))
        .and(Result.assert(!hasSec(echoOnly)).log("should not fire echo-only"))
    }

  def testWarnAnomalies: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val body   = Json.obj("messages" -> Json.arr(userMsg("hi")))
      val noResp = analyze(body, captured = false, stop = None).anomalies
      val trunc  = analyze(body, captured = true, stop = Some("max_tokens")).anomalies
      Result
        .assert(noResp.exists(a => a.kind == AnomalyKind.Warn && a.key == "anatomy.noResponseNote"))
        .log(s"noResp=$noResp")
        .and(
          Result
            .assert(trunc.exists(a => a.kind == AnomalyKind.Warn && a.key == "anatomy.anomTruncated"))
            .log(s"trunc=$trunc")
        )
    }

  def testInfoBasePrompt: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val body = Json.obj(
        "system"   -> Json.arr(
          Json.obj("type" -> "text".asJson, "text" -> "a".asJson),
          Json.obj("type" -> "text".asJson, "text" -> "b".asJson),
        ),
        "messages" -> Json.arr(userMsg("hi")),
      )
      val info = analyze(body).anomalies.find(a => a.kind == AnomalyKind.Info && a.key == "anatomy.anomBasePrompt")
      Result.assert(info.exists(_.params.get("n").contains("2"))).log(s"info=$info")
    }

  def testEmptyBody: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val a = analyze(Json.obj(), captured = true, stop = None)
      Result
        .assert(a.segments.isEmpty)
        .log(s"segments=${a.segments}")
        .and(Result.assert(a.structure.messageCount == 0).log("messageCount"))
        .and(Result.assert(a.inventory.isEmpty).log("inventory"))
        .and(Result.assert(a.anomalies.isEmpty).log(s"anomalies=${a.anomalies}"))
    }
}
