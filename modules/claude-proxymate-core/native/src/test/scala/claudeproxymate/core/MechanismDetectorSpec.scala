package claudeproxymate.core

import io.circe.Json
import io.circe.syntax.*

/* munit port of the shared hedgehog MechanismDetectorSpec for Scala
 * Native (the hedgehog runner hangs on SN 0.5, so coreNative runs
 * munit-only tests). Every shared property uses a constant generator
 * (a single fixed input), so each ports 1:1 to a deterministic test
 * with the same body and expectations. */
class MechanismDetectorSpec extends munit.FunSuite {

  test("system-reminder with Contents of -> claudeMd detected") {
    val body = Json.obj(
      "messages" -> Json.arr(
        Json.obj(
          "role"    -> "user".asJson,
          "content" -> "<system-reminder>\nContents of /path/CLAUDE.md (desc):\n\ncontent\n</system-reminder>\nhello".asJson,
        )
      )
    )
    val det  = MechanismDetector.detectMechanisms(body)
    assert(det.claudeMd.isDefined)
    assert(det.claudeMd.exists(_.contains("Contents of")))
  }

  test("system array 2+ items -> outputStyle detected") {
    val body = Json.obj(
      "system"   -> Json.arr(
        Json.obj("type" -> "text".asJson, "text" -> "base system".asJson),
        Json.obj("type" -> "text".asJson, "text" -> "output style".asJson),
      ),
      "messages" -> Json.arr(),
    )
    val det  = MechanismDetector.detectMechanisms(body)
    assertEquals(det.outputStyle.map(_.length), Some(2))
  }

  test("command-message -> slashCommands detected") {
    val body = Json.obj(
      "messages" -> Json.arr(
        Json.obj(
          "role"    -> "user".asJson,
          "content" -> "<command-message>commit</command-message>\n/commit".asJson,
        )
      )
    )
    val det  = MechanismDetector.detectMechanisms(body)
    assert(det.slashCommands.nonEmpty)
    assert(det.slashCommands.headOption.exists(_.tag.contains("commit")))
  }

  test("tool_use Skill -> skills detected") {
    val body       = Json.obj(
      "messages" -> Json.arr(
        Json.obj(
          "role"    -> "assistant".asJson,
          "content" -> Json.arr(
            Json.obj(
              "type"  -> "tool_use".asJson,
              "id"    -> "tu_1".asJson,
              "name"  -> "Skill".asJson,
              "input" -> Json.obj("skill" -> "e2e".asJson),
            )
          ),
        )
      )
    )
    val det        = MechanismDetector.detectMechanisms(body)
    assertEquals(det.skills.length, 1)
    val skillInput = det.skills.headOption.flatMap(_.input.hcursor.get[String]("skill").toOption)
    assertEquals(skillInput, Some("e2e"))
  }

  test("tool_use Agent -> subAgents detected") {
    val body = Json.obj(
      "messages" -> Json.arr(
        Json.obj(
          "role"    -> "assistant".asJson,
          "content" -> Json.arr(
            Json.obj(
              "type"  -> "tool_use".asJson,
              "id"    -> "tu_2".asJson,
              "name"  -> "Agent".asJson,
              "input" -> Json.obj("description" -> "test agent".asJson),
            )
          ),
        )
      )
    )
    val det  = MechanismDetector.detectMechanisms(body)
    assertEquals(det.subAgents.length, 1)
    assertEquals(det.subAgents.headOption.map(_.name), Some("Agent"))
  }

  test("tool_use mcp__ -> mcpTools detected") {
    val body = Json.obj(
      "messages" -> Json.arr(
        Json.obj(
          "role"    -> "assistant".asJson,
          "content" -> Json.arr(
            Json.obj(
              "type"  -> "tool_use".asJson,
              "id"    -> "tu_3".asJson,
              "name"  -> "mcp__serena__find_symbol".asJson,
              "input" -> Json.obj("name" -> "Foo".asJson),
            )
          ),
        )
      )
    )
    val det  = MechanismDetector.detectMechanisms(body)
    assertEquals(det.mcpTools.length, 1)
    assertEquals(det.mcpTools.headOption.map(_.name), Some("mcp__serena__find_symbol"))
  }

  test("empty body -> all empty") {
    val det = MechanismDetector.detectMechanisms(Json.obj())
    assertEquals(det, Mechanisms.empty)
  }

  test("tool_result associates with skill") {
    val body = Json.obj(
      "messages" -> Json.arr(
        Json.obj(
          "role"    -> "assistant".asJson,
          "content" -> Json.arr(
            Json.obj(
              "type"  -> "tool_use".asJson,
              "id"    -> "tu_s1".asJson,
              "name"  -> "Skill".asJson,
              "input" -> Json.obj("skill" -> "commit".asJson),
            )
          ),
        ),
        Json.obj(
          "role"    -> "user".asJson,
          "content" -> Json.arr(
            Json.obj(
              "type"        -> "tool_result".asJson,
              "tool_use_id" -> "tu_s1".asJson,
              "content"     -> "Skill result here".asJson,
            )
          ),
        ),
      )
    )
    val det  = MechanismDetector.detectMechanisms(body)
    assertEquals(det.skills.length, 1)
    assertEquals(det.skills.headOption.flatMap(_.result), Some("Skill result here"))
  }
}
