package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*
import io.circe.Json
import io.circe.syntax.*

object MechanismDetectorSpec extends Properties {

  override def tests: List[Test] = List(
    property("system-reminder with Contents of -> claudeMd detected", testClaudeMd),
    property("system array 2+ items -> outputStyle detected", testOutputStyle),
    property("command-message -> slashCommands detected", testSlashCommand),
    property("tool_use Skill -> skills detected", testSkillToolUse),
    property("tool_use Agent -> subAgents detected", testAgentToolUse),
    property("tool_use mcp__ -> mcpTools detected", testMcpToolUse),
    property("empty body -> all empty", testEmptyBody),
    property("tool_result associates with skill", testToolResultSkill),
  )

  def testClaudeMd: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
      val body = Json.obj(
        "messages" -> Json.arr(
          Json.obj(
            "role" -> "user".asJson,
            "content" -> "<system-reminder>\nContents of /path/CLAUDE.md (desc):\n\ncontent\n</system-reminder>\nhello".asJson,
          )
        )
      )
      val det  = MechanismDetector.detectMechanisms(body)
      Result.assert(det.claudeMd.isDefined).and(Result.assert(det.claudeMd.exists(_.contains("Contents of"))))
    }

  def testOutputStyle: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
      val body = Json.obj(
        "system"   -> Json.arr(
          Json.obj("type" -> "text".asJson, "text" -> "base system".asJson),
          Json.obj("type" -> "text".asJson, "text" -> "output style".asJson),
        ),
        "messages" -> Json.arr(),
      )
      val det  = MechanismDetector.detectMechanisms(body)
      Result.assert(det.outputStyle.isDefined).and(Result.assert(det.outputStyle.exists(_.length == 2)))
    }

  def testSlashCommand: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
      val body = Json.obj(
        "messages" -> Json.arr(
          Json.obj(
            "role"    -> "user".asJson,
            "content" -> "<command-message>commit</command-message>\n/commit".asJson,
          )
        )
      )
      val det  = MechanismDetector.detectMechanisms(body)
      Result.assert(det.slashCommands.nonEmpty).and(Result.assert(det.slashCommands.head.tag.contains("commit")))
    }

  def testSkillToolUse: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
      val body = Json.obj(
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
      val det  = MechanismDetector.detectMechanisms(body)
      Result.assert(det.skills.length == 1).and {
        val skillInput = det.skills.head.input.hcursor.get[String]("skill").toOption
        Result.assert(skillInput.contains("e2e"))
      }
    }

  def testAgentToolUse: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
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
      Result.assert(det.subAgents.length == 1).and(Result.assert(det.subAgents.head.name == "Agent"))
    }

  def testMcpToolUse: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
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
      Result.assert(det.mcpTools.length == 1).and(Result.assert(det.mcpTools.head.name == "mcp__serena__find_symbol"))
    }

  def testEmptyBody: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
      val det = MechanismDetector.detectMechanisms(Json.obj())
      Result
        .assert(det.claudeMd.isEmpty)
        .and(Result.assert(det.outputStyle.isEmpty))
        .and(Result.assert(det.slashCommands.isEmpty))
        .and(Result.assert(det.skills.isEmpty))
        .and(Result.assert(det.subAgents.isEmpty))
        .and(Result.assert(det.mcpTools.isEmpty))
    }

  def testToolResultSkill: Property =
    for {
      _ <- Gen.constant(()).forAll
    } yield {
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
      Result.assert(det.skills.length == 1).and(Result.assert(det.skills.head.result.contains("Skill result here")))
    }
}
