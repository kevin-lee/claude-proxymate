package claudeproxymate.core

import io.circe.{Json, JsonObject, Printer}

import scala.util.matching.Regex

/** Detects the 5 prompt augmentation mechanisms in an API request body.
  *
  * Ports `detectMechanisms` from `index.html:1887-1951`.
  */
object MechanismDetector {

  private val systemReminderPattern: Regex =
    """(?s)<system-reminder>(.*?)</system-reminder>""".r

  private val contentsOfPattern: Regex =
    "(?i)Contents of ".r

  private val commandMessagePattern: Regex =
    """(?s)<command-message>(.*?)</command-message>""".r

  private val commandNamePattern: Regex =
    """^<command-name>\s*/(\S+)\s*</command-name>""".r

  private val hashSlashPattern: Regex =
    """^#\s*/(\S+)""".r

  private val isRunningPattern: Regex =
    """^(\S+)\s+is running""".r

  private val wordLikePattern: Regex =
    """^\w[\w-]*$""".r

  def detectMechanisms(body: Json): Mechanisms =
    body.asObject match {
      case None => Mechanisms.empty
      case Some(obj) => detectFromObject(obj)
    }

  private def detectFromObject(obj: JsonObject): Mechanisms = {
    val outputStyle = extractOutputStyle(obj)
    val msgs        = obj("messages").flatMap(_.asArray).getOrElse(Vector.empty)

    val withMessages = msgs.foldLeft(Mechanisms.empty.copy(outputStyle = outputStyle)) { (acc, msg) =>
      val contents = extractContents(msg)
      contents.foldLeft(acc)(processContent)
    }

    withMessages
  }

  private def extractOutputStyle(obj: JsonObject): Option[List[String]] =
    obj("system").flatMap(_.asArray).flatMap { arr =>
      if (arr.size >= 2) {
        val texts = arr.flatMap { s =>
          for {
            o    <- s.asObject
            typ  <- o("type").flatMap(_.asString) if typ == "text"
            text <- o("text").flatMap(_.asString)
          } yield text
        }
        Some(texts.toList)
      } else None
    }

  private def extractContents(msg: Json): List[Json] =
    msg
      .asObject
      .flatMap { m =>
        m("content").map { c =>
          c.asArray.map(_.toList).getOrElse {
            c.asString
              .map(s => List(Json.obj("type" -> Json.fromString("text"), "text" -> Json.fromString(s))))
              .getOrElse(Nil)
          }
        }
      }
      .getOrElse(Nil)

  private def processContent(acc: Mechanisms, c: Json): Mechanisms = {
    val cObj  = c.asObject.getOrElse(JsonObject.empty)
    val cType = cObj("type").flatMap(_.asString).getOrElse("")
    val cName = cObj("name").flatMap(_.asString)
    val cId   = cObj("id").flatMap(_.asString).getOrElse("")

    cType match {
      case "text" =>
        cObj("text").flatMap(_.asString) match {
          case Some(text) => processText(acc, text)
          case None => acc
        }

      case "tool_use" =>
        cName match {
          case Some(name) if name == "Skill" =>
            val input = cObj("input").getOrElse(Json.Null)
            acc.copy(skills = acc.skills :+ SkillUse(cId, input, None))

          case Some(name) if name == "Task" || name == "Agent" =>
            val input = cObj("input").getOrElse(Json.Null)
            acc.copy(subAgents = acc.subAgents :+ SubAgent(cId, name, input))

          case Some(name) if name.startsWith("mcp__") =>
            val input = cObj("input").getOrElse(Json.Null)
            acc.copy(mcpTools = acc.mcpTools :+ McpTool(cId, name, input, None))

          case _ => acc
        }

      case "tool_result" =>
        val toolUseId     = cObj("tool_use_id").flatMap(_.asString).getOrElse("")
        val resultContent = cObj("content").map { ct =>
          ct.asString.getOrElse(ct.printWith(Printer.spaces2))
        }
        acc.copy(
          skills = acc.skills.map { s =>
            if (s.id == toolUseId) s.copy(result = resultContent) else s
          },
          mcpTools = acc.mcpTools.map { m =>
            if (m.id == toolUseId) m.copy(result = resultContent) else m
          },
        )

      case _ => acc
    }
  }

  private def processText(acc: Mechanisms, text: String): Mechanisms = {
    val withClaudeMd      = collectClaudeMd(acc, text)
    val withSlashCommands = collectSlashCommands(withClaudeMd, text)
    withSlashCommands
  }

  private def collectClaudeMd(acc: Mechanisms, text: String): Mechanisms =
    systemReminderPattern.findAllMatchIn(text).foldLeft(acc) { (a, m) =>
      val inner = m.group(1).trim
      if (contentsOfPattern.findFirstIn(inner).isDefined) {
        val updated = a.claudeMd match {
          case Some(existing) => Some(existing + "\n\n" + inner)
          case None => Some(inner)
        }
        a.copy(claudeMd = updated)
      } else a
    }

  private def collectSlashCommands(acc: Mechanisms, text: String): Mechanisms =
    commandMessagePattern.findAllMatchIn(text).foldLeft(acc) { (a, cmdMatch) =>
      val tag      = cmdMatch.group(1).trim
      val endIdx   = cmdMatch.end
      val afterTag = text.slice(endIdx, Math.min(endIdx + 200, text.length))

      val name = commandNamePattern.findFirstMatchIn(afterTag) match {
        case Some(m) => m.group(1)
        case None =>
          hashSlashPattern
            .findFirstMatchIn(tag)
            .map(_.group(1))
            .orElse(isRunningPattern.findFirstMatchIn(tag).map(_.group(1)))
            .getOrElse {
              if (wordLikePattern.findFirstIn(tag).isDefined) tag
              else s"Cmd ${a.slashCommands.size + 1}"
            }
      }
      a.copy(slashCommands = a.slashCommands :+ SlashCommand(name, tag, text))
    }
}
