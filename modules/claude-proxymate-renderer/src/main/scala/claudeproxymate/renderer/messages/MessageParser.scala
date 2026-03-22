package claudeproxymate.renderer.messages

import claudeproxymate.core.ClaudeMdParser

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.matching.Regex

/** Parse user message text into typed portions and injected blocks.
  *
  * Ports `parseUserText` from renderer.js.
  * Uses `ClaudeMdParser.parseClaudeMdSections` from core.
  */
object MessageParser {

  /** A parsed segment of user text. */
  sealed trait Part {
    def tpe: String
  }
  final case class TextPart(content: String) extends Part { val tpe = "text" }
  final case class InjectedPart(label: String, content: String, cls: String) extends Part { val tpe = "injected" }

  private val blockRe: Regex =
    """(?s)(<system-reminder>.*?</system-reminder>|<command-message>.*?</command-message>)""".r

  private val skillsPattern: Regex   = "(?i)skills are available".r
  private val dateMemoryPattern: Regex = "(?i)currentDate|auto-memory".r
  private val claudeMdPattern: Regex = "(?i)claudeMd|CLAUDE\\.md".r

  @JSExportTopLevel("parseUserText")
  def parseUserTextJs(text: String): js.Array[js.Dynamic] = {
    val parts = parseUserText(text)
    js.Array(parts.map { p =>
      val obj = js.Dynamic.literal("type" -> p.tpe)
      p match {
        case TextPart(content) =>
          obj.content = content
        case InjectedPart(label, content, cls) =>
          obj.label = label
          obj.content = content
          obj.cls = cls
      }
      obj
    }*)
  }

  /** JS-callable bridge for `ClaudeMdParser.parseClaudeMdSections`.
    * Returns `js.Array[js.Dynamic]` with `{label, path, content, cls, scope}` fields.
    * Used by remaining JS functions (buildMechFilterChips, etc.) until they are ported.
    */
  @JSExportTopLevel("parseClaudeMdSections")
  def parseClaudeMdSectionsJs(inner: String): js.Array[js.Dynamic] = {
    val sections = ClaudeMdParser.parseClaudeMdSections(inner)
    js.Array(sections.map { s =>
      js.Dynamic.literal(
        "label" -> s.label,
        "path" -> s.path,
        "content" -> s.content,
        "cls" -> s.cls,
        "scope" -> s.scope,
      )
    }*)
  }

  def parseUserText(text: String): List[Part] = {
    val parts = scala.collection.mutable.ListBuffer.empty[Part]
    var pos = 0

    for (m <- blockRe.findAllMatchIn(text)) {
      if (m.start > pos) {
        val plain = text.substring(pos, m.start).trim
        if (plain.nonEmpty) parts += TextPart(plain)
      }

      val raw = m.matched
      if (raw.startsWith("<system-reminder>")) {
        val inner = raw.substring("<system-reminder>".length, raw.length - "</system-reminder>".length)
        if (skillsPattern.findFirstIn(inner).isDefined) {
          parts += InjectedPart("\uD83D\uDD27 Skills", inner, "green")
        } else if (dateMemoryPattern.findFirstIn(inner).isDefined && claudeMdPattern.findFirstIn(inner).isEmpty) {
          parts += InjectedPart("\uD83E\uDDE0 Memory", inner, "green")
        } else if (claudeMdPattern.findFirstIn(inner).isDefined) {
          val sections = ClaudeMdParser.parseClaudeMdSections(inner)
          if (sections.nonEmpty) {
            sections.foreach(s => parts += InjectedPart(s.label, s.content, s.cls))
          } else {
            parts += InjectedPart("\uD83D\uDCCB CLAUDE.md", inner, "green")
          }
        } else {
          parts += InjectedPart("\uD83D\uDCCB system-reminder", inner, "green")
        }
      } else {
        // command-message
        val inner = raw.substring("<command-message>".length, raw.length - "</command-message>".length)
        parts += InjectedPart("\u2328 slash command", inner, "yellow")
      }

      pos = m.end
    }

    if (pos < text.length) {
      val plain = text.substring(pos).trim
      if (plain.nonEmpty) parts += TextPart(plain)
    }

    parts.toList
  }
}
