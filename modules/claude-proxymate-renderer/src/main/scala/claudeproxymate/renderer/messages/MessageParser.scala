package claudeproxymate.renderer.messages

import claudeproxymate.core.ClaudeMdParser

import scala.util.matching.Regex

/** Parse user message text into typed portions and injected blocks.
  *
  * Ports `parseUserText` from renderer.js.
  * Uses `ClaudeMdParser.parseClaudeMdSections` from core.
  */
object MessageParser {

  /** A parsed segment of user text. */
  enum Part {
    case TextPart(content: String)
    case InjectedPart(label: String, content: String, cls: String)
  }

  private val blockRe: Regex =
    """(?s)(<system-reminder>.*?</system-reminder>|<command-message>.*?</command-message>)""".r

  private val skillsPattern: Regex     = "(?i)skills are available".r
  private val dateMemoryPattern: Regex = "(?i)currentDate|auto-memory".r
  private val claudeMdPattern: Regex   = "(?i)claudeMd|CLAUDE\\.md".r

  def parseUserText(text: String): List[Part] = {
    val parts = scala.collection.mutable.ListBuffer.empty[Part]
    var pos   = 0

    for (m <- blockRe.findAllMatchIn(text)) {
      if (m.start > pos) {
        val plain = text.substring(pos, m.start).trim
        if (plain.nonEmpty) parts += Part.TextPart(plain)
      }

      val raw = m.matched
      if (raw.startsWith("<system-reminder>")) {
        val inner = raw.substring("<system-reminder>".length, raw.length - "</system-reminder>".length)
        if (skillsPattern.findFirstIn(inner).isDefined) {
          parts += Part.InjectedPart("\uD83D\uDD27 Skills", inner, "green")
        } else if (dateMemoryPattern.findFirstIn(inner).isDefined && claudeMdPattern.findFirstIn(inner).isEmpty) {
          parts += Part.InjectedPart("\uD83E\uDDE0 Memory", inner, "green")
        } else if (claudeMdPattern.findFirstIn(inner).isDefined) {
          val sections = ClaudeMdParser.parseClaudeMdSections(inner)
          if (sections.nonEmpty) {
            sections.foreach(s => parts += Part.InjectedPart(s.label, s.content, s.cls))
          } else {
            parts += Part.InjectedPart("\uD83D\uDCCB CLAUDE.md", inner, "green")
          }
        } else {
          parts += Part.InjectedPart("\uD83D\uDCCB system-reminder", inner, "green")
        }
      } else {
        // command-message
        val inner = raw.substring("<command-message>".length, raw.length - "</command-message>".length)
        parts += Part.InjectedPart("\u2328 slash command", inner, "yellow")
      }

      pos = m.end
    }

    if (pos < text.length) {
      val plain = text.substring(pos).trim
      if (plain.nonEmpty) parts += Part.TextPart(plain)
    }

    parts.toList
  }
}
