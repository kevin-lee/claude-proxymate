package claudeproxymate.core.filter

import scala.util.matching.Regex

/** One segment of a user text block: typed text, or an injected block Claude
  * Code wrapped in `<system-reminder>` / `<command-message>` tags.
  */
enum Block {
  case Plain(text: String)
  case SystemReminder(raw: String, inner: String)
  case CommandMessage(raw: String)

  /** The exact source text of this segment. */
  def source: String = this match {
    case Block.Plain(text) => text
    case Block.SystemReminder(raw, _) => raw
    case Block.CommandMessage(raw) => raw
  }
}

/** Splits user text into typed text and injected blocks without losing a byte:
  * `join(split(text)) == text`.
  *
  * The block regex is the one the renderer's `MessageParser` uses, so the
  * filter and the Messages tab agree on what counts as injected.
  */
object ReminderBlocks {

  val ReminderOpen: String  = "<system-reminder>"
  val ReminderClose: String = "</system-reminder>"

  val Pattern: Regex =
    """(?s)(<system-reminder>.*?</system-reminder>|<command-message>.*?</command-message>)""".r

  def split(text: String): List[Block] = {
    val (reversed, pos) = Pattern.findAllMatchIn(text).foldLeft((List.empty[Block], 0)) {
      case ((acc, pos), m) =>
        val withPlain = if (m.start > pos) Block.Plain(text.substring(pos, m.start)) :: acc else acc
        val raw       = m.matched
        val block     =
          if (raw.startsWith(ReminderOpen)) {
            Block.SystemReminder(raw, raw.substring(ReminderOpen.length, raw.length - ReminderClose.length))
          } else {
            Block.CommandMessage(raw)
          }
        (block :: withPlain, m.end)
    }
    val tail            = if (pos < text.length) List(Block.Plain(text.substring(pos))) else Nil
    reversed.reverse ++ tail
  }

  def join(blocks: List[Block]): String = blocks.map(_.source).mkString

  def wrapReminder(inner: String): String = ReminderOpen + inner + ReminderClose
}
