package claudeproxymate.core.filter

import cats.syntax.all.*

import scala.annotation.tailrec

/** One `- name: description` entry of the skills reminder, as the half-open
  * range `[start, end)` of the reminder's inner text (the range includes the
  * newline that ends the entry's last line, when there is one).
  */
final case class SkillEntry(name: String, start: Int, end: Int)

/** Parses and edits the "skills are available for use with the Skill tool"
  * reminder. An entry starts at a line beginning with `- ` after the marker
  * and runs until the next such line, a blank line, or the end of the text,
  * so multi-line descriptions stay with their entry.
  */
object SkillsList {

  val Marker: String = "skills are available for use with the Skill tool"

  private val EntryPrefix: String = "- "

  def isSkillsReminder(inner: String): Boolean = inner.contains(Marker)

  /** `(start, endExclusiveIncludingNewline, content)` for every line at or after `from`. */
  private def lines(text: String, from: Int): List[(Int, Int, String)] = {
    @tailrec
    def loop(pos: Int, acc: List[(Int, Int, String)]): List[(Int, Int, String)] =
      if (pos >= text.length) acc.reverse
      else {
        val nl      = text.indexOf('\n', pos)
        val lineEnd = if (nl < 0) text.length else nl
        val next    = if (nl < 0) text.length else nl + 1
        loop(next, (pos, next, text.substring(pos, lineEnd)) :: acc)
      }
    loop(from, Nil)
  }

  private def isEntryStart(line: String): Boolean = line.trim.startsWith(EntryPrefix)
  private def isBlank(line: String): Boolean      = line.trim.isEmpty

  /** The name is everything before the first `:` that is followed by a space or
    * ends the line, so `skill-creator:skill-creator: Create…` keeps its colon and
    * `- name:` with an empty description still yields `name`.
    */
  private def nameOf(line: String): String = {
    val body = line.trim.drop(EntryPrefix.length)
    val idx  = body.indices.find(i => body(i) === ':' && (i + 1 === body.length || body(i + 1) === ' '))
    idx.fold(body.trim)(i => body.substring(0, i))
  }

  def entries(inner: String): List[SkillEntry] = {
    val markerIdx = inner.indexOf(Marker)
    if (markerIdx < 0) Nil
    else {
      val afterMarker    = inner.indexOf('\n', markerIdx) match {
        case -1 => inner.length
        case i => i + 1
      }
      val (closed, open) = lines(inner, afterMarker).foldLeft((List.empty[SkillEntry], Option.empty[SkillEntry])) {
        case ((done, current), (start, end, line)) =>
          if (isEntryStart(line)) {
            (current.fold(done)(_ :: done), SkillEntry(nameOf(line), start, end).some)
          } else if (isBlank(line)) {
            (current.fold(done)(_ :: done), none[SkillEntry])
          } else {
            (done, current.map(_.copy(end = end)))
          }
      }
      (open.fold(closed)(_ :: closed)).reverse
    }
  }

  /** Removes every entry `shouldRemove` accepts; returns the new inner text and
    * the removed entries (in document order).
    */
  def remove(inner: String, shouldRemove: SkillEntry => Boolean): (String, List[SkillEntry]) = {
    val removed = entries(inner).filter(shouldRemove)
    val out     = removed.sortBy(e => -e.start).foldLeft(inner) { (acc, e) =>
      acc.substring(0, e.start) + acc.substring(e.end)
    }
    (out, removed)
  }
}
