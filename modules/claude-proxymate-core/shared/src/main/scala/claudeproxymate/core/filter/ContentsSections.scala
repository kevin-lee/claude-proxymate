package claudeproxymate.core.filter

import claudeproxymate.core.ClaudeMdParser

import scala.util.matching.Regex

enum SectionCategory {
  case Rules
  case Docs
}

object SectionCategory {
  given cats.Eq[SectionCategory] = cats.Eq.fromUniversalEquals
}

/** A `Contents of <path> (<desc>):` section located inside a system-reminder,
  * as the half-open range `[start, end)` of the reminder's inner text.
  */
final case class LocatedSection(
  path: String,
  desc: String,
  start: Int,
  end: Int,
  category: SectionCategory,
  label: String,
)

/** Locates and removes `Contents of …` sections with bounds precise enough for
  * deletion. [[ClaudeMdParser]] lets the last section run to the end of the
  * reminder, which is fine for display but would delete the reminder's own
  * trailer (`# userEmail`, `# currentDate`, the closing IMPORTANT line).
  */
object ContentsSections {

  /** Lines that end the last section: the reminder's own keyed sections and
    * its closing notice. Anchored on the newline that precedes the line rather
    * than with `(?m)^…$`: the Scala.js regex engine rejects that form and
    * RE2 is happier without it too. A match starts at that newline, so the
    * trailer (newline included) survives a removal intact.
    */
  val TrailerPattern: Regex =
    """\n(?:# (?:userEmail|currentDate|gitStatus)(?:\n|$)|[ \t]*IMPORTANT: this context may or may not be relevant)""".r

  private val ContentsMarker: String = "Contents of "

  /** Same test as `MechanismDetector.contentsOfPattern`. */
  def isContentsReminder(inner: String): Boolean = inner.contains(ContentsMarker)

  def categoryOf(path: String): SectionCategory =
    if (path.contains("/rules/")) SectionCategory.Rules else SectionCategory.Docs

  def locate(inner: String): List[LocatedSection] = {
    val headers  = ClaudeMdParser.HeaderPattern.findAllMatchIn(inner).toList
    val trailers = TrailerPattern.findAllMatchIn(inner).map(_.start).toList
    headers.zipWithIndex.map {
      case (m, i) =>
        val path        = m.group(1)
        val desc        = m.group(2)
        val nextHeader  = headers.lift(i + 1).map(_.start)
        val nextTrailer = trailers.find(_ > m.start)
        val end         = List(nextHeader, nextTrailer).flatten.minOption.getOrElse(inner.length)
        val (lbl, _, _) = ClaudeMdParser.label(path, desc)
        LocatedSection(path, desc, m.start, end, categoryOf(path), lbl)
    }
  }

  /** Removes every section `shouldRemove` accepts; returns the new inner text and
    * the removed sections (in document order).
    */
  def remove(inner: String, shouldRemove: LocatedSection => Boolean): (String, List[LocatedSection]) = {
    val removed = locate(inner).filter(shouldRemove)
    val out     = removed.sortBy(s => -s.start).foldLeft(inner) { (acc, s) =>
      acc.substring(0, s.start) + acc.substring(s.end)
    }
    (out, removed)
  }
}
