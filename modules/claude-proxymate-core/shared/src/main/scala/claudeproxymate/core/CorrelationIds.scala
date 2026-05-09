package claudeproxymate.core

import scala.util.matching.Regex

/** Anthropic correlation-id detector. Used by the renderer to
  * compact long identifiers like `msg_01ABCDEF...` into a short
  * `msg_…<last-4>` form for readability while preserving the
  * pattern prefix so users can recognize the id type at a glance.
  *
  * Distinct from [[TokenPatterns]]: correlation IDs are NOT
  * secrets, just verbose identifiers. The mask is informational
  * compaction, not redaction. Copy paths emit the raw id verbatim
  * so cross-capture correlation stays usable in pasted output.
  */
object CorrelationIds {

  final case class Pattern(name: String, regex: Regex)

  /** A matched correlation id within a source string. `start` and
    * `end` are substring offsets; `name` is the pattern prefix
    * (`"msg"`, `"toolu"`, `"srvtoolu"`).
    */
  final case class CorrMatch(start: Int, end: Int, name: String) {
    def length: Int = end - start
  }

  /** Pattern table. Order matters when prefixes overlap: longer
    * prefixes are listed first so `srvtoolu_…` is preferred over
    * `toolu_…` when scanning.
    */
  val Patterns: List[Pattern] = List(
    Pattern("srvtoolu", raw"srvtoolu_[a-zA-Z0-9_]{16,}".r),
    Pattern("toolu",    raw"toolu_[a-zA-Z0-9_]{16,}".r),
    Pattern("msg",      raw"msg_[a-zA-Z0-9_]{16,}".r),
  )

  /** Scan `s` for correlation-id matches, returning a non-overlapping
    * list ordered by start offset. Longer / more specific patterns
    * win on overlap.
    */
  def scan(s: String): List[CorrMatch] = {
    if (s == null || s.isEmpty) return Nil
    val candidates = scala.collection.mutable.ListBuffer.empty[CorrMatch]
    Patterns.foreach { p =>
      p.regex.findAllMatchIn(s).foreach { m =>
        candidates += CorrMatch(m.start, m.end, p.name)
      }
    }
    if (candidates.isEmpty) return Nil
    val sorted = candidates.toList.sortBy(m => (m.start, -m.length))
    val out = scala.collection.mutable.ListBuffer.empty[CorrMatch]
    var lastEnd: Int = 0
    sorted.foreach { m =>
      if (m.start >= lastEnd) {
        out += m
        lastEnd = m.end
      }
    }
    out.toList
  }

  /** Render the compact form for a matched id. Format:
    * `<prefix>_…<last-4>` (e.g. `msg_…WXYZ`). Falls back to the raw
    * value when it's already too short for compaction to save chars.
    *
    * `name` is the pattern name (= the prefix without the trailing
    * `_`); `raw` is the matched substring (including the prefix).
    */
  def fingerprint(name: String, raw: String): String = {
    if (raw == null) return ""
    // Compacted form length: name.length + 1 (`_`) + 1 (`…`) + 4 = name.length + 6.
    // If raw is shorter than or equal to this, no point compressing.
    val compactLen = name.length + 6
    if (raw.length <= compactLen) raw
    else s"${name}_…${raw.takeRight(4)}"
  }
}
