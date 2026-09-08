package claudeproxymate.core

import scala.util.matching.Regex

/** Extracts CLAUDE.md / rule / memory sections from system-reminder text.
  *
  * Ports `parseClaudeMdSections` from `index.html:1197-1221`.
  */
object ClaudeMdParser {

  /* Scala Native's java.util.regex is RE2-based and rejects lookahead, so only the
   * section header is matched here; each body is sliced between consecutive header
   * matches. Equivalent to the old `(.*?)(?=\n\nContents of |\s*$)` capture because
   * the body is trimmed either way. */
  val HeaderPattern: Regex =
    """Contents of (.+?) \((.+?)\):\n\n""".r

  private val globalPattern: Regex     = "(?i)global|private global".r
  private val memoryDescPattern: Regex = "(?i)memory".r
  private val memoryPathPattern: Regex = "/memory/".r
  private val rulesPathPattern: Regex  = "/rules/".r
  private val claudeMdPattern: Regex   = "(?i)CLAUDE\\.md$".r

  /** Display label, badge class and scope for a `Contents of <path> (<desc>)` section.
    * Shared with the request filter so its inventory and the Messages badges agree.
    */
  def label(path: String, desc: String): (String, String, String) = {
    val fname = path.split('/').last

    val isGlobal = globalPattern.findFirstIn(desc).isDefined
    val isMemory = memoryDescPattern.findFirstIn(desc).isDefined || memoryPathPattern.findFirstIn(path).isDefined

    val (lbl, cls) =
      if (isMemory) {
        (s"🧠 Memory: $fname", "green")
      } else if (rulesPathPattern.findFirstIn(path).isDefined) {
        val prefix = if (isGlobal) "📜 Global Rule: " else "📜 Local Rule: "
        (prefix + fname, if (isGlobal) "green" else "cyan")
      } else if (claudeMdPattern.findFirstIn(path).isDefined) {
        if (isGlobal) ("📋 Global CLAUDE.md", "green")
        else ("📋 Local CLAUDE.md", "cyan")
      } else {
        (s"📋 $fname", "green")
      }

    val scope = if (isGlobal) "global" else "local"
    (lbl, cls, scope)
  }

  def parseClaudeMdSections(inner: String): List[Section] = {
    val headers = HeaderPattern.findAllMatchIn(inner).toList
    headers
      .zipWithIndex
      .map {
        case (m, i) =>
          val path    = m.group(1)
          val desc    = m.group(2)
          val bodyEnd = headers.lift(i + 1).fold(inner.length)(_.start)
          val content = inner.substring(m.end, bodyEnd).trim

          val (lbl, cls, scope) = label(path, desc)
          Section(lbl, path, content, cls, scope)
      }
  }
}
