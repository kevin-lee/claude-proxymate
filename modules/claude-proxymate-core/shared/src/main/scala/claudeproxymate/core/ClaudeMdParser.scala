package claudeproxymate.core

import scala.util.matching.Regex

/** Extracts CLAUDE.md / rule / memory sections from system-reminder text.
  *
  * Ports `parseClaudeMdSections` from `index.html:1197-1221`.
  */
object ClaudeMdParser {

  // (?s) enables DOTALL so . matches newlines (equivalent to [\s\S] in JS)
  private val sectionPattern: Regex =
    """(?s)Contents of (.+?) \((.+?)\):\n\n(.*?)(?=\n\nContents of |\s*$)""".r

  private val globalPattern: Regex     = "(?i)global|private global".r
  private val memoryDescPattern: Regex = "(?i)memory".r
  private val memoryPathPattern: Regex = "/memory/".r
  private val rulesPathPattern: Regex  = "/rules/".r
  private val claudeMdPattern: Regex   = "(?i)CLAUDE\\.md$".r

  def parseClaudeMdSections(inner: String): List[Section] = {
    sectionPattern
      .findAllMatchIn(inner)
      .map { m =>
        val path    = m.group(1)
        val desc    = m.group(2)
        val content = m.group(3).trim
        val fname   = path.split('/').last

        val isGlobal = globalPattern.findFirstIn(desc).isDefined
        val isMemory = memoryDescPattern.findFirstIn(desc).isDefined || memoryPathPattern.findFirstIn(path).isDefined

        val (label, cls) =
          if (isMemory) {
            (s"\uD83E\uDDE0 Memory: $fname", "green")
          } else if (rulesPathPattern.findFirstIn(path).isDefined) {
            val prefix = if (isGlobal) "\uD83D\uDCDC Global Rule: " else "\uD83D\uDCDC Local Rule: "
            (prefix + fname, if (isGlobal) "green" else "cyan")
          } else if (claudeMdPattern.findFirstIn(path).isDefined) {
            if (isGlobal) ("\uD83D\uDCCB Global CLAUDE.md", "green")
            else ("\uD83D\uDCCB Local CLAUDE.md", "cyan")
          } else {
            (s"\uD83D\uDCCB $fname", "green")
          }

        val scope = if (isGlobal) "global" else "local"
        Section(label, path, content, cls, scope)
      }
      .toList
  }
}
