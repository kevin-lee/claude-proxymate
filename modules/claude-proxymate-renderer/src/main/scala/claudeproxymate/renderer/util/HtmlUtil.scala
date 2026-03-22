package claudeproxymate.renderer.util

import scala.scalajs.js.annotation.JSExportTopLevel

/** HTML escaping and search-highlight utilities.
  *
  * Ports `esc()`, `escAttr()`, and `highlightSearch()` from renderer.js.
  */
object HtmlUtil {

  @JSExportTopLevel("esc")
  def esc(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")

  @JSExportTopLevel("escAttr")
  def escAttr(s: String): String =
    esc(s).replace("\"", "&quot;")

  /** Escape regex meta-characters in user query for safe use in a regex. */
  private def escapeRegex(s: String): String = {
    val sb = new StringBuilder
    s.foreach { c =>
      if (".*+?^${}()|[]\\".indexOf(c) >= 0) sb.append('\\')
      sb.append(c)
    }
    sb.toString()
  }

  def highlightSearch(escapedHtml: String, query: String): String = {
    if (query.isEmpty) escapedHtml
    else {
      val escaped = escapeRegex(query)
      val re      = s"(?i)($escaped)".r
      re.replaceAllIn(escapedHtml, m => java.util.regex.Matcher.quoteReplacement(s"""<mark class="search-hl">${m.matched}</mark>"""))
    }
  }
}
