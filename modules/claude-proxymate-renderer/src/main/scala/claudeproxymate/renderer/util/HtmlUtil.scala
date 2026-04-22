package claudeproxymate.renderer.util

import scala.scalajs.js.annotation.JSExportTopLevel

/** HTML escaping and search-highlight utilities.
  *
  * Escaping delegates to `scalatags.Escaping.escape`, which handles
  * `& < > "` plus control-character stripping (preserving `\t\n\r`).
  * We additionally post-replace `'` → `&#39;` because Scalatags does
  * NOT escape single quotes by default, and we want output that is
  * safe in single-quoted attribute contexts too.
  */
object HtmlUtil {

  /** Escape `s` for safe inclusion in HTML text content or in any quoted
    * attribute value (single- or double-quoted).
    *
    * Handled: `& < > " '` → entities; control chars (< space, except
    * `\t\n\r`) → stripped.
    */
  @JSExportTopLevel("esc")
  def esc(s: String): String = {
    val sw = new java.io.StringWriter
    scalatags.Escaping.escape(s, sw)
    sw.toString.replace("'", "&#39;")
  }

  /** Alias for [[esc]] — retained for backward compatibility with code that
    * distinguished attribute escaping. The underlying implementation now
    * produces output safe in any HTML context, so [[esc]] and [[escAttr]]
    * are equivalent.
    */
  @JSExportTopLevel("escAttr")
  def escAttr(s: String): String = esc(s)

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
      re.replaceAllIn(
        escapedHtml,
        m => java.util.regex.Matcher.quoteReplacement(s"""<mark class="search-hl">${m.matched}</mark>"""),
      )
    }
  }
}
