package claudeproxymate.renderer.util

import scalatags.Text.all.*
import scalatags.Text.tags2 as st2

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

  /** Tokenise `text` into matched / unmatched runs for `query` and emit a
    * Scalatags `Frag` with each match wrapped in `<mark class="search-hl">`.
    * Operates on raw text (not escaped HTML) — Scalatags handles escaping
    * for both the surrounding text and the wrapped match.
    */
  def highlightSearchFrag(text: String, query: String): Frag = {
    if (query.isEmpty) stringFrag(text)
    else {
      val pattern = java.util.regex.Pattern.compile(
        escapeRegex(query),
        java.util.regex.Pattern.CASE_INSENSITIVE,
      )
      val matcher = pattern.matcher(text)
      val parts   = scala.collection.mutable.ListBuffer.empty[Frag]
      var last    = 0
      while (matcher.find()) {
        val s = matcher.start
        val e = matcher.end
        if (s > last) parts += stringFrag(text.substring(last, s))
        parts += st2.mark(cls := "search-hl")(text.substring(s, e))
        last = e
      }
      if (last < text.length) parts += stringFrag(text.substring(last))
      frag(parts.toList)
    }
  }
}
