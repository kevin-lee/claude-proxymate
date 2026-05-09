package claudeproxymate.renderer.messages

import claudeproxymate.core.TokenPatterns
import claudeproxymate.renderer.json.JsonTreeView
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.HtmlUtil
import scalatags.Text.all.*

/** Pure helper that emits a Scalatags `Frag` for message text,
  * composing the regex-shape token mask (PR 2b) with the existing
  * search-highlight pipeline.
  *
  * Composition policy:
  *
  *   1. Token matches (per
  *      [[claudeproxymate.core.TokenPatterns.scan]]) take precedence
  *      and render as `<first-4>…<last-4>` fingerprints inside a
  *      click-to-reveal span. They are NOT search-highlighted —
  *      you can't usefully search content you can't see.
  *   2. Plain (non-token) segments pass through
  *      [[HtmlUtil.highlightSearchFrag]] so search hits inside
  *      surrounding text still wrap in `<mark>`.
  *   3. When a token has been revealed (its id is in
  *      [[AppState.maskRevealed]]), the revealed text is rendered
  *      *inside* the span and DOES go through the search highlighter
  *      so a search hit inside a revealed token still wraps.
  *
  * Token ids follow `<idPrefix>#<offset>` where `idPrefix` is
  * caller-supplied (e.g. `m.0.user.0`) and `offset` is the start
  * position of the match within `text`. Stable across re-renders of
  * the same data.
  *
  * Pure: no DOM access. Click dispatch is the orchestrator's
  * responsibility (`MessageRenderer.handleClick`).
  */
object MessageTokenView {

  /** Build the rendered fragment for a piece of message text.
    * `query` is the active search query (empty when no search is
    * active). `idPrefix` is the stable caller-supplied prefix for
    * token ids.
    */
  def buildTextFrag(text: String, query: String, idPrefix: String): Frag = {
    val tokens = TokenPatterns.scan(text)
    if (tokens.isEmpty) HtmlUtil.highlightSearchFrag(text, query)
    else {
      val parts  = scala.collection.mutable.ListBuffer.empty[Frag]
      var cursor = 0
      tokens.foreach { t =>
        if (t.start > cursor) {
          parts += HtmlUtil.highlightSearchFrag(text.substring(cursor, t.start), query)
        }
        val raw = text.substring(t.start, t.end)
        val tid = s"$idPrefix#${t.start}"
        if (AppState.maskRevealed.contains(tid)) {
          parts += span(
            cls                            := JsonTreeView.TokenMaskRevealedClass,
            attr(JsonTreeView.TokenMaskDataAttr) := tid,
          )(HtmlUtil.highlightSearchFrag(raw, query))
        } else {
          parts += span(
            cls                            := JsonTreeView.TokenMaskClass,
            attr(JsonTreeView.TokenMaskDataAttr) := tid,
          )(TokenPatterns.fingerprint(raw))
        }
        cursor = t.end
      }
      if (cursor < text.length) {
        parts += HtmlUtil.highlightSearchFrag(text.substring(cursor), query)
      }
      frag(parts.toList)
    }
  }
}
