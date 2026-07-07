package claudeproxymate.renderer.messages

import claudeproxymate.core.{CorrelationIds, TokenPatterns}
import claudeproxymate.renderer.json.JsonTreeView
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.HtmlUtil
import scalatags.Text.all.*

/** Pure helper that emits a Scalatags `Frag` for message text,
  * composing both the regex-shape credential mask (PR 2b) and the
  * correlation-id mask (PR 3) with the existing search-highlight
  * pipeline.
  *
  * Composition policy:
  *
  *   1. Mask hits (TokenPatterns + CorrelationIds, merged and
  *      sorted by start) take precedence. Token masks render as
  *      `<first-4>…<last-4>` fingerprints; correlation-id masks
  *      render as `<prefix>_…<last-4>`. Both are click-to-reveal
  *      spans and NOT search-highlighted (you can't usefully
  *      search content you can't see).
  *   2. Plain (non-mask) segments pass through
  *      [[HtmlUtil.highlightSearchFrag]] so search hits inside
  *      surrounding text still wrap in `<mark>`.
  *   3. When a mask is revealed (per
  *      [[AppState.isRevealed]]), the revealed text is rendered
  *      inside the span and DOES go through the search highlighter
  *      so a search hit inside a revealed mask still wraps.
  *
  * Token ids follow `<idPrefix>#<offset>`; correlation-id ids
  * follow `corr:<idPrefix>#<offset>`. Stable across re-renders.
  *
  * Pure: no DOM access. Click dispatch is the orchestrator's
  * responsibility (`MessageRenderer.handleClick`).
  */
object MessageTokenView {

  sealed private trait Hit { def start: Int; def end: Int }
  final private case class TokHit(start: Int, end: Int) extends Hit
  final private case class CorrHit(start: Int, end: Int, name: String) extends Hit

  private def collectHits(text: String): List[Hit] = {
    val tokens = TokenPatterns.scan(text).map(t => TokHit(t.start, t.end))
    val corrs  = CorrelationIds.scan(text).map(c => CorrHit(c.start, c.end, c.name))
    (tokens ++ corrs).sortBy(_.start)
  }

  /** Build the rendered fragment for a piece of message text. */
  def buildTextFrag(text: String, query: String, idPrefix: String): Frag = {
    val hits = collectHits(text)
    if (hits.isEmpty) HtmlUtil.highlightSearchFrag(text, query)
    else {
      val parts  = scala.collection.mutable.ListBuffer.empty[Frag]
      var cursor = 0
      hits.foreach { h =>
        if (h.start > cursor) {
          parts += HtmlUtil.highlightSearchFrag(text.substring(cursor, h.start), query)
        }
        val raw = text.substring(h.start, h.end)
        h match {
          case TokHit(_, _) =>
            val tid = s"$idPrefix#${h.start}"
            if (AppState.isRevealed(tid)) {
              parts += span(
                cls := JsonTreeView.TokenMaskRevealedClass,
                attr(JsonTreeView.TokenMaskDataAttr) := tid,
              )(HtmlUtil.highlightSearchFrag(raw, query))
            } else {
              parts += span(
                cls := JsonTreeView.TokenMaskClass,
                attr(JsonTreeView.TokenMaskDataAttr) := tid,
              )(TokenPatterns.fingerprint(raw))
            }
          case CorrHit(_, _, name) =>
            val cid = s"corr:$idPrefix#${h.start}"
            if (AppState.isRevealed(cid)) {
              parts += span(
                cls := JsonTreeView.CorrMaskRevealedClass,
                attr(JsonTreeView.CorrMaskDataAttr) := cid,
              )(HtmlUtil.highlightSearchFrag(raw, query))
            } else {
              parts += span(
                cls := JsonTreeView.CorrMaskClass,
                attr(JsonTreeView.CorrMaskDataAttr) := cid,
              )(CorrelationIds.fingerprint(name, raw))
            }
        }
        cursor = h.end
      }
      if (cursor < text.length) {
        parts += HtmlUtil.highlightSearchFrag(text.substring(cursor), query)
      }
      frag(parts.toList)
    }
  }
}
