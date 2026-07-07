package claudeproxymate.renderer.copy

import cats.syntax.all.*
import claudeproxymate.core.{CorrelationIds, SensitiveKeys, TokenPatterns}

import scala.scalajs.js

/** Result of a WYSIWYG body walk: the masked copy plus span counts.
  * `total` is the number of maskable spans encountered (sensitive-key
  * values + regex tokens + correlation ids); `revealed` is how many of
  * them the `isRevealed` predicate let through verbatim.
  */
final case class MaskedResult(body: js.Dynamic, revealed: Int, total: Int)

/** Result of a WYSIWYG string scan: the masked text plus span counts. */
final case class MaskedText(text: String, revealed: Int, total: Int)

/** Pure helper that walks a JSON-shaped `js.Dynamic` and produces a
  * WYSIWYG masked copy driven by an `isRevealed` predicate (in
  * production `AppState.isRevealed`, i.e. the effective on-screen
  * per-span state — presenter baseline XOR per-span overrides):
  *
  *   - Object entries whose **key** is sensitive (per
  *     [[claudeproxymate.core.SensitiveKeys.isSensitive]]) are one
  *     span each, id = JSON dot-path (e.g. `$.config.api_key`).
  *     Masked → value replaced wholesale with [[Sentinel]]; revealed
  *     → the walker recurses into the value (interior strings still
  *     get their own spans), mirroring `JsonTreeView.buildMaskFrag`.
  *   - String values get **regex-detected token substrings** (per
  *     [[claudeproxymate.core.TokenPatterns.scan]], span id
  *     `<path>#<offset>`) and **correlation ids** (per
  *     [[claudeproxymate.core.CorrelationIds.scan]], span id
  *     `corr:<path>#<offset>`) replaced with [[Sentinel]] when
  *     masked, emitted verbatim when revealed.
  *
  * Span ids mirror `JsonTreeView` exactly (root `$`, object child
  * `path.key`, array element `path[i]`) so the clipboard follows the
  * screen span-for-span. Masked spans always emit [[Sentinel]] — not
  * the on-screen fingerprint — by design: WYSIWYG in *state*, not
  * glyphs; the copy artifact carries zero partial disclosure.
  *
  * Display-only — never mutates the input. Returns a fresh
  * `js.Dynamic` (or the input value verbatim for primitives without
  * detected spans).
  */
object MaskedCopy {

  /** Sentinel that replaces masked spans in the copied output. */
  val Sentinel: String = "***"

  sealed private trait Hit { def start: Int; def end: Int }
  final private case class TokHit(start: Int, end: Int) extends Hit
  final private case class CorrHit(start: Int, end: Int) extends Hit

  /* Mirrors JsonTreeView.collectHits / MessageTokenView.collectHits:
   * token and correlation matches merged and ordered by start offset. */
  private def collectHits(s: String): List[Hit] = {
    val tokens = TokenPatterns.scan(s).map(t => TokHit(t.start, t.end))
    val corrs  = CorrelationIds.scan(s).map(c => CorrHit(c.start, c.end))
    (tokens ++ corrs).sortBy(_.start)
  }

  /** WYSIWYG scan of a single string. Span ids are
    * `<idPrefix>#<offset>` for tokens and `corr:<idPrefix>#<offset>`
    * for correlation ids — the same derivation as the renderer, so
    * `idPrefix` is a JSON dot-path here and an `m.…` prefix when
    * called from `MessageCopy`. Revealed spans emit the raw
    * substring; masked spans emit [[Sentinel]].
    */
  def maskString(s: String, idPrefix: String, isRevealed: String => Boolean): MaskedText = {
    val hits = collectHits(s)
    if (hits.isEmpty) MaskedText(s, 0, 0)
    else {
      val sb       = new StringBuilder
      var cursor   = 0
      var revealed = 0
      hits.foreach { h =>
        if (h.start > cursor) sb.append(s.substring(cursor, h.start))
        val id = h match {
          case TokHit(_, _) => s"$idPrefix#${h.start}"
          case CorrHit(_, _) => s"corr:$idPrefix#${h.start}"
        }
        if (isRevealed(id)) {
          sb.append(s.substring(h.start, h.end))
          revealed += 1
        } else {
          sb.append(Sentinel)
        }
        cursor = h.end
      }
      if (cursor < s.length) sb.append(s.substring(cursor))
      MaskedText(sb.toString, revealed, hits.length)
    }
  }

  /** Recursively walk a JSON-shaped value from root path `$`,
    * producing the WYSIWYG masked copy and span counts. A masked
    * sensitive-key value counts as exactly one span and its interior
    * is neither walked nor queried (matching the screen, which shows
    * a single lock placeholder).
    */
  def maskBody(value: js.Dynamic, isRevealed: String => Boolean): MaskedResult = {
    var revealed = 0
    var total    = 0

    def walk(v: js.Dynamic, path: String): js.Dynamic = {
      if (v == null || js.isUndefined(v)) v
      else if (js.Array.isArray(v)) {
        val arr    = v.asInstanceOf[js.Array[js.Dynamic]]
        val out    = new js.Array[js.Dynamic](arr.length)
        var i: Int = 0
        while (i < arr.length) {
          out(i) = walk(arr(i), s"$path[$i]")
          i += 1
        }
        out.asInstanceOf[js.Dynamic]
      } else if (js.typeOf(v) === "object") {
        val keys   = js.Object.keys(v.asInstanceOf[js.Object])
        val out    = js.Dictionary.empty[js.Any]
        var i: Int = 0
        while (i < keys.length) {
          val k         = keys(i)
          val child     = v.selectDynamic(k)
          val childPath = s"$path.$k"
          if (SensitiveKeys.isSensitive(k)) {
            total += 1
            if (isRevealed(childPath)) {
              revealed += 1
              out(k) = walk(child, childPath).asInstanceOf[js.Any]
            } else {
              out(k) = Sentinel.asInstanceOf[js.Any]
            }
          } else {
            out(k) = walk(child, childPath).asInstanceOf[js.Any]
          }
          i += 1
        }
        out.asInstanceOf[js.Dynamic]
      } else if (js.typeOf(v) === "string") {
        val r = maskString(v.asInstanceOf[String], path, isRevealed)
        revealed += r.revealed
        total += r.total
        if (r.total === 0) v else r.text.asInstanceOf[js.Dynamic]
      } else {
        /* number / boolean — no spans, no recursion */
        v
      }
    }

    val body = walk(value, "$")
    MaskedResult(body, revealed, total)
  }
}
