package claudeproxymate.renderer.json

import claudeproxymate.core.{CorrelationIds, SensitiveKeys, TokenPatterns}
import claudeproxymate.renderer.state.AppState
import scalatags.Text.all.*

import scala.scalajs.js

/** Pure view for the interactive JSON tree.
  *
  * Builds Scalatags fragments for arbitrary JSON values. Mutates the
  * counters on [[claudeproxymate.renderer.state.AppState]] (`jtId`,
  * `jtLine`, `maskId`) to assign unique element IDs and line numbers;
  * tests reset them before each run.
  *
  * Click dispatch is the orchestrator's responsibility — the view emits
  * `data-jt-id` / `data-jt-str-id` / `data-mask-id` attributes;
  * [[JsonTreeViewer]] reads them in its document-level click handler.
  *
  * Field-name masking (C3): values held under a key matching
  * [[claudeproxymate.core.SensitiveKeys.isSensitive]] are rendered
  * mask-by-default. The orchestrator passes the localized hidden
  * label; the view consults
  * [[claudeproxymate.renderer.state.AppState.maskRevealed]] to decide
  * whether to render the masked placeholder or the original value.
  * Pure: no `I18n` / DOM dependencies.
  */
object JsonTreeView {

  val ContainerToggleClass: String  = "jt-btn"
  val ContainerSummaryClass: String = "jt-tag"
  val ContainerDataAttr: String     = "data-jt-id"

  val StringToggleClass: String  = "jt-str-toggle"
  val StringPreviewClass: String = "jt-str-preview"
  val StringDataAttr: String     = "data-jt-str-id"

  val MaskClass: String          = "jt-mask"
  val MaskRevealedClass: String  = "jt-mask-revealed"
  val MaskDataAttr: String       = "data-mask-id"

  val TokenMaskClass: String         = "jt-token-mask"
  val TokenMaskRevealedClass: String = "jt-token-mask-revealed"
  val TokenMaskDataAttr: String      = "data-token-id"

  /* Correlation-id mask (C3 3): visually distinct from C3 1/2 because
   * these are identifiers, not credentials. Reveal state still uses
   * the shared `AppState.maskRevealed` set; ids are namespaced by
   * the `corr:` prefix to avoid collision with token / field-name ids.
   */
  val CorrMaskClass: String          = "jt-corr-mask"
  val CorrMaskRevealedClass: String  = "jt-corr-mask-revealed"
  val CorrMaskDataAttr: String       = "data-corr-id"

  val CollapseLenThreshold: Int = 300

  /** Convenience overload: trailing = empty frag, depth = 0,
    * maskLabel = `""` (no localization — used by tests).
    */
  def buildJsonFrag(value: js.Dynamic, totalBytes: Int): Frag =
    buildJsonFrag(value, depth = 0, trailing = frag(), totalBytes = totalBytes, maskLabel = "", path = "$")

  /** Convenience overload with a localized mask label. */
  def buildJsonFrag(value: js.Dynamic, totalBytes: Int, maskLabel: String): Frag =
    buildJsonFrag(value, depth = 0, trailing = frag(), totalBytes = totalBytes, maskLabel = maskLabel, path = "$")

  /** Build the Scalatags fragment for a JSON value plus a trailing
    * fragment (typically a comma between siblings).
    *
    * `path` is the stable dot-path identifier used for mask ids
    * (e.g. `$.metadata.user_id`). Stable across re-renders of the
    * same data, so the per-value reveal state in
    * `AppState.maskRevealed` survives a re-render.
    */
  def buildJsonFrag(value: js.Dynamic, depth: Int, trailing: Frag, totalBytes: Int, maskLabel: String, path: String): Frag = {
    if (value == null || js.isUndefined(value)) {
      frag(span(cls := "jt-null")("null"), trailing)
    } else {
      val typeOf = js.typeOf(value)
      if (typeOf == "boolean") {
        frag(span(cls := "jt-bool")(value.toString), trailing)
      } else if (typeOf == "number") {
        frag(span(cls := "jt-num")(value.toString), trailing)
      } else if (typeOf == "string") {
        buildStringFrag(value.asInstanceOf[String], trailing, path)
      } else if (js.Array.isArray(value)) {
        buildContainerFrag(value, isArr = true, depth, trailing, totalBytes, maskLabel, path)
      } else {
        buildContainerFrag(value, isArr = false, depth, trailing, totalBytes, maskLabel, path)
      }
    }
  }

  private def buildStringFrag(s: String, trailing: Frag, path: String): Frag = {
    if (s.length > CollapseLenThreshold) {
      AppState.jtId += 1
      val sid  = s"jts${AppState.jtId}"
      val hits = collectHits(s)

      // Collapsed preview: render the first 80 raw chars with
      // newlines flattened to spaces and tokens masked inline.
      // Tokens that cross the 80-char boundary are skipped from
      // the preview (they show in the expanded view).
      val previewSliceLen = math.min(80, s.length)
      val previewSlice    = s.substring(0, previewSliceLen)
      val previewParts: List[Frag] =
        buildTokenizedSlice(previewSlice, hits, sliceOffset = 0, path, flattenNewlines = true)
          .getOrElse(List(stringFrag(jsonEscapeBody(maybeFlatten(previewSlice, flatten = true)))))

      // Expanded view: split raw `s` on real newlines. Each line
      // becomes a `<div class="jt-exp-line">` with tokenized
      // contents. The legacy `\\n` → `\n` substitution is dropped;
      // raw values containing the two-char `\` + `n` sequence
      // render as `\n` in the line text per JSON.stringify
      // convention (matches clipboard format).
      val lines  = s.split("\n", -1)
      val baseLn = AppState.jtLine

      var sliceAt = 0
      val rowFrags: List[Frag] = lines.indices.toList.map { li =>
        val line       = lines(li)
        val ln         = if (li == 0) baseLn else { AppState.jtLine += 1; AppState.jtLine }
        val isLast     = li == lines.length - 1
        val openQuote  = if (li == 0) "\"" else ""
        val closeQuote = if (isLast) "\"" else ""
        val lineParts: List[Frag] =
          buildTokenizedSlice(line, hits, sliceOffset = sliceAt, path, flattenNewlines = false)
            .getOrElse(List(stringFrag(jsonEscapeBody(line))))
        sliceAt += line.length + 1 // +1 for the consumed `\n`
        div(cls := "jt-exp-line", attr("data-ln") := ln.toString)(
          openQuote,
          frag(lineParts*),
          closeQuote,
        )
      }

      frag(
        span(cls := "jt-str-long")(
          span(
            cls                 := StringToggleClass,
            id                  := s"$sid-btn",
            attr(StringDataAttr) := sid,
          )("▶"),
          span(
            cls                  := StringPreviewClass,
            id                   := s"$sid-s",
            attr(StringDataAttr) := sid,
          )(
            "\"",
            frag(previewParts*),
            "…\" ",
            span(style := "color:var(--dim);font-size:10px")(s"(${s.length} chars)"),
          ),
          div(
            cls   := "jt-str-expanded",
            id    := s"$sid-b",
            style := "display:none",
          )(
            span(
              cls                  := StringToggleClass,
              attr(StringDataAttr) := sid,
            )("▼"),
            frag(rowFrags),
          ),
        ),
        trailing,
      )
    } else {
      /* Short string — scan for token-shape matches AND
       * correlation-id matches (PR 3), then either render plain
       * (when no hits) or interleave mask spans with JSON-escaped
       * surrounding text.
       */
      val hits = collectHits(s)
      if (hits.isEmpty) {
        val display = js.JSON.stringify(s.asInstanceOf[js.Any])
        frag(span(cls := "jt-str")(display), trailing)
      } else {
        val parts = buildTokenizedSlice(s, hits, sliceOffset = 0, path).getOrElse(List(stringFrag(s)))
        frag(span(cls := "jt-str")("\"", frag(parts*), "\""), trailing)
      }
    }
  }

  /** A merged mask candidate (regex-token from PR 2 OR
    * correlation-id from PR 3) used by the unified walk in
    * [[buildTokenizedSlice]]. The two surfaces are disjoint by
    * pattern prefix in practice, so a sort + sequential walk
    * suffices.
    */
  private sealed trait MaskHit { def start: Int; def end: Int }
  private final case class TokenHit(start: Int, end: Int)              extends MaskHit
  private final case class CorrHit (start: Int, end: Int, name: String) extends MaskHit

  private def collectHits(s: String): List[MaskHit] = {
    val tokens = TokenPatterns.scan(s).map(t => TokenHit(t.start, t.end))
    val corrs  = CorrelationIds.scan(s).map(c => CorrHit(c.start, c.end, c.name))
    (tokens ++ corrs).sortBy(_.start)
  }

  /** Tokenize `text` using the pre-collected `hits` list (whose
    * offsets are relative to the raw value the slice came from).
    * `sliceOffset` is the offset of `text[0]` within that raw
    * value, which is also the offset used in mask ids.
    *
    * When `flattenNewlines` is true, real `\n` / `\r` chars in the
    * plain segments are replaced with space before JSON-escape — used
    * by the long-string collapsed preview to keep everything on one
    * visual line.
    *
    * Returns `None` when no hits fall inside the slice (caller can
    * fall back to a single plain frag), or `Some(parts)` with the
    * interleaved sequence of plain-text and `<span>` frags. Plain-
    * text parts go through `jsonEscapeBody` so `"` / `\` /
    * control chars render as JSON literals (matching the unmasked
    * branch).
    */
  private def buildTokenizedSlice(
    text: String,
    hits: List[MaskHit],
    sliceOffset: Int,
    path: String,
    flattenNewlines: Boolean = false,
  ): Option[List[Frag]] = {
    val sliceEnd = sliceOffset + text.length
    val inSlice  = hits.filter(h => h.start >= sliceOffset && h.end <= sliceEnd)
    if (inSlice.isEmpty) return None
    val parts = scala.collection.mutable.ListBuffer.empty[Frag]
    var cursor = sliceOffset
    inSlice.foreach { h =>
      if (h.start > cursor) {
        val plain = text.substring(cursor - sliceOffset, h.start - sliceOffset)
        parts += stringFrag(jsonEscapeBody(maybeFlatten(plain, flattenNewlines)))
      }
      val raw = text.substring(h.start - sliceOffset, h.end - sliceOffset)
      parts += (h match {
        case TokenHit(_, _)        => buildTokenMaskFrag(path, h.start, raw)
        case CorrHit(_, _, name)   => buildCorrMaskFrag(path, h.start, raw, name)
      })
      cursor = h.end
    }
    if (cursor < sliceEnd) {
      val plain = text.substring(cursor - sliceOffset, sliceEnd - sliceOffset)
      parts += stringFrag(jsonEscapeBody(maybeFlatten(plain, flattenNewlines)))
    }
    Some(parts.toList)
  }

  private def maybeFlatten(s: String, flatten: Boolean): String =
    if (flatten) s.replace('\n', ' ').replace('\r', ' ') else s

  /** Build the masked-or-revealed frag for a single regex-detected
    * token. Token id format: `<json-path>#<offset>`, stable across
    * re-renders. Public so [[JsonTreeViewer]] can reuse it for
    * in-place DOM swaps.
    */
  def buildTokenMaskFrag(path: String, offset: Int, raw: String): Frag = {
    val tid = s"$path#$offset"
    if (AppState.maskRevealed.contains(tid)) {
      span(cls := TokenMaskRevealedClass, attr(TokenMaskDataAttr) := tid)(
        jsonEscapeBody(raw),
      )
    } else {
      span(cls := TokenMaskClass, attr(TokenMaskDataAttr) := tid)(
        TokenPatterns.fingerprint(raw),
      )
    }
  }

  /** Build the masked-or-revealed frag for a single correlation id
    * (PR 3). Render shape: `<prefix>_…<last-4>` (e.g. `msg_…XYZ4`).
    * Distinct CSS class from token mask so visual cue signals
    * "informational compaction" rather than "sensitive redaction".
    *
    * Id format: `corr:<json-path>#<offset>`. The `corr:` prefix
    * keeps the namespace disjoint from token-mask ids in the
    * shared `AppState.maskRevealed` set.
    */
  def buildCorrMaskFrag(path: String, offset: Int, raw: String, name: String): Frag = {
    val cid = s"corr:$path#$offset"
    if (AppState.maskRevealed.contains(cid)) {
      span(cls := CorrMaskRevealedClass, attr(CorrMaskDataAttr) := cid)(
        jsonEscapeBody(raw),
      )
    } else {
      span(cls := CorrMaskClass, attr(CorrMaskDataAttr) := cid)(
        CorrelationIds.fingerprint(name, raw),
      )
    }
  }

  /** Build the masked-or-revealed frag for a single sensitive-key
    * value. Public so [[JsonTreeViewer]] can reuse it for in-place
    * DOM swaps on click without doing a full detail re-render.
    *
    * Returns one outer `<span>` with the data-mask-id attribute,
    * containing either the lock placeholder or the recursive value
    * tree. Trailing commas / siblings are the caller's
    * responsibility.
    */
  def buildMaskFrag(
    path: String,
    value: js.Dynamic,
    maskLabel: String,
    depth: Int,
    totalBytes: Int,
  ): Frag = {
    if (AppState.maskRevealed.contains(path)) {
      span(cls := MaskRevealedClass, attr(MaskDataAttr) := path)(
        buildJsonFrag(value, depth + 1, frag(), totalBytes, maskLabel, path),
      )
    } else {
      span(cls := MaskClass, attr(MaskDataAttr) := path)(
        span(cls := "jt-mask-icon")("🔒"),
        span(cls := "jt-mask-label")(if (maskLabel.isEmpty) "hidden" else maskLabel),
      )
    }
  }

  private def buildContainerFrag(
    value: js.Dynamic,
    isArr: Boolean,
    depth: Int,
    trailing: Frag,
    totalBytes: Int,
    maskLabel: String,
    path: String,
  ): Frag = {
    val entries: js.Array[js.Tuple2[String | Null, js.Dynamic]] =
      if (isArr) {
        val arr = value.asInstanceOf[js.Array[js.Dynamic]]
        arr.map { v => js.Tuple2[String | Null, js.Dynamic](null, v) }
      } else {
        val keys = js.Object.keys(value.asInstanceOf[js.Object])
        keys.map { k => js.Tuple2[String | Null, js.Dynamic](k, value.selectDynamic(k)) }
      }

    val open  = if (isArr) "[" else "{"
    val close = if (isArr) "]" else "}"

    if (entries.length == 0) {
      return frag(span(s"$open$close"), trailing)
    }

    AppState.jtId += 1
    val id_   = s"jt${AppState.jtId}"
    val label = entries.length.toString

    val rowFrags: List[Frag] = (0 until entries.length).toList.map { i =>
      AppState.jtLine += 1
      val ln    = AppState.jtLine
      val comma: Frag =
        if (i < entries.length - 1) span(style := "color:var(--dim)")(",")
        else frag()
      val k     = entries(i)._1
      val v     = entries(i)._2
      val keyPart: Frag =
        if (k != null)
          frag(
            span(cls := "jt-key")("\"", k.toString, "\""),
            span(style := "color:var(--dim)")(": "),
          )
        else frag()

      // Per-entry path: `parent.key` for objects, `parent[i]` for arrays.
      val childPath: String =
        if (k != null) s"$path.${k.toString}"
        else s"$path[$i]"

      // Field-name masking branch. When the key is sensitive, render
      // either a masked placeholder or (when revealed) the original
      // value wrapped in a click-to-remask span. Arrays don't have
      // keys (`k == null`), so this branch never fires for array
      // entries. The comma is rendered separately so the masked /
      // revealed span is a single replaceable element (used by
      // `JsonTreeViewer.toggleMaskReveal` for in-place swap).
      val valueFrag: Frag =
        if (k != null && SensitiveKeys.isSensitive(k.toString)) {
          frag(buildMaskFrag(childPath, v, maskLabel, depth, totalBytes), comma)
        } else {
          buildJsonFrag(v, depth + 1, comma, totalBytes, maskLabel, childPath)
        }

      div(
        cls          := "jt-row",
        attr("data-ln") := ln.toString,
        style        := "padding-left:16px",
      )(
        keyPart,
        valueFrag,
      )
    }

    val typeTag = if (isArr) s"[$label]" else s"{$label}"

    val jsonBytes = utf8ByteLength(js.JSON.stringify(value))
    val tokens    = Math.ceil(jsonBytes.toDouble / 3.5).toInt
    val tokStr =
      if (tokens >= 1000000) f"${tokens / 1000000.0}%.1fM"
      else if (tokens >= 1000) f"${tokens / 1000.0}%.1fK"
      else tokens.toString
    val pct    = if (totalBytes > 0) jsonBytes.toDouble / totalBytes * 100 else 0.0
    val pctStr =
      if (pct >= 1) f"$pct%.0f%%"
      else if (pct >= 0.1) f"$pct%.1f%%"
      else "<0.1%"

    AppState.jtLine += 1
    val closeLn = AppState.jtLine

    span(
      span(
        cls                     := ContainerToggleClass,
        id                      := s"${id_}-btn",
        attr(ContainerDataAttr) := id_,
      )("▼"),
      span(style := "color:var(--dim)")(open),
      span(
        cls                     := ContainerSummaryClass,
        id                      := s"${id_}-s",
        attr(ContainerDataAttr) := id_,
        style                   := "display:none",
      )(
        s"$typeTag ",
        span(cls := "jt-tok")(s"~$tokStr tok · $pctStr"),
        trailing,
      ),
      span(id := s"${id_}-b")(
        frag(rowFrags),
        div(cls := "jt-row", attr("data-ln") := closeLn.toString)(
          span(style := "color:var(--dim)")(close),
          trailing,
        ),
      ),
    )
  }

  /** Measure UTF-8 byte length. Pure (no DOM dependency) so the view can
    * compute the `jt-tok` percentage and remain unit-testable on any
    * Scala.js runtime, not just one with `window.TextEncoder`.
    */
  private def utf8ByteLength(s: String): Int =
    s.getBytes(java.nio.charset.StandardCharsets.UTF_8).length

  /** JSON-escape the body of a string (no surrounding `"`s).
    * Delegates to `JSON.stringify` and strips the outer quotes so
    * callers can compose the surrounding context themselves
    * (e.g. add their own `"…"` wrapping or split the body across
    * lines). Produces the same `\"` / `\\` / `\n` / `\uXXXX` escapes
    * the clipboard-copy path emits.
    */
  private def jsonEscapeBody(s: String): String = {
    val stringified = js.JSON.stringify(s.asInstanceOf[js.Any])
    if (stringified.length >= 2) stringified.substring(1, stringified.length - 1)
    else stringified
  }

  /** Walk a JSON value to the location identified by `path`. The path
    * format matches what [[buildJsonFrag]] generates: `$` for the
    * root, `.key` for object members, `[i]` for array indices.
    *
    * Returns `null` if the path doesn't resolve (key missing,
    * out-of-range index, primitive-encountered-where-container-
    * expected). Pure; used by [[JsonTreeViewer.toggleMaskReveal]] to
    * swap a single masked element in-place without a full re-render.
    */
  def resolvePath(root: js.Dynamic, path: String): js.Dynamic = {
    if (path == null || path.isEmpty || path == "$") return root
    if (!path.startsWith("$")) return null.asInstanceOf[js.Dynamic]
    var current: js.Dynamic = root
    var i: Int              = 1 // skip the leading `$`
    while (i < path.length) {
      if (current == null || js.isUndefined(current)) return null.asInstanceOf[js.Dynamic]
      val c = path.charAt(i)
      if (c == '.') {
        // Read key up to next `.` or `[` or end-of-string.
        var j = i + 1
        while (j < path.length && path.charAt(j) != '.' && path.charAt(j) != '[') j += 1
        val key = path.substring(i + 1, j)
        current = current.selectDynamic(key)
        i = j
      } else if (c == '[') {
        // Read digits up to `]`.
        val close = path.indexOf(']', i)
        if (close < 0) return null.asInstanceOf[js.Dynamic]
        val idxStr = path.substring(i + 1, close)
        val idx =
          try idxStr.toInt
          catch { case _: NumberFormatException => return null.asInstanceOf[js.Dynamic] }
        if (!js.Array.isArray(current)) return null.asInstanceOf[js.Dynamic]
        val arr = current.asInstanceOf[js.Array[js.Dynamic]]
        if (idx < 0 || idx >= arr.length) return null.asInstanceOf[js.Dynamic]
        current = arr(idx)
        i = close + 1
      } else {
        return null.asInstanceOf[js.Dynamic]
      }
    }
    current
  }
}
