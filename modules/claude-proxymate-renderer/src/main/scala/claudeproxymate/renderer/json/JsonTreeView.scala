package claudeproxymate.renderer.json

import claudeproxymate.core.{SensitiveKeys, TokenPatterns}
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
      val sid     = s"jts${AppState.jtId}"
      // Preview line: flatten newlines (both real and the literal
      // `\n` two-char sequence that some captured payloads carry),
      // JSON-escape `"` / `\` / control chars, truncate to 80 chars
      // + ellipsis.
      val flat    = s.replace("\\n", " ").replace('\n', ' ').replace('\r', ' ')
      val preview = jsonEscapeBody(flat.take(80)) + "…"
      // Expanded view: split on real newlines AND on the literal
      // `\n` two-char sequence (legacy: some downstream payloads
      // store newlines as literal backslash-n rather than as a
      // real newline char). Each line is then JSON-escaped so `"`
      // / `\` / tab show as their JSON literals (matches clipboard
      // format).
      //
      // Token-mask spans are interleaved per-line, intersecting the
      // global token list (offsets in raw `s`) against the line's
      // post-replace slice. The replace step changes offsets, so we
      // skip token rendering inside expanded lines for now and
      // surface tokens only in the collapsed preview. (Long-string
      // token rendering inside the expanded view: deferred — the
      // current users see this in the preview, and the hole in
      // coverage is consistent with the existing `\\n` → `\n`
      // legacy treatment.)
      val lines  = s.replace("\\n", "\n").split("\n", -1)
      val baseLn = AppState.jtLine

      val rowFrags: List[Frag] = lines.indices.toList.map { li =>
        val ln = if (li == 0) baseLn else { AppState.jtLine += 1; AppState.jtLine }
        val isLast     = li == lines.length - 1
        val openQuote  = if (li == 0) "\"" else ""
        val closeQuote = if (isLast) "\"" else ""
        val escaped    = jsonEscapeBody(lines(li))
        div(cls := "jt-exp-line", attr("data-ln") := ln.toString)(
          s"$openQuote$escaped$closeQuote",
        )
      }

      /* Long-string token rendering inside preview / expanded
       * lines: deferred. The `\\n` → `\n` replace shifts character
       * offsets, making it non-trivial to project token offsets
       * (which are relative to the raw value) onto the post-replace
       * slices. PR 2 only handles short strings inline; long-string
       * token coverage follows in a later PR.
       */
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
            preview,
            "\" ",
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
      /* Short string — scan for token-shape matches and either
       * render plain (when no matches) or interleave mask spans
       * with JSON-escaped surrounding text.
       */
      val tokens = TokenPatterns.scan(s)
      if (tokens.isEmpty) {
        val display = js.JSON.stringify(s.asInstanceOf[js.Any])
        frag(span(cls := "jt-str")(display), trailing)
      } else {
        val parts = buildTokenizedSlice(s, tokens, sliceOffset = 0, path).getOrElse(List(stringFrag(s)))
        frag(span(cls := "jt-str")("\"", frag(parts*), "\""), trailing)
      }
    }
  }

  /** Tokenize `text` using the pre-scanned `tokens` list (whose
    * offsets are relative to the raw value the slice came from).
    * `sliceOffset` is the offset of `text[0]` within that raw value,
    * which is also the offset used in token ids.
    *
    * Returns `None` when no token matches fall inside the slice
    * (caller can fall back to a single plain frag), or `Some(parts)`
    * with the interleaved sequence of plain-text and `<span>`
    * frags. Plain-text parts go through `jsonEscapeBody` so `"` /
    * `\` / control chars render as JSON literals (matching the
    * unmasked branch).
    */
  private def buildTokenizedSlice(
    text: String,
    tokens: List[TokenPatterns.TokenMatch],
    sliceOffset: Int,
    path: String,
  ): Option[List[Frag]] = {
    val sliceEnd = sliceOffset + text.length
    val inSlice  = tokens.filter(t => t.start >= sliceOffset && t.end <= sliceEnd)
    if (inSlice.isEmpty) return None
    val parts = scala.collection.mutable.ListBuffer.empty[Frag]
    var cursor = sliceOffset
    inSlice.foreach { t =>
      if (t.start > cursor) {
        val plain = text.substring(cursor - sliceOffset, t.start - sliceOffset)
        parts += stringFrag(jsonEscapeBody(plain))
      }
      val raw = text.substring(t.start - sliceOffset, t.end - sliceOffset)
      parts += buildTokenMaskFrag(path, t.start, raw)
      cursor = t.end
    }
    if (cursor < sliceEnd) {
      val plain = text.substring(cursor - sliceOffset, sliceEnd - sliceOffset)
      parts += stringFrag(jsonEscapeBody(plain))
    }
    Some(parts.toList)
  }

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
