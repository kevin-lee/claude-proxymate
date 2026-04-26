package claudeproxymate.renderer.json

import claudeproxymate.renderer.state.AppState
import scalatags.Text.all.*

import scala.scalajs.js

/** Pure view for the interactive JSON tree.
  *
  * Builds Scalatags fragments for arbitrary JSON values. Mutates the
  * counters on [[claudeproxymate.renderer.state.AppState]] (`jtId`,
  * `jtLine`) to assign unique element IDs and line numbers; tests reset
  * them before each run.
  *
  * Click dispatch is the orchestrator's responsibility — the view emits
  * `data-jt-id` / `data-jt-str-id` attributes; [[JsonTreeViewer]] reads
  * them in its document-level click handler.
  */
object JsonTreeView {

  val ContainerToggleClass: String  = "jt-btn"
  val ContainerSummaryClass: String = "jt-tag"
  val ContainerDataAttr: String     = "data-jt-id"

  val StringToggleClass: String  = "jt-str-toggle"
  val StringPreviewClass: String = "jt-str-preview"
  val StringDataAttr: String     = "data-jt-str-id"

  val CollapseLenThreshold: Int = 300

  /** Convenience overload: trailing = empty frag, depth = 0. */
  def buildJsonFrag(value: js.Dynamic, totalBytes: Int): Frag =
    buildJsonFrag(value, depth = 0, trailing = frag(), totalBytes = totalBytes)

  /** Build the Scalatags fragment for a JSON value plus a trailing
    * fragment (typically a comma between siblings).
    */
  def buildJsonFrag(value: js.Dynamic, depth: Int, trailing: Frag, totalBytes: Int): Frag = {
    if (value == null || js.isUndefined(value)) {
      frag(span(cls := "jt-null")("null"), trailing)
    } else {
      val typeOf = js.typeOf(value)
      if (typeOf == "boolean") {
        frag(span(cls := "jt-bool")(value.toString), trailing)
      } else if (typeOf == "number") {
        frag(span(cls := "jt-num")(value.toString), trailing)
      } else if (typeOf == "string") {
        buildStringFrag(value.asInstanceOf[String], trailing)
      } else if (js.Array.isArray(value)) {
        buildContainerFrag(value, isArr = true, depth, trailing, totalBytes)
      } else {
        buildContainerFrag(value, isArr = false, depth, trailing, totalBytes)
      }
    }
  }

  private def buildStringFrag(s: String, trailing: Frag): Frag = {
    if (s.length > CollapseLenThreshold) {
      AppState.jtId += 1
      val sid     = s"jts${AppState.jtId}"
      val preview = s.take(80).replace("\\n", " ").replace("\n", " ") + "…"
      val expanded = s.replace("\\n", "\n")
      val lines   = expanded.split("\n", -1)
      val baseLn  = AppState.jtLine

      val rowFrags: List[Frag] = lines.indices.toList.map { li =>
        val ln = if (li == 0) baseLn else { AppState.jtLine += 1; AppState.jtLine }
        val isLast     = li == lines.length - 1
        val openQuote  = if (li == 0) "\"" else ""
        val closeQuote = if (isLast) "\"" else ""
        div(cls := "jt-exp-line", attr("data-ln") := ln.toString)(
          s"$openQuote${lines(li)}$closeQuote",
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
      frag(span(cls := "jt-str")("\"", s, "\""), trailing)
    }
  }

  private def buildContainerFrag(
    value: js.Dynamic,
    isArr: Boolean,
    depth: Int,
    trailing: Frag,
    totalBytes: Int,
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
      div(
        cls          := "jt-row",
        attr("data-ln") := ln.toString,
        style        := "padding-left:16px",
      )(
        keyPart,
        buildJsonFrag(v, depth + 1, comma, totalBytes),
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
}
