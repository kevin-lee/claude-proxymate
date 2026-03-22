package claudeproxymate.renderer.json

import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.HtmlUtil.esc
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Interactive JSON tree viewer.
  *
  * Ports `buildJsonHtml`, `jtToggle`, `jtStrToggle`, `renderJsonTree`,
  * and `splitLongExpLines` from renderer.js.
  */
object JsonTreeViewer {

  private val CollapseLenThreshold = 300

  /** Measure byte length of a string using TextEncoder (Web API). */
  private def utf8ByteLength(s: String): Int = {
    val encoder = js.Dynamic.newInstance(dom.window.asInstanceOf[js.Dynamic].TextEncoder)()
    encoder.encode(s).length.asInstanceOf[Int]
  }

  /** Build HTML string for a JSON value. Uses `AppState.jtId` and `AppState.jtLine` as mutable counters. */
  def buildJsonHtml(value: js.Dynamic, depth: Int = 0, trailing: String = "", totalBytes: Int = 0): String = {
    if (value == null || js.isUndefined(value)) {
      s"""<span class="jt-null">null</span>$trailing"""
    } else {
      val typeOf = js.typeOf(value)
      if (typeOf == "boolean") {
        s"""<span class="jt-bool">$value</span>$trailing"""
      } else if (typeOf == "number") {
        s"""<span class="jt-num">$value</span>$trailing"""
      } else if (typeOf == "string") {
        buildStringHtml(value.asInstanceOf[String], trailing)
      } else if (js.Array.isArray(value)) {
        buildContainerHtml(value, isArr = true, depth, trailing, totalBytes)
      } else {
        // object
        buildContainerHtml(value, isArr = false, depth, trailing, totalBytes)
      }
    }
  }

  private def buildStringHtml(s: String, trailing: String): String = {
    if (s.length > CollapseLenThreshold) {
      AppState.jtId += 1
      val sid     = s"jts${AppState.jtId}"
      val preview = esc(s.take(80)).replace("\\n", " ").replace("\n", " ") + "\u2026"
      val expanded = esc(s).replace("\\n", "\n")
      val lines   = expanded.split("\n")
      val baseLn  = AppState.jtLine
      val sb      = new StringBuilder
      for (li <- lines.indices) {
        val ln = if (li == 0) baseLn else { AppState.jtLine += 1; AppState.jtLine }
        val isLast     = li == lines.length - 1
        val openQuote  = if (li == 0) "\"" else ""
        val closeQuote = if (isLast) "\"" else ""
        sb.append(s"""<div class="jt-exp-line" data-ln="$ln">$openQuote${lines(li)}$closeQuote</div>""")
      }
      val expandedRows = sb.toString()

      s"""<span class="jt-str-long">""" +
        s"""<span class="jt-str-toggle" id="$sid-btn" onclick="jtStrToggle('$sid')">\u25b6</span>""" +
        s"""<span class="jt-str-preview" id="$sid-s" onclick="jtStrToggle('$sid')">"$preview" <span style="color:var(--dim);font-size:10px">(${s.length} chars)</span></span>""" +
        s"""<div class="jt-str-expanded" id="$sid-b" style="display:none">""" +
        s"""<span class="jt-str-toggle" onclick="jtStrToggle('$sid')">\u25bc</span>""" +
        expandedRows +
        "</div>" +
        "</span>" + trailing
    } else {
      s"""<span class="jt-str">"${esc(s)}"</span>$trailing"""
    }
  }

  private def buildContainerHtml(
    value: js.Dynamic,
    isArr: Boolean,
    depth: Int,
    trailing: String,
    totalBytes: Int,
  ): String = {
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
      return s"<span>$open$close</span>$trailing"
    }

    AppState.jtId += 1
    val id    = s"jt${AppState.jtId}"
    val label = entries.length.toString

    val rowsSb = new StringBuilder
    for (i <- 0 until entries.length) {
      AppState.jtLine += 1
      val ln    = AppState.jtLine
      val comma = if (i < entries.length - 1) """<span style="color:var(--dim)">,</span>""" else ""
      val k     = entries(i)._1
      val v     = entries(i)._2
      val keyPart =
        if (k != null) s"""<span class="jt-key">"${esc(k.toString)}"</span><span style="color:var(--dim)">: </span>"""
        else ""
      rowsSb.append(s"""<div class="jt-row" data-ln="$ln" style="padding-left:16px">$keyPart${buildJsonHtml(v, depth + 1, comma, totalBytes)}</div>""")
    }
    val rows    = rowsSb.toString()
    val typeTag = if (isArr) s"[$label]" else s"{$label}"

    // Token weight display for collapsed nodes
    val jsonBytes = utf8ByteLength(js.JSON.stringify(value))
    val tokens    = Math.ceil(jsonBytes.toDouble / 3.5).toInt
    val tokStr =
      if (tokens >= 1000000) f"${tokens / 1000000.0}%.1fM"
      else if (tokens >= 1000) f"${tokens / 1000.0}%.1fK"
      else tokens.toString
    val pct    = if (totalBytes > 0) jsonBytes.toDouble / totalBytes * 100 else 0.0
    val pctStr = if (pct >= 1) f"$pct%.0f%%" else if (pct >= 0.1) f"$pct%.1f%%" else "<0.1%"
    val tokTag = s"""<span class="jt-tok">~$tokStr tok \u00b7 $pctStr</span>"""

    AppState.jtLine += 1
    val closeLn = AppState.jtLine

    "<span>" +
      s"""<span class="jt-btn" id="$id-btn" onclick="jtToggle('$id')">\u25bc</span>""" +
      s"""<span style="color:var(--dim)">$open</span>""" +
      s"""<span class="jt-tag" id="$id-s" onclick="jtToggle('$id')" style="display:none">$typeTag $tokTag$trailing</span>""" +
      s"""<span id="$id-b">""" +
      rows +
      s"""<div class="jt-row" data-ln="$closeLn"><span style="color:var(--dim)">$close</span>$trailing</div>""" +
      "</span>" +
      "</span>"
  }

  /** Toggle collapsed/expanded state for a long string node. */
  @JSExportTopLevel("jtStrToggle")
  def jtStrToggle(id: String): Unit = {
    val body    = dom.document.getElementById(s"$id-b")
    val summary = dom.document.getElementById(s"$id-s")
    val btn     = dom.document.getElementById(s"$id-btn")
    if (body == null) return

    val isOpen = body.asInstanceOf[dom.html.Element].style.display != "none"
    body.asInstanceOf[dom.html.Element].style.display = if (isOpen) "none" else ""
    if (summary != null) summary.asInstanceOf[dom.html.Element].style.display = if (isOpen) "" else "none"
    if (btn != null) btn.asInstanceOf[dom.html.Element].style.display = if (isOpen) "" else "none"

    val parentRow = body.asInstanceOf[dom.Element].closest(".jt-row")
    if (parentRow != null) { val _ = parentRow.classList.toggle("jt-no-ln", !isOpen) }

    // On expand: split long lines (first time only)
    if (!isOpen) {
      val bodyEl = body.asInstanceOf[dom.html.Element]
      if (bodyEl.dataset.get("split").isEmpty) {
        bodyEl.dataset.update("split", "1")
        val lined = bodyEl.closest(".jt-lined")
        if (lined != null) {
          val _ = dom.window.requestAnimationFrame { _ =>
            splitLongExpLines(lined.asInstanceOf[dom.html.Element], Some(bodyEl))
          }
        }
      }
    }
  }

  /** Toggle collapsed/expanded state for an object/array node. */
  @JSExportTopLevel("jtToggle")
  def jtToggle(id: String): Unit = {
    val body    = dom.document.getElementById(s"$id-b")
    val summary = dom.document.getElementById(s"$id-s")
    val btn     = dom.document.getElementById(s"$id-btn")
    if (body == null) return

    val isOpen = body.asInstanceOf[dom.html.Element].style.display != "none"
    body.asInstanceOf[dom.html.Element].style.display = if (isOpen) "none" else ""
    if (summary != null) summary.asInstanceOf[dom.html.Element].style.display = if (isOpen) "" else "none"
    if (btn != null) btn.textContent = if (isOpen) "\u25b6" else "\u25bc"
  }

  /** Render a JSON tree into a container element. */
  @JSExportTopLevel("renderJsonTree")
  def renderJsonTree(container: dom.html.Element, data: js.Dynamic): Unit = {
    val obj: js.Dynamic =
      if (js.typeOf(data) == "string") {
        try js.JSON.parse(data.asInstanceOf[String])
        catch {
          case _: Throwable =>
            container.textContent = data.asInstanceOf[String]
            return
        }
      } else data

    val totalBytes = utf8ByteLength(js.JSON.stringify(obj))
    AppState.jtLine = 0
    container.innerHTML = buildJsonHtml(obj, 0, "", totalBytes) +
      s"""<div class="jt-line-info">${AppState.jtLine} lines total</div>"""
    container.classList.add("jt-lined")
    splitLongExpLines(container)
  }

  /** Split long expanded string lines to fit the container width. */
  def splitLongExpLines(container: dom.html.Element, targetBlock: Option[dom.html.Element] = None): Unit = {
    // Measure character width
    val measure = dom.document.createElement("span").asInstanceOf[dom.html.Span]
    measure.style.cssText = "visibility:hidden;position:absolute;white-space:nowrap;font:inherit"
    measure.textContent = "X" * 100
    locally { val _ = container.appendChild(measure) }
    val charWidth = measure.offsetWidth / 100.0
    locally { val _ = container.removeChild(measure) }
    val availWidth = container.clientWidth - 60
    val maxChars   = Math.max(40, Math.floor(availWidth / charWidth).toInt)

    val blocks: Iterable[dom.Element] = targetBlock match {
      case Some(b) => List(b)
      case None =>
        val nl = container.querySelectorAll(".jt-str-expanded")
        (0 until nl.length).map(nl(_))
    }

    for (block <- blocks) {
      val lineNodes = block.querySelectorAll(".jt-exp-line")
      val lines     = (0 until lineNodes.length).map(lineNodes(_))
      if (lines.nonEmpty) {
        val needsSplit = lines.exists(_.textContent.length > maxChars)
        if (needsSplit) {
          val baseLn = {
            val attr = lines.head.getAttribute("data-ln")
            if (attr != null) try attr.toInt catch { case _: Throwable => 1 } else 1
          }
          var lineNum = baseLn

          case class Fragment(text: String, ln: Int, cls: String)
          val fragments = scala.collection.mutable.ArrayBuffer.empty[Fragment]

          for (line <- lines) {
            val text = line.textContent
            val cls  = if (line.classList.contains("mech-hl-text")) "jt-exp-line mech-hl-text" else "jt-exp-line"
            if (text.length <= maxChars) {
              fragments += Fragment(text, lineNum, cls)
              lineNum += 1
            } else {
              var i = 0
              while (i < text.length) {
                fragments += Fragment(text.slice(i, i + maxChars), lineNum, cls)
                lineNum += 1
                i += maxChars
              }
            }
          }

          // Rebuild DOM
          val toggle = block.querySelector(".jt-str-toggle")
          block.innerHTML = ""
          if (toggle != null) { val _ = block.appendChild(toggle) }
          for (f <- fragments) {
            val div = dom.document.createElement("div").asInstanceOf[dom.html.Div]
            div.className = f.cls
            div.setAttribute("data-ln", f.ln.toString)
            div.textContent = f.text
            val _ = block.appendChild(div)
          }
        }
      }
    }
  }
}
