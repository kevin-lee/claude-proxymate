package claudeproxymate.renderer.json

import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.view.ViewHelpers
import org.scalajs.dom
import scalatags.Text.all.{cls, div, frag, *}

import scala.scalajs.js

/** Interactive JSON tree viewer.
  *
  * Owns the document-level click handler that dispatches container /
  * long-string toggles, the post-render `splitLongExpLines` pass, and the
  * `renderJsonTree` entry point used by `AnalysisRenderer` and
  * `DetailView`. The HTML building lives in [[JsonTreeView]].
  */
object JsonTreeViewer {

  /** Install the document-level click handler. Called from RendererMain. */
  def install(): Unit =
    dom.document.addEventListener("click", handleClick _)

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return

    val containerEl = target.closest(
      s".${JsonTreeView.ContainerToggleClass},.${JsonTreeView.ContainerSummaryClass}",
    )
    if (containerEl != null) {
      val raw = containerEl.asInstanceOf[dom.html.Element].getAttribute(JsonTreeView.ContainerDataAttr)
      if (raw != null && raw.nonEmpty) { jtToggle(raw); return }
    }

    val stringEl = target.closest(
      s".${JsonTreeView.StringToggleClass},.${JsonTreeView.StringPreviewClass}",
    )
    if (stringEl != null) {
      val raw = stringEl.asInstanceOf[dom.html.Element].getAttribute(JsonTreeView.StringDataAttr)
      if (raw != null && raw.nonEmpty) { jtStrToggle(raw); return }
    }
  }

  /** Measure byte length of a string using TextEncoder (Web API). */
  private def utf8ByteLength(s: String): Int = {
    val encoder = js.Dynamic.newInstance(dom.window.asInstanceOf[js.Dynamic].TextEncoder)()
    encoder.encode(s).length.asInstanceOf[Int]
  }

  /** Toggle collapsed/expanded state for a long string node. Called by the
    * click handler and by [[claudeproxymate.renderer.analysis.MechHighlight]].
    */
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

  /** Toggle collapsed/expanded state for an object/array node. Called by
    * the click handler and by [[claudeproxymate.renderer.analysis.MechHighlight]].
    */
  def jtToggle(id: String): Unit = {
    val body    = dom.document.getElementById(s"$id-b")
    val summary = dom.document.getElementById(s"$id-s")
    val btn     = dom.document.getElementById(s"$id-btn")
    if (body == null) return

    val isOpen = body.asInstanceOf[dom.html.Element].style.display != "none"
    body.asInstanceOf[dom.html.Element].style.display = if (isOpen) "none" else ""
    if (summary != null) summary.asInstanceOf[dom.html.Element].style.display = if (isOpen) "" else "none"
    if (btn != null) btn.textContent = if (isOpen) "▶" else "▼"
  }

  /** Render a JSON tree into a container element. */
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
    val tree     = JsonTreeView.buildJsonFrag(obj, totalBytes)
    val lineInfo = div(cls := "jt-line-info")(s"${AppState.jtLine} lines total")
    ViewHelpers.setInnerHtml(container, frag(tree, lineInfo))
    container.classList.add("jt-lined")
    splitLongExpLines(container)
  }

  /** Split long expanded string lines to fit the container width. */
  def splitLongExpLines(container: dom.html.Element, targetBlock: Option[dom.html.Element] = None): Unit = {
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

          case class Fragment(text: String, ln: Int, cssClass: String)
          val fragments = scala.collection.mutable.ArrayBuffer.empty[Fragment]

          for (line <- lines) {
            val text   = line.textContent
            val cssCls = if (line.classList.contains("mech-hl-text")) "jt-exp-line mech-hl-text" else "jt-exp-line"
            if (text.length <= maxChars) {
              fragments += Fragment(text, lineNum, cssCls)
              lineNum += 1
            } else {
              var i = 0
              while (i < text.length) {
                fragments += Fragment(text.slice(i, i + maxChars), lineNum, cssCls)
                lineNum += 1
                i += maxChars
              }
            }
          }

          val toggle = block.querySelector(s".${JsonTreeView.StringToggleClass}")
          block.innerHTML = ""
          if (toggle != null) { val _ = block.appendChild(toggle) }
          for (f <- fragments) {
            val divEl = dom.document.createElement("div").asInstanceOf[dom.html.Div]
            divEl.className = f.cssClass
            divEl.setAttribute("data-ln", f.ln.toString)
            divEl.textContent = f.text
            val _ = block.appendChild(divEl)
          }
        }
      }
    }
  }
}
