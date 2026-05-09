package claudeproxymate.renderer.json

import claudeproxymate.renderer.detail.DetailView
import claudeproxymate.renderer.i18n.I18n
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

    val tokenEl = target.closest(s".${JsonTreeView.TokenMaskClass},.${JsonTreeView.TokenMaskRevealedClass}")
    if (tokenEl != null) {
      val raw = tokenEl.asInstanceOf[dom.html.Element].getAttribute(JsonTreeView.TokenMaskDataAttr)
      // Messages-tab token ids are prefixed `m.` and owned by
      // `MessageRenderer.handleClick` (different scroll-preserve
      // behavior). Skip them here.
      if (raw != null && raw.nonEmpty && !raw.startsWith("m.")) { toggleTokenReveal(raw); return }
      if (raw != null && raw.startsWith("m.")) return
    }

    val corrEl = target.closest(s".${JsonTreeView.CorrMaskClass},.${JsonTreeView.CorrMaskRevealedClass}")
    if (corrEl != null) {
      val raw = corrEl.asInstanceOf[dom.html.Element].getAttribute(JsonTreeView.CorrMaskDataAttr)
      /* Same namespace discrimination as token mask: messages-tab
       * corr-ids are prefixed `corr:m.` and owned by MessageRenderer.
       */
      if (raw != null && raw.nonEmpty && !raw.startsWith("corr:m.")) { toggleCorrReveal(raw); return }
      if (raw != null && raw.startsWith("corr:m.")) return
    }

    val maskEl = target.closest(s".${JsonTreeView.MaskClass},.${JsonTreeView.MaskRevealedClass}")
    if (maskEl != null) {
      val raw = maskEl.asInstanceOf[dom.html.Element].getAttribute(JsonTreeView.MaskDataAttr)
      if (raw != null && raw.nonEmpty) { toggleMaskReveal(raw); return }
    }

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

  /** Toggle the per-value reveal state for a masked field, then swap
    * just the single `[data-mask-id]` element in the live DOM. We
    * avoid a full `DetailView.renderProxyDetail()` so the scrollable
    * container's `scrollTop` is not reset; the user stays where
    * they were.
    *
    * Falls back to a full re-render only if the live DOM lookup or
    * the path-walk against the capture body fails — that should not
    * happen in practice, but the safety net keeps the user-visible
    * behaviour correct if the data shape changed under us.
    */
  private def toggleMaskReveal(maskId: String): Unit = {
    if (AppState.maskRevealed.contains(maskId)) {
      val _ = AppState.maskRevealed.remove(maskId)
    } else {
      val _ = AppState.maskRevealed.add(maskId)
    }

    val swapped = trySwapMaskInPlace(maskId)
    if (!swapped) DetailView.renderProxyDetail()
  }

  private def trySwapMaskInPlace(path: String): Boolean = {
    val container = dom.document.getElementById(claudeproxymate.core.HtmlIds.ProxyDetailCode)
    if (container == null) return false

    val cssEscape   = js.Dynamic.global.CSS.applyDynamic("escape")(path).asInstanceOf[String]
    val live        = container.querySelector(s"[${JsonTreeView.MaskDataAttr}=\"$cssEscape\"]")
    if (live == null) return false

    // Resolve the value at `path` against the active capture body.
    val entry = AppState.proxyCaptures.find(e => e.id == AppState.selectedProxyId.map(_.asInstanceOf[js.Any]).orNull)
    val body: js.Dynamic = entry match {
      case None    => null.asInstanceOf[js.Dynamic]
      case Some(e) =>
        if (AppState.proxyDetailTab == "request") e.selectDynamic("body")
        else {
          val resp = e.selectDynamic("response")
          if (!js.isUndefined(resp) && resp != null) resp.selectDynamic("body")
          else null.asInstanceOf[js.Dynamic]
        }
    }
    if (body == null || js.isUndefined(body)) return false

    val value = JsonTreeView.resolvePath(body, path)
    if (value == null && path != "$") {
      // Path didn't resolve. Bail out so the caller falls back to a
      // full re-render rather than drawing nonsense.
      return false
    }

    val maskLabel = I18n.t("mask.hiddenLabel")
    val newFrag   = JsonTreeView.buildMaskFrag(path, value, maskLabel, depth = 0, totalBytes = 0).render

    // Parse the freshly-rendered HTML and replace the live element
    // with the new node. A throwaway <div> is fine as a parser
    // sandbox; the outer span we want is its firstChild.
    val sandbox = dom.document.createElement("div").asInstanceOf[dom.html.Element]
    sandbox.innerHTML = newFrag
    val replacement = sandbox.firstChild
    if (replacement == null) return false
    val _ = live.asInstanceOf[js.Dynamic].replaceWith(replacement)
    true
  }

  /** Toggle reveal state for a regex-detected token mask, then swap
    * the single span in the live DOM. The token id is `path#offset`;
    * we resolve the parent string by `path`, then locate the match
    * at `offset` in the scanned token list to recover the original
    * matched substring.
    */
  private def toggleTokenReveal(tokenId: String): Unit = {
    if (AppState.maskRevealed.contains(tokenId)) {
      val _ = AppState.maskRevealed.remove(tokenId)
    } else {
      val _ = AppState.maskRevealed.add(tokenId)
    }

    val swapped = trySwapTokenInPlace(tokenId)
    if (!swapped) DetailView.renderProxyDetail()
  }

  private def trySwapTokenInPlace(tokenId: String): Boolean = {
    val container = dom.document.getElementById(claudeproxymate.core.HtmlIds.ProxyDetailCode)
    if (container == null) return false

    val cssEscape = js.Dynamic.global.CSS.applyDynamic("escape")(tokenId).asInstanceOf[String]
    val live      = container.querySelector(s"[${JsonTreeView.TokenMaskDataAttr}=\"$cssEscape\"]")
    if (live == null) return false

    // Split `path#offset`. `#` doesn't appear in JSON paths the
    // builder emits, so a single `lastIndexOf` is unambiguous.
    val hashAt = tokenId.lastIndexOf('#')
    if (hashAt < 0) return false
    val path   = tokenId.substring(0, hashAt)
    val offset =
      try tokenId.substring(hashAt + 1).toInt
      catch { case _: NumberFormatException => return false }

    val entry = AppState.proxyCaptures.find(e => e.id == AppState.selectedProxyId.map(_.asInstanceOf[js.Any]).orNull)
    val body: js.Dynamic = entry match {
      case None    => null.asInstanceOf[js.Dynamic]
      case Some(e) =>
        if (AppState.proxyDetailTab == "request") e.selectDynamic("body")
        else {
          val resp = e.selectDynamic("response")
          if (!js.isUndefined(resp) && resp != null) resp.selectDynamic("body")
          else null.asInstanceOf[js.Dynamic]
        }
    }
    if (body == null || js.isUndefined(body)) return false

    val parent = JsonTreeView.resolvePath(body, path)
    if (parent == null || js.isUndefined(parent) || js.typeOf(parent) != "string") return false
    val parentStr = parent.asInstanceOf[String]

    val matches = claudeproxymate.core.TokenPatterns.scan(parentStr)
    matches.find(_.start == offset) match {
      case None => false
      case Some(m) =>
        val raw     = parentStr.substring(m.start, m.end)
        val newFrag = JsonTreeView.buildTokenMaskFrag(path, m.start, raw).render
        val sandbox = dom.document.createElement("div").asInstanceOf[dom.html.Element]
        sandbox.innerHTML = newFrag
        val replacement = sandbox.firstChild
        if (replacement == null) false
        else {
          val _ = live.asInstanceOf[js.Dynamic].replaceWith(replacement)
          true
        }
    }
  }

  /** Toggle reveal state for a correlation-id mask (PR 3). Same
    * shape as `toggleTokenReveal` but routes to the corr-mask
    * builder + `CorrelationIds.scan` for value lookup.
    */
  private def toggleCorrReveal(corrId: String): Unit = {
    if (AppState.maskRevealed.contains(corrId)) {
      val _ = AppState.maskRevealed.remove(corrId)
    } else {
      val _ = AppState.maskRevealed.add(corrId)
    }

    val swapped = trySwapCorrInPlace(corrId)
    if (!swapped) DetailView.renderProxyDetail()
  }

  private def trySwapCorrInPlace(corrId: String): Boolean = {
    val container = dom.document.getElementById(claudeproxymate.core.HtmlIds.ProxyDetailCode)
    if (container == null) return false

    val cssEscape = js.Dynamic.global.CSS.applyDynamic("escape")(corrId).asInstanceOf[String]
    val live      = container.querySelector(s"[${JsonTreeView.CorrMaskDataAttr}=\"$cssEscape\"]")
    if (live == null) return false

    // Strip the `corr:` namespace prefix, then split `path#offset`.
    if (!corrId.startsWith("corr:")) return false
    val payload = corrId.substring("corr:".length)
    val hashAt  = payload.lastIndexOf('#')
    if (hashAt < 0) return false
    val path   = payload.substring(0, hashAt)
    val offset =
      try payload.substring(hashAt + 1).toInt
      catch { case _: NumberFormatException => return false }

    val entry = AppState.proxyCaptures.find(e => e.id == AppState.selectedProxyId.map(_.asInstanceOf[js.Any]).orNull)
    val body: js.Dynamic = entry match {
      case None    => null.asInstanceOf[js.Dynamic]
      case Some(e) =>
        if (AppState.proxyDetailTab == "request") e.selectDynamic("body")
        else {
          val resp = e.selectDynamic("response")
          if (!js.isUndefined(resp) && resp != null) resp.selectDynamic("body")
          else null.asInstanceOf[js.Dynamic]
        }
    }
    if (body == null || js.isUndefined(body)) return false

    val parent = JsonTreeView.resolvePath(body, path)
    if (parent == null || js.isUndefined(parent) || js.typeOf(parent) != "string") return false
    val parentStr = parent.asInstanceOf[String]

    val matches = claudeproxymate.core.CorrelationIds.scan(parentStr)
    matches.find(_.start == offset) match {
      case None    => false
      case Some(m) =>
        val raw     = parentStr.substring(m.start, m.end)
        val newFrag = JsonTreeView.buildCorrMaskFrag(path, m.start, raw, m.name).render
        val sandbox = dom.document.createElement("div").asInstanceOf[dom.html.Element]
        sandbox.innerHTML = newFrag
        val replacement = sandbox.firstChild
        if (replacement == null) false
        else {
          val _ = live.asInstanceOf[js.Dynamic].replaceWith(replacement)
          true
        }
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
    val tree     = JsonTreeView.buildJsonFrag(obj, totalBytes, I18n.t("mask.hiddenLabel"))
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
      // Skip the wrap pass when any line contains a token-mask span.
      // The wrap rebuilds the block as plain `<div>`s with
      // `textContent`, which would flatten the span structure and
      // break click-to-reveal. Long lines with tokens overflow
      // horizontally instead — acceptable trade-off for correctness.
      val hasTokenMask = block.querySelector(s".${JsonTreeView.TokenMaskClass},.${JsonTreeView.TokenMaskRevealedClass}") != null
      if (lines.nonEmpty && !hasTokenMask) {
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
