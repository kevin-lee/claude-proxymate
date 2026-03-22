package claudeproxymate.renderer.detail

import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.util.HtmlUtil.esc
import org.scalajs.dom

import scala.scalajs.js

/** Token cost popover click handler.
  *
  * Ports the `document.addEventListener('click', ...)` handler
  * for token popovers from renderer.js.
  */
object TokenPopover {

  /** Install the document-level click handler. Called from RendererMain. */
  def install(): Unit = {
    dom.document.addEventListener("click", handleClick _)
  }

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.html.Element]

    // Copy button in popover
    if (target.classList.contains("token-popover-copy")) {
      val text = target.dataset.get("text").getOrElse("")
      locally { val _ = dom.window.navigator.clipboard.writeText(text) }
      target.textContent = I18n.t("token.copied")
      locally { val _ = js.timers.setTimeout(1500.0) { target.textContent = I18n.t("token.copyBtn") } }
      return
    }

    // Click inside popover — ignore
    val existingPop = target.asInstanceOf[dom.Element].closest(".token-popover")
    if (existingPop != null) return

    // Close existing popover
    val old = dom.document.querySelector(".token-popover")
    if (old != null) { locally { val _ = old.parentNode.removeChild(old) } }

    // Click on pill — open popover
    val pill = target.asInstanceOf[dom.Element].closest(".proxy-token-pill[data-cost]")
    if (pill == null) return

    val costData = pill.asInstanceOf[dom.html.Element].dataset.get("cost").getOrElse("{}")
    val d = js.JSON.parse(costData)

    val descs = Map(
      I18n.t("token.cacheRead") -> I18n.t("token.descCacheRead"),
      I18n.t("token.cacheWrite") -> I18n.t("token.descCacheWrite"),
      I18n.t("token.uncachedInput") -> I18n.t("token.descUncached"),
      I18n.t("token.output") -> I18n.t("token.descOutput"),
    )

    val rows = d.selectDynamic("rows").asInstanceOf[js.Array[js.Dynamic]]
    val modelStr = d.model.asInstanceOf[String]
    val kbStr = d.kb.asInstanceOf[String]
    val totalStr = d.total.asInstanceOf[String]
    val cachePctVal = d.cachePct.asInstanceOf[Int]
    val pricingDateStr = d.pricingDate.asInstanceOf[String]

    val textLines = scala.collection.mutable.ListBuffer.empty[String]
    textLines += s"${I18n.t("token.model")}: $modelStr"
    textLines += s"${I18n.t("token.reqSize")}: $kbStr KB"
    textLines += ""

    val rowsSb = new StringBuilder
    for (r <- rows) {
      val label = r.label.asInstanceOf[String]
      val tokens = r.tokens.asInstanceOf[String]
      val price = r.price.toString
      val costVal = r.cost.asInstanceOf[String]
      rowsSb.append(s"""<div class="token-popover-row"><span class="tp-label">${esc(label)}</span><span class="tp-formula">${esc(tokens)} tok × $$${esc(price)}/MTok</span><span class="tp-result">${esc(costVal)}</span></div>""")
      descs.get(label).foreach(desc => rowsSb.append(s"""<div class="tp-desc">$desc</div>"""))
      textLines += s"$label: $tokens tok × $$$price/MTok = $costVal"
    }
    textLines += ""
    textLines += s"${I18n.t("token.total")}: $totalStr"
    textLines += s"${I18n.t("token.cacheHitRate")}: $cachePctVal%"

    val noteSb = new StringBuilder
    noteSb.append("""<div class="token-popover-note">""")
    noteSb.append(I18n.t("token.notePricingDate", Map("date" -> pricingDateStr)))
    noteSb.append(s"""<br>${I18n.t("token.noteModelPrice")} (<a href="https://docs.anthropic.com/en/docs/about-claude/models#model-comparison" target="_blank" style="color:var(--blue);text-decoration:underline;cursor:pointer" onclick="event.stopPropagation();require('electron').shell.openExternal(this.href);return false;">${I18n.t("token.noteOfficialDoc")}</a>)""")
    noteSb.append(s"<br>${I18n.t("token.noteMTok")}")
    if (cachePctVal >= 50) noteSb.append(s"<br>${I18n.t("token.noteCacheSaving", Map("pct" -> cachePctVal.toString))}")
    noteSb.append("</div>")

    val textForCopy = textLines.mkString("\n").replace("\"", "&quot;")
    val pop = dom.document.createElement("div").asInstanceOf[dom.html.Div]
    pop.className = "token-popover"
    pop.innerHTML =
      s"""<div class="token-popover-title"><span>${I18n.t("token.costTitle")}</span><span class="token-popover-copy" data-text="$textForCopy">${I18n.t("token.copyBtn")}</span></div>""" +
        s"""<div class="token-popover-info">${I18n.t("token.model")}: ${esc(modelStr)} · ${I18n.t("token.reqSize")}: ${esc(kbStr)} KB</div>""" +
        rowsSb.toString() +
        s"""<div class="token-popover-row tp-total"><span class="tp-label">${I18n.t("token.total")}</span><span class="tp-result">${esc(totalStr)}</span></div>""" +
        s"""<div class="token-popover-row tp-total"><span class="tp-label">${I18n.t("token.cacheHitRate")}</span><span class="tp-result">${esc(cachePctVal.toString)}%</span></div>""" +
        noteSb.toString()

    locally { val _ = pill.appendChild(pop) }
  }
}
