package claudeproxymate.renderer.detail

import claudeproxymate.renderer.facades.ElectronApi
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.view.ViewHelpers
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
    dom.document.addEventListener("click", handleClick(_))
  }

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.html.Element]

    // External link inside popover — route through electronAPI.openExternal
    val classNames = {
      val buf = scala.collection.mutable.ListBuffer.empty[String]
      for (i <- 0 until target.classList.length) buf += target.classList.item(i)
      buf.toList
    }
    val href       = {
      val raw = target.getAttribute("href")
      if (raw == null) "" else raw
    }
    TokenPopoverLinks.extractExternalLinkHref(classNames, href) match {
      case Some(h) =>
        e.preventDefault()
        e.stopPropagation()
        ElectronApi.get.foreach { api =>
          val _ = api
            .openExternal(h)
            .`then`[Unit]({ (result: js.Dynamic) =>
              if (!js.isUndefined(result) && result != null) {
                val ok = result.selectDynamic("ok")
                if (!js.isUndefined(ok) && ok.asInstanceOf[Boolean]) ()
                else {
                  val reason = result.selectDynamic("reason")
                  dom
                    .console
                    .warn(
                      "openExternal refused:",
                      if (js.isUndefined(reason) || reason == null) "(no reason)" else reason.toString,
                    )
                }
              }
            }: js.Function1[js.Dynamic, Unit])
        }
        return
      case None => ()
    }

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
    val d        = js.JSON.parse(costData)

    val rows = d.selectDynamic("rows").asInstanceOf[js.Array[js.Dynamic]].toList.map { r =>
      TokenPopoverRow(
        label = r.label.asInstanceOf[String],
        tokens = r.tokens.asInstanceOf[String],
        price = r.price.toString,
        cost = r.cost.asInstanceOf[String],
      )
    }

    val data = TokenPopoverData(
      model = d.model.asInstanceOf[String],
      kb = d.kb.asInstanceOf[String],
      total = d.total.asInstanceOf[String],
      cachePct = d.cachePct.asInstanceOf[Int],
      pricingDate = d.pricingDate.asInstanceOf[String],
      rows = rows,
    )

    val descriptions = Map(
      I18n.t("token.cacheRead")     -> I18n.t("token.descCacheRead"),
      I18n.t("token.cacheWrite")    -> I18n.t("token.descCacheWrite"),
      I18n.t("token.uncachedInput") -> I18n.t("token.descUncached"),
      I18n.t("token.output")        -> I18n.t("token.descOutput"),
    )

    val labels = TokenPopoverLabels(
      costTitle = I18n.t("token.costTitle"),
      modelLabel = I18n.t("token.model"),
      reqSizeLabel = I18n.t("token.reqSize"),
      copyBtn = I18n.t("token.copyBtn"),
      totalLabel = I18n.t("token.total"),
      cacheHitRateLabel = I18n.t("token.cacheHitRate"),
      notePricingDate = I18n.t("token.notePricingDate", Map("date" -> data.pricingDate)),
      noteModelPrice = I18n.t("token.noteModelPrice"),
      noteOfficialDoc = I18n.t("token.noteOfficialDoc"),
      noteMTok = I18n.t("token.noteMTok"),
      noteCacheSaving =
        Option.when(data.cachePct >= 50)(I18n.t("token.noteCacheSaving", Map("pct" -> data.cachePct.toString))),
    )

    val pop = dom.document.createElement("div").asInstanceOf[dom.html.Div]
    pop.className = "token-popover"
    ViewHelpers.setInnerHtml(pop, TokenPopoverView.buildPopoverFrag(data, labels, descriptions))
    locally { val _ = pill.appendChild(pop) }
  }
}
