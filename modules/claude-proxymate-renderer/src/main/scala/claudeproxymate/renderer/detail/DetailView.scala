package claudeproxymate.renderer.detail

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.analysis.{AnalysisRenderer, MechChips, MechHighlight}
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.json.JsonTreeViewer
import claudeproxymate.renderer.messages.MessageRenderer
import claudeproxymate.renderer.search.SearchNavigation
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.view.ViewHelpers
import org.scalajs.dom
import scalatags.Text.all.*

import scala.scalajs.js

/** Render the proxy detail panel (request/response/messages/analysis tabs).
  *
  * Thin orchestrator: routes to per-tab renderers, populates typed views
  * for the placeholder + Request/Response header, and runs post-render
  * side effects (json-tree hydration, mech highlight, search highlight,
  * input refocus). HTML building lives in [[DetailEmptyView]] +
  * [[DetailHeaderView]].
  */
object DetailView {

  private val PricingDate = "2026-04-20"

  def renderProxyDetail(): Unit = {
    val entry = AppState.proxyCaptures.find(e => e.id == AppState.selectedProxyId.map(_.asInstanceOf[js.Any]).orNull)
    val detail = dom.document.getElementById(HtmlIds.ProxyDetailView)
    if (detail == null) return

    val detailEl = detail.asInstanceOf[dom.html.Element]
    // Reset inline style from messages tab
    detailEl.style.cssText = "flex:1;overflow:hidden;display:flex;flex-direction:column"

    entry match {
      case None    => renderEmpty(detailEl)
      case Some(e) => renderEntry(e, detailEl)
    }
  }

  private def renderEmpty(detail: dom.html.Element): Unit = {
    val labels = DetailEmptyLabels(
      selectRequestTitle = I18n.t("proxy.selectRequestTitle"),
      selectRequestHint  = I18n.t("proxy.selectRequestHint"),
    )
    ViewHelpers.setInnerHtml(detail, DetailEmptyView.buildFrag(labels))
  }

  private def renderEntry(entry: js.Dynamic, detail: dom.html.Element): Unit = {
    if (AppState.proxyDetailTab == "messages") {
      MessageRenderer.renderProxyMessages(entry, detail)
      return
    }

    if (AppState.proxyDetailTab == "analysis") {
      renderAnalysisTab(entry, detail)
      return
    }

    // Request or Response tab
    val data: js.Dynamic = if (AppState.proxyDetailTab == "request") {
      entry.selectDynamic("body")
    } else {
      val resp = entry.selectDynamic("response")
      if (!js.isUndefined(resp) && resp != null) resp.selectDynamic("body")
      else null
    }

    val q             = AppState.proxyDetailSearch
    val searchBarFrag = buildSearchBarFrag(q)
    val mechChipsHtml = if (AppState.proxyDetailTab == "request") MechChips.buildMechFilterChips(entry.selectDynamic("body")) else ""

    if (js.isUndefined(data) || data == null) {
      val msg = if (AppState.proxyDetailTab == "response") I18n.t("proxy.waitingResponse") else I18n.t("proxy.noBody")
      val emptyBodyFrag = frag(
        div(style := "flex-shrink:0")(searchBarFrag, raw(mechChipsHtml)),
        div(style := "color:var(--dim);padding:16px;font-size:12px")(msg),
      )
      ViewHelpers.setInnerHtml(detail, emptyBodyFrag)
      return
    }

    val tokenPill: Option[TokenPill] =
      if (AppState.proxyDetailTab == "request") buildTokenPill(entry, data) else None

    val prevScrollTop = {
      val el = dom.document.getElementById(HtmlIds.ProxyDetailCode)
      if (el != null) el.asInstanceOf[dom.html.Element].scrollTop else 0.0
    }

    val tokenPillFrag: Frag = tokenPill match {
      case Some(p) => DetailHeaderView.buildTokenPillFrag(p)
      case None    => frag()
    }

    val full = frag(
      div(style := "flex-shrink:0")(searchBarFrag, raw(mechChipsHtml)),
      tokenPillFrag,
      div(
        cls   := "json-tree-view",
        id    := HtmlIds.ProxyDetailCode,
        style := "flex:1;overflow:auto",
      ),
    )
    ViewHelpers.setInnerHtml(detail, full)

    val code = dom.document.getElementById(HtmlIds.ProxyDetailCode).asInstanceOf[dom.html.Element]
    JsonTreeViewer.renderJsonTree(code, data)

    AppState.proxyDetailMechFilter.foreach { mechFilter =>
      locally { val _ = dom.window.requestAnimationFrame { _ => code.scrollTop = prevScrollTop } }
      MechHighlight.highlightMechInJsonTree(code, entry.selectDynamic("body"), mechFilter)
    }

    if (q.nonEmpty) {
      MechHighlight.applyDetailHighlight(code, q)
      val marks = code.querySelectorAll("mark.search-hl")
      if (marks.length > 0) {
        if (AppState.searchCurrentIdx < 0) AppState.searchCurrentIdx = 0
        if (AppState.searchCurrentIdx >= marks.length) AppState.searchCurrentIdx = 0
        locally { val _ = marks(AppState.searchCurrentIdx).classList.add("current") }
        locally {
          val _ = marks(AppState.searchCurrentIdx).asInstanceOf[js.Dynamic].scrollIntoView(
            js.Dynamic.literal("behavior" -> "smooth", "block" -> "center"),
          )
        }
        SearchNavigation.updateSearchCounter(marks.length)
      }
    }

    if (q.nonEmpty || AppState.detailSearchWasFocused) {
      val inp = dom.document.getElementById(HtmlIds.ProxyDetailSearchInput)
      if (inp != null) {
        val inputEl = inp.asInstanceOf[dom.html.Input]
        inputEl.focus()
        val len = inputEl.value.length
        inputEl.setSelectionRange(len, len)
      }
    }
  }

  private def renderAnalysisTab(entry: js.Dynamic, detail: dom.html.Element): Unit = {
    val prevAnalysisScroll = {
      val scrollEl = detail.querySelector("[style*=\"overflow:auto\"]")
      if (scrollEl != null) scrollEl.asInstanceOf[dom.html.Element].scrollTop else 0.0
    }

    AnalysisRenderer.renderProxyAnalysis(entry, detail)

    AppState.proxyDetailMechFilter.foreach { mechFilter =>
      locally { val _ = dom.window.requestAnimationFrame { _ =>
        val scrollEl = detail.querySelector("[style*=\"overflow:auto\"]")
        if (scrollEl != null) scrollEl.asInstanceOf[dom.html.Element].scrollTop = prevAnalysisScroll
        val activeSection = detail.querySelector(s"""[data-mech-key="$mechFilter"]""")
        if (activeSection != null) {
          locally { val _ = activeSection.classList.add("mech-section-active") }
          locally { val _ = activeSection.asInstanceOf[js.Dynamic].scrollIntoView(
            js.Dynamic.literal("block" -> "start", "behavior" -> "smooth")
          ) }
        }
      }}
    }

    if (AppState.proxyDetailSearch.nonEmpty) {
      MechHighlight.applyDetailHighlight(detail, AppState.proxyDetailSearch)
      val marks = detail.querySelectorAll("mark.search-hl")
      if (marks.length > 0) {
        if (AppState.searchCurrentIdx < 0) AppState.searchCurrentIdx = 0
        if (AppState.searchCurrentIdx >= marks.length) AppState.searchCurrentIdx = 0
        locally { val _ = marks(AppState.searchCurrentIdx).classList.add("current") }
        SearchNavigation.updateSearchCounter(marks.length)
      }
    }

    if (AppState.proxyDetailSearch.nonEmpty || AppState.detailSearchWasFocused) {
      val inp = dom.document.getElementById(HtmlIds.ProxyDetailSearchInput)
      if (inp != null) {
        val inputEl = inp.asInstanceOf[dom.html.Input]
        inputEl.focus()
        val len = inputEl.value.length
        inputEl.setSelectionRange(len, len)
      }
    }
  }

  private def buildSearchBarFrag(q: String): Frag = {
    val labels = SearchBarLabels(
      placeholder = I18n.t("analysis.searchPlaceholder"),
      clear       = I18n.t("messages.searchClear"),
      searchPrev  = I18n.t("search.prev"),
      searchNext  = I18n.t("search.next"),
    )
    DetailHeaderView.buildSearchBarFrag(
      searchInputId   = HtmlIds.ProxyDetailSearchInput,
      searchCounterId = HtmlIds.SearchCounter,
      query           = q,
      labels          = labels,
    )
  }

  private def fmtTok(n: Int): String =
    if (n >= 1000000) f"${n / 1000000.0}%.1fM"
    else if (n >= 1000) f"${n / 1000.0}%.1fK"
    else n.toString

  private def fmtCost(n: Double): String = f"$$$n%.4f"

  /** Build the typed token pill model for the Request tab.
    *
    * Returns `None` when there's no usable response/usage data and we'd
    * otherwise fall back to a tiny estimate-only pill (still emitted via
    * the Some branch — we always have at least the size badge).
    */
  private def buildTokenPill(entry: js.Dynamic, data: js.Dynamic): Option[TokenPill] = {
    val jsonStr = js.JSON.stringify(data)
    val bytes = {
      val encoder = js.Dynamic.newInstance(dom.window.asInstanceOf[js.Dynamic].TextEncoder)()
      encoder.encode(jsonStr).length.asInstanceOf[Int]
    }
    val kb = f"${bytes / 1024.0}%.1f"

    val resp = entry.selectDynamic("response")
    val respBody = if (!js.isUndefined(resp) && resp != null) resp.selectDynamic("body") else null
    val usage = if (respBody != null && !js.isUndefined(respBody)) respBody.selectDynamic("usage") else null

    if (!js.isUndefined(usage) && usage != null) {
      Some(buildUsageTokenPill(entry, kb, usage))
    } else {
      val tokens = Math.ceil(bytes.toDouble / 3.5).toInt
      val tokStr = fmtTok(tokens)
      val badges = List(
        TokenBadge(s"$kb KB", None),
        TokenBadge(s"~$tokStr tok (${I18n.t("token.estimated")})", None),
      )
      // No usage → no popover data needed, but we still need a data-cost
      // attribute so the click handler doesn't miss; emit empty JSON.
      Some(TokenPill(badges = badges, dataCost = "{}"))
    }
  }

  private def buildUsageTokenPill(entry: js.Dynamic, kb: String, usage: js.Dynamic): TokenPill = {
    def intField(name: String): Int = {
      val v = usage.selectDynamic(name)
      if (!js.isUndefined(v) && v != null) try v.asInstanceOf[Int] catch { case _: Throwable => 0 } else 0
    }

    val inputTokens = intField("input_tokens")
    val cacheRead   = intField("cache_read_input_tokens")
    val cacheWrite  = intField("cache_creation_input_tokens")
    val outTok      = intField("output_tokens")
    val totalIn     = inputTokens + cacheRead + cacheWrite
    val cachePct    = if (totalIn > 0) Math.round(cacheRead.toDouble / totalIn * 100).toInt else 0

    val model = {
      val reqBody       = entry.selectDynamic("body")
      val reqModel      = if (!js.isUndefined(reqBody) && reqBody != null) reqBody.selectDynamic("model") else null
      val respBody      = entry.selectDynamic("response")
      val respBodyInner = if (!js.isUndefined(respBody) && respBody != null) respBody.selectDynamic("body") else null
      val respModel     = if (respBodyInner != null && !js.isUndefined(respBodyInner)) respBodyInner.selectDynamic("model") else null
      if (!js.isUndefined(reqModel) && reqModel != null) reqModel.toString
      else if (!js.isUndefined(respModel) && respModel != null) respModel.toString
      else ""
    }

    val rates = ModelTier.forModel(model).rates
    val inP   = rates.input
    val outP  = rates.output
    val crP   = rates.cacheRead
    val cwP   = rates.cacheWrite5m

    val cost    = (inputTokens * inP + cacheRead * crP + cacheWrite * cwP + outTok * outP) / 1000000.0
    val costStr = fmtCost(cost)

    val popData = js.JSON.stringify(js.Dynamic.literal(
      "model"       -> (if (model.nonEmpty) model else I18n.t("token.unknown")),
      "kb"          -> kb,
      "pricingDate" -> PricingDate,
      "rows" -> js.Array(
        js.Dynamic.literal("label" -> I18n.t("token.cacheRead"),     "tokens" -> fmtTok(cacheRead),   "price" -> crP, "cost" -> fmtCost(cacheRead * crP / 1000000.0)),
        js.Dynamic.literal("label" -> I18n.t("token.cacheWrite"),    "tokens" -> fmtTok(cacheWrite),  "price" -> cwP, "cost" -> fmtCost(cacheWrite * cwP / 1000000.0)),
        js.Dynamic.literal("label" -> I18n.t("token.uncachedInput"), "tokens" -> fmtTok(inputTokens), "price" -> inP, "cost" -> fmtCost(inputTokens * inP / 1000000.0)),
        js.Dynamic.literal("label" -> I18n.t("token.output"),        "tokens" -> fmtTok(outTok),      "price" -> outP, "cost" -> fmtCost(outTok * outP / 1000000.0)),
      ),
      "total"    -> costStr,
      "cachePct" -> cachePct,
    ))

    val badges = scala.collection.mutable.ListBuffer.empty[TokenBadge]
    badges += TokenBadge(s"$kb KB", None)
    badges += TokenBadge(s"${I18n.t("token.input")} ${fmtTok(totalIn)}", None)
    badges += TokenBadge(s"${I18n.t("token.output")} ${fmtTok(outTok)}", None)
    if (cachePct > 0) badges += TokenBadge(s"${I18n.t("token.cache")} $cachePct%", Some("var(--green)"))
    badges += TokenBadge(costStr, Some("var(--yellow)"))

    TokenPill(badges = badges.toList, dataCost = popData.asInstanceOf[String])
  }

  def showDetailTab(tab: String): Unit = {
    AppState.proxyDetailTab = tab
    AppState.proxyDetailSearch = ""
    AppState.proxyDetailMechFilter = None

    val tabs = dom.document.querySelectorAll(".dtab")
    var i = 0
    while (i < tabs.length) {
      val btn  = tabs(i).asInstanceOf[dom.html.Element]
      val dtab = btn.dataset.get("dtab").getOrElse("")
      locally { val _ = btn.classList.toggle("active", dtab == tab) }
      i += 1
    }

    // Hide copy button for analysis/messages tabs
    val copyBtn = dom.document.getElementById(HtmlIds.CopyDetailBtn)
    if (copyBtn != null) {
      copyBtn.asInstanceOf[dom.html.Element].style.display =
        if (tab == "analysis" || tab == "messages") "none" else ""
    }

    renderProxyDetail()
  }
}
