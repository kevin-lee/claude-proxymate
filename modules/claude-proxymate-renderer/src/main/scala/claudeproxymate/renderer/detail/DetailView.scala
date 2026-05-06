package claudeproxymate.renderer.detail

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.analysis.{AnalysisRenderer, MechChips, MechHighlight}
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.json.JsonTreeViewer
import claudeproxymate.renderer.messages.MessageRenderer
import claudeproxymate.renderer.search.SearchNavigation
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.HtmlUtil.escAttr
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Render the proxy detail panel (request/response/messages/analysis tabs).
  *
  * Ports `renderProxyDetail`, `showDetailTab` from renderer.js.
  */
object DetailView {

  private val PricingDate = "2026-04-20"

  @JSExportTopLevel("renderProxyDetail")
  def renderProxyDetail(): Unit = {
    val entry = AppState.proxyCaptures.find(e => e.id == AppState.selectedProxyId.map(_.asInstanceOf[js.Any]).orNull)
    val detail = dom.document.getElementById(HtmlIds.ProxyDetailView)
    if (detail == null) return

    val detailEl = detail.asInstanceOf[dom.html.Element]
    // Reset inline style from messages tab
    detailEl.style.cssText = "flex:1;overflow:hidden;display:flex;flex-direction:column"

    entry match {
      case None =>
        detailEl.innerHTML = s"""<div class="proxy-empty"><span style="font-size:28px">\uD83D\uDD0D</span><span>${I18n.t("proxy.selectRequest")}</span></div>"""
        return
      case Some(e) => renderEntry(e, detailEl)
    }
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

    val q = AppState.proxyDetailSearch
    val searchBar = buildSearchBar(q)
    val mechChips = if (AppState.proxyDetailTab == "request") MechChips.buildMechFilterChips(entry.selectDynamic("body")) else ""
    val header = s"""<div style="flex-shrink:0">$searchBar$mechChips</div>"""

    if (js.isUndefined(data) || data == null) {
      val msg = if (AppState.proxyDetailTab == "response") I18n.t("proxy.waitingResponse") else I18n.t("proxy.noBody")
      detail.innerHTML = header + s"""<div style="color:var(--dim);padding:16px;font-size:12px">$msg</div>"""
      return
    }

    val tokenInfo = if (AppState.proxyDetailTab == "request") buildTokenInfo(entry, data) else ""

    val prevScrollTop = {
      val el = dom.document.getElementById(HtmlIds.ProxyDetailCode)
      if (el != null) el.asInstanceOf[dom.html.Element].scrollTop else 0.0
    }

    detail.innerHTML = header + tokenInfo + s"""<div class="json-tree-view" id="${HtmlIds.ProxyDetailCode}" style="flex:1;overflow:auto"></div>"""
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
        locally { val _ = marks(AppState.searchCurrentIdx).asInstanceOf[js.Dynamic].scrollIntoView(
          js.Dynamic.literal("behavior" -> "smooth", "block" -> "center")
        ) }
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

  private def buildSearchBar(q: String): String = {
    // Inline handlers are intentionally NOT emitted here; the input + buttons
    // are wired by ProxyDetailSearchListeners (a single document-level listener
    // installed by RendererMain). Inline handlers turned out not to fire from
    // Scala.js NoModule output in this Electron version — symptoms: typing
    // produces no result update and apparent focus loss.
    val searchNav = if (q.nonEmpty) {
      s"""<div class="search-nav">""" +
        s"""<span id="${HtmlIds.SearchCounter}"></span>""" +
        s"""<button class="search-nav-btn search-nav-prev" title="${escAttr(I18n.t("search.prev"))}">▲</button>""" +
        s"""<button class="search-nav-btn search-nav-next" title="${escAttr(I18n.t("search.next"))}">▼</button>""" +
        "</div>"
    } else ""

    val clearBtn = if (q.nonEmpty)
      s"""<button class="msg-search-clear" title="${escAttr(I18n.t("messages.searchClear"))}">✕</button>"""
    else ""

    s"""<div class="msg-search-bar" style="flex-shrink:0">""" +
      s"""<input type="text" class="msg-search-input" id="${HtmlIds.ProxyDetailSearchInput}" """ +
      s"""placeholder="${escAttr(I18n.t("analysis.searchPlaceholder"))}" value="${escAttr(q)}">""" +
      searchNav + clearBtn +
      "</div>"
  }

  private def fmtTok(n: Int): String =
    if (n >= 1000000) f"${n / 1000000.0}%.1fM"
    else if (n >= 1000) f"${n / 1000.0}%.1fK"
    else n.toString

  private def fmtCost(n: Double): String = f"$$$n%.4f"

  private def buildTokenInfo(entry: js.Dynamic, data: js.Dynamic): String = {
    val jsonStr = js.JSON.stringify(data)
    val bytes = {
      val encoder = js.Dynamic.newInstance(dom.window.asInstanceOf[js.Dynamic].TextEncoder)()
      encoder.encode(jsonStr).length.asInstanceOf[Int]
    }
    val kb = f"${bytes / 1024.0}%.1f"

    // Try to get usage from response
    val resp = entry.selectDynamic("response")
    val respBody = if (!js.isUndefined(resp) && resp != null) resp.selectDynamic("body") else null
    val usage = if (respBody != null && !js.isUndefined(respBody)) respBody.selectDynamic("usage") else null

    if (!js.isUndefined(usage) && usage != null) {
      buildUsageTokenInfo(entry, kb, usage)
    } else {
      val tokens = Math.ceil(bytes.toDouble / 3.5).toInt
      val tokStr = fmtTok(tokens)
      s"""<div class="proxy-token-pill"><span class="tt-badge">$kb KB</span><span class="tt-badge">~$tokStr tok (${I18n.t("token.estimated")})</span></div>"""
    }
  }

  private def buildUsageTokenInfo(entry: js.Dynamic, kb: String, usage: js.Dynamic): String = {
    def intField(name: String): Int = {
      val v = usage.selectDynamic(name)
      if (!js.isUndefined(v) && v != null) try v.asInstanceOf[Int] catch { case _: Throwable => 0 } else 0
    }

    val inputTokens = intField("input_tokens")
    val cacheRead = intField("cache_read_input_tokens")
    val cacheWrite = intField("cache_creation_input_tokens")
    val outTok = intField("output_tokens")
    val totalIn = inputTokens + cacheRead + cacheWrite
    val cachePct = if (totalIn > 0) Math.round(cacheRead.toDouble / totalIn * 100).toInt else 0

    // Model-based pricing ($/MTok)
    val model = {
      val reqBody = entry.selectDynamic("body")
      val reqModel = if (!js.isUndefined(reqBody) && reqBody != null) reqBody.selectDynamic("model") else null
      val respBody = entry.selectDynamic("response")
      val respBodyInner = if (!js.isUndefined(respBody) && respBody != null) respBody.selectDynamic("body") else null
      val respModel = if (respBodyInner != null && !js.isUndefined(respBodyInner)) respBodyInner.selectDynamic("model") else null
      if (!js.isUndefined(reqModel) && reqModel != null) reqModel.toString
      else if (!js.isUndefined(respModel) && respModel != null) respModel.toString
      else ""
    }

    val rates = ModelTier.forModel(model).rates
    val inP = rates.input
    val outP = rates.output
    val crP = rates.cacheRead
    val cwP = rates.cacheWrite5m

    val cost = (inputTokens * inP + cacheRead * crP + cacheWrite * cwP + outTok * outP) / 1000000.0
    val costStr = fmtCost(cost)

    val popData = js.JSON.stringify(js.Dynamic.literal(
      "model" -> (if (model.nonEmpty) model else I18n.t("token.unknown")),
      "kb" -> kb,
      "pricingDate" -> PricingDate,
      "rows" -> js.Array(
        js.Dynamic.literal("label" -> I18n.t("token.cacheRead"), "tokens" -> fmtTok(cacheRead), "price" -> crP, "cost" -> fmtCost(cacheRead * crP / 1000000.0)),
        js.Dynamic.literal("label" -> I18n.t("token.cacheWrite"), "tokens" -> fmtTok(cacheWrite), "price" -> cwP, "cost" -> fmtCost(cacheWrite * cwP / 1000000.0)),
        js.Dynamic.literal("label" -> I18n.t("token.uncachedInput"), "tokens" -> fmtTok(inputTokens), "price" -> inP, "cost" -> fmtCost(inputTokens * inP / 1000000.0)),
        js.Dynamic.literal("label" -> I18n.t("token.output"), "tokens" -> fmtTok(outTok), "price" -> outP, "cost" -> fmtCost(outTok * outP / 1000000.0)),
      ),
      "total" -> costStr,
      "cachePct" -> cachePct,
    ))

    val sb = new StringBuilder
    sb.append(s"""<span class="tt-badge">$kb KB</span>""")
    sb.append(s"""<span class="tt-badge">${I18n.t("token.input")} ${fmtTok(totalIn)}</span>""")
    sb.append(s"""<span class="tt-badge">${I18n.t("token.output")} ${fmtTok(outTok)}</span>""")
    if (cachePct > 0) sb.append(s"""<span class="tt-badge" style="color:var(--green)">${I18n.t("token.cache")} $cachePct%</span>""")
    sb.append(s"""<span class="tt-badge" style="color:var(--yellow)">$costStr</span>""")

    s"""<div class="proxy-token-pill" data-cost='${popData.replace("'", "&#39;")}'>${sb.toString()}</div>"""
  }

  @JSExportTopLevel("showDetailTab")
  def showDetailTab(tab: String): Unit = {
    AppState.proxyDetailTab = tab
    AppState.proxyDetailSearch = ""
    AppState.proxyDetailMechFilter = None

    val tabs = dom.document.querySelectorAll(".dtab")
    var i = 0
    while (i < tabs.length) {
      val btn = tabs(i).asInstanceOf[dom.html.Element]
      val dtab = btn.dataset.get("dtab").getOrElse("")
      locally { val _ = btn.classList.toggle("active", dtab == tab) }
      i += 1
    }

    // Hide copy button for analysis/messages tabs
    val copyBtn = dom.document.querySelector("[onclick=\"copyProxyDetail()\"]")
    if (copyBtn != null) {
      copyBtn.asInstanceOf[dom.html.Element].style.display =
        if (tab == "analysis" || tab == "messages") "none" else ""
    }

    renderProxyDetail()
  }
}
