package claudeproxymate.renderer.messages

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.Debounce
import claudeproxymate.renderer.util.HtmlUtil.{esc, escAttr, highlightSearch}
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Render proxy messages tab: filter, search, message cards.
  *
  * Ports `renderUserMsgContent`, `renderProxyMessages`,
  * `setMsgFilter`, `setMsgSearch` from renderer.js.
  */
object MessageRenderer {

  private val msgSearchDebounce = new Debounce(150)

  /** Render a single user message's text content (typed portions + injected blocks).
    * Returns `None` if `typedOnly` and there are no typed portions.
    */
  def renderUserMsgContent(text: String, typedOnly: Boolean, query: String): Option[String] = {
    val parts = MessageParser.parseUserText(text)

    if (typedOnly) {
      val typed = parts.collect { case p: MessageParser.TextPart => p }
      if (typed.isEmpty) None
      else Some(typed.map(p => s"""<div class="msg-typed">${highlightSearch(esc(p.content), query)}</div>""").mkString)
    } else {
      val html = parts.map {
        case MessageParser.TextPart(content) =>
          s"""<div class="msg-typed">${highlightSearch(esc(content), query)}</div>"""
        case MessageParser.InjectedPart(label, content, cls) =>
          val uid = dom.window.asInstanceOf[js.Dynamic].Math.random().applyDynamic("toString")(36).asInstanceOf[String].substring(2, 8)
          s"""<div class="msg-injected-row">""" +
            s"""<span id="bb_$uid" class="msg-badge $cls expandable" onclick="toggleBadge('$uid')">${esc(label)}</span>""" +
            s"""<div id="bc_$uid" class="badge-expand-content" style="display:none">${highlightSearch(esc(content), query)}</div>""" +
            "</div>"
      }.mkString
      Some(html)
    }
  }

  @JSExportTopLevel("renderProxyMessages")
  def renderProxyMessages(entry: js.Dynamic, container: dom.html.Element): Unit = {
    val body = entry.selectDynamic("body")
    val msgs: js.Array[js.Dynamic] =
      if (!js.isUndefined(body) && body != null) {
        val m = body.selectDynamic("messages")
        if (!js.isUndefined(m) && m != null) m.asInstanceOf[js.Array[js.Dynamic]]
        else js.Array[js.Dynamic]()
      } else js.Array[js.Dynamic]()

    if (msgs.length == 0) {
      container.innerHTML = s"""<div class="proxy-empty"><span>${I18n.t("proxy.noMessages")}</span></div>"""
      return
    }

    val typedOnly    = AppState.msgFilter == "typed"
    val isUserFilter = AppState.msgFilter == "user" || typedOnly
    val q            = AppState.msgSearchQuery

    val baseFiltered: js.Array[js.Dynamic] =
      if (isUserFilter) msgs.filter(m => m.role.asInstanceOf[String] == "user")
      else if (AppState.msgFilter == "assistant") msgs.filter(m => m.role.asInstanceOf[String] == "assistant")
      else msgs

    val filterHtml = buildFilterHtml()
    val searchHtml = buildSearchHtml(q)

    val cards = scala.collection.mutable.ListBuffer.empty[String]
    for (msg <- baseFiltered) {
      val role = msg.role.asInstanceOf[String]
      val rawContent = msg.selectDynamic("content")
      val contents: js.Array[js.Dynamic] =
        if (js.Array.isArray(rawContent)) rawContent.asInstanceOf[js.Array[js.Dynamic]]
        else {
          val text = if (!js.isUndefined(rawContent) && rawContent != null) rawContent.toString else ""
          js.Array(js.Dynamic.literal("type" -> "text", "text" -> text))
        }

      // Skip user messages with no typed text when filtering
      val shouldSkip = isUserFilter && role == "user" && !contents.exists { c =>
        val cType = c.selectDynamic("type")
        val cText = c.selectDynamic("text")
        if (cType.asInstanceOf[String] != "text") false
        else if (js.isUndefined(cText) || cText == null || cText.toString.trim.isEmpty) false
        else MessageParser.parseUserText(cText.toString).exists(_.isInstanceOf[MessageParser.TextPart])
      }

      if (!shouldSkip) {
        val bodyParts = buildBodyParts(contents, role, isUserFilter, q)
        if (bodyParts.nonEmpty) {
          val joined = bodyParts.mkString
          if (q.isEmpty || joined.contains("""class="search-hl"""")) {
            cards += s"""<div class="msg-card msg-${esc(role)}">""" +
              s"""<div class="msg-role">${esc(role)}</div>""" +
              s"""<div class="msg-body">$joined</div>""" +
              "</div>"
          }
        }
      }
    }

    // Override container styles for scrollable layout
    container.style.cssText = "flex:1;overflow-y:auto;display:block"
    container.innerHTML =
      s"""<div style="position:sticky;top:0;z-index:1;background:var(--bg)">$filterHtml$searchHtml</div>""" +
        s"""<div style="display:flex;flex-direction:column;gap:8px;padding:12px">""" +
        (if (cards.isEmpty) s"""<div class="proxy-empty"><span>${I18n.t("proxy.noResults")}</span></div>"""
         else cards.mkString) +
        "</div>"

    val countEl = dom.document.getElementById(HtmlIds.MsgCountEl)
    if (countEl != null) {
      countEl.textContent = I18n.t("proxy.msgCount", Map("count" -> cards.length.toString, "total" -> msgs.length.toString))
    }

    // Restore focus on search input after re-render
    val inp = dom.document.getElementById(HtmlIds.MsgSearchInput)
    if (inp != null && (q.nonEmpty || AppState.msgSearchWasFocused)) {
      val inputEl = inp.asInstanceOf[dom.html.Input]
      inputEl.focus()
      val len = inputEl.value.length
      inputEl.setSelectionRange(len, len)
    }
  }

  private def buildFilterHtml(): String = {
    def active(f: String): String = if (AppState.msgFilter == f) " active" else ""
    s"""<div class="msg-filter">""" +
      s"""<button class="mf-btn${active("user")}" onclick="setMsgFilter('user')">${I18n.t("messages.filterUser")}</button>""" +
      s"""<button class="mf-btn${active("typed")}" onclick="setMsgFilter('typed')">${I18n.t("messages.filterTyped")}</button>""" +
      s"""<button class="mf-btn${active("assistant")}" onclick="setMsgFilter('assistant')">${I18n.t("messages.filterAssistant")}</button>""" +
      s"""<button class="mf-btn${active("all")}" onclick="setMsgFilter('all')">${I18n.t("messages.filterAll")}</button>""" +
      s"""<span class="msg-count" id="${HtmlIds.MsgCountEl}"></span>""" +
      "</div>"
  }

  private def buildSearchHtml(q: String): String = {
    val clearBtn =
      if (q.nonEmpty)
        s"""<button class="msg-search-clear" onclick="setMsgSearch('');document.getElementById('${HtmlIds.MsgSearchInput}')?.focus()" title="${escAttr(I18n.t("messages.searchClear"))}">✕</button>"""
      else ""
    s"""<div class="msg-search-bar">""" +
      s"""<input type="text" id="${HtmlIds.MsgSearchInput}" class="msg-search-input" """ +
      s"""placeholder="${escAttr(I18n.t("messages.searchPlaceholder"))}" value="${escAttr(q)}" """ +
      s"""oninput="setMsgSearch(this.value)" """ +
      s"""oncompositionstart="_imeComposing=true" """ +
      s"""oncompositionend="_imeComposing=false;setMsgSearch(this.value)">""" +
      clearBtn +
      "</div>"
  }

  private def buildBodyParts(
    contents: js.Array[js.Dynamic],
    role: String,
    isUserFilter: Boolean,
    q: String,
  ): List[String] = {
    val parts = scala.collection.mutable.ListBuffer.empty[String]
    for (c <- contents) {
      val cType = c.selectDynamic("type").asInstanceOf[String]
      cType match {
        case "text" =>
          val cText = c.selectDynamic("text")
          val text  = if (!js.isUndefined(cText) && cText != null) cText.toString else ""
          if (role == "user") {
            renderUserMsgContent(text, isUserFilter, q).foreach(parts += _)
          } else {
            parts += s"""<div class="msg-text">${highlightSearch(esc(text), q)}</div>"""
          }
        case "tool_use" =>
          if (!isUserFilter) {
            val name = c.selectDynamic("name")
            val nameStr = if (!js.isUndefined(name) && name != null) name.toString else ""
            parts += s"""<div class="msg-tool">\uD83D\uDD27 ${esc(nameStr)}()</div>"""
          }
        case "tool_result" =>
          if (!isUserFilter) {
            val rc = c.selectDynamic("content")
            val preview =
              if (js.typeOf(rc) == "string") rc.asInstanceOf[String].take(120)
              else if (js.Array.isArray(rc)) {
                rc.asInstanceOf[js.Array[js.Dynamic]]
                  .map { x =>
                    val t = x.selectDynamic("text")
                    if (!js.isUndefined(t) && t != null) t.toString else ""
                  }
                  .mkString
                  .take(120)
              } else "[object]"
            val ellipsis = if (preview.length == 120) "\u2026" else ""
            parts += s"""<div class="msg-tool-result">\uD83D\uDCE4 ${esc(preview)}$ellipsis</div>"""
          }
        case other =>
          parts += s"""<div class="msg-other">[${esc(other)}]</div>"""
      }
    }
    parts.toList
  }

  @JSExportTopLevel("setMsgFilter")
  def setMsgFilter(f: String): Unit = {
    AppState.msgFilter = f
    val entry = AppState.proxyCaptures.find(e => e.id == AppState.selectedProxyId.map(_.asInstanceOf[js.Any]).orNull)
    val detail = dom.document.getElementById(HtmlIds.ProxyDetailView)
    entry.foreach { e =>
      if (detail != null) renderProxyMessages(e, detail.asInstanceOf[dom.html.Element])
    }
  }

  @JSExportTopLevel("setMsgSearch")
  def setMsgSearch(q: String): Unit = {
    AppState.msgSearchQuery = q
    // _imeComposing is set by inline oncompositionstart/end handlers in the HTML
    val imeComposing = dom.window.asInstanceOf[js.Dynamic].selectDynamic("_imeComposing")
    if (!js.isUndefined(imeComposing) && imeComposing.asInstanceOf[Boolean]) return
    msgSearchDebounce { () =>
      AppState.msgSearchWasFocused = dom.document.activeElement match {
        case el: dom.html.Element => el.id == HtmlIds.MsgSearchInput
        case _                    => false
      }
      val entry = AppState.proxyCaptures.find(e => e.id == AppState.selectedProxyId.map(_.asInstanceOf[js.Any]).orNull)
      val detail = dom.document.getElementById(HtmlIds.ProxyDetailView)
      entry.foreach { e =>
        if (detail != null && AppState.proxyDetailTab == "messages") {
          renderProxyMessages(e, detail.asInstanceOf[dom.html.Element])
        }
      }
    }
  }
}
