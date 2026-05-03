package claudeproxymate.renderer.messages

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.Debounce
import claudeproxymate.renderer.view.ViewHelpers
import org.scalajs.dom
import scalatags.Text.all.*

import scala.scalajs.js

/** Render proxy messages tab: filter, search, message cards.
  *
  * Owns the document-level click / input / composition listeners that drive
  * filter button toggles, search input changes, IME composition tracking,
  * and badge toggles. The HTML building lives in [[MessageView]].
  */
object MessageRenderer {

  private val msgSearchDebounce = new Debounce(150)

  /** Install the document-level listeners. Called from RendererMain. */
  def install(): Unit = {
    dom.document.addEventListener("click", handleClick _)
    dom.document.addEventListener("input", handleInput _)
    dom.document.addEventListener("compositionstart", handleCompositionStart _)
    dom.document.addEventListener("compositionend", handleCompositionEnd _)
  }

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return

    val filterEl = target.closest(s".${MessageView.FilterButtonClass}[${MessageView.FilterDataAttr}]")
    if (filterEl != null) {
      val key = filterEl.asInstanceOf[dom.html.Element].getAttribute(MessageView.FilterDataAttr)
      if (key != null && key.nonEmpty) { setMsgFilter(key); return }
    }

    if (target.closest(s".${MessageView.SearchClearClass}") != null) {
      setMsgSearch("")
      val inp = dom.document.getElementById(HtmlIds.MsgSearchInput)
      if (inp != null) inp.asInstanceOf[dom.html.Input].focus()
      return
    }

    val badgeEl = target.closest(s".${MessageView.BadgeClass}[${MessageView.BadgeDataAttr}]")
    if (badgeEl != null) {
      val uid = badgeEl.asInstanceOf[dom.html.Element].getAttribute(MessageView.BadgeDataAttr)
      if (uid != null && uid.nonEmpty) BadgeToggle.toggleBadge(uid)
    }
  }

  private def handleInput(e: dom.Event): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null || target.id != HtmlIds.MsgSearchInput) return
    setMsgSearch(target.asInstanceOf[dom.html.Input].value)
  }

  private def handleCompositionStart(e: dom.Event): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null || target.id != HtmlIds.MsgSearchInput) return
    // The IME flag stays on the window global until A3g/A3h migrate the
    // other two search inputs; sharing one source of truth is required by
    // SearchNavigation.
    dom.window.asInstanceOf[js.Dynamic]._imeComposing = true
  }

  private def handleCompositionEnd(e: dom.Event): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null || target.id != HtmlIds.MsgSearchInput) return
    dom.window.asInstanceOf[js.Dynamic]._imeComposing = false
    setMsgSearch(target.asInstanceOf[dom.html.Input].value)
  }

  private def nextBadgeUid(): String = {
    AppState.badgeUidCounter += 1
    s"b${AppState.badgeUidCounter}"
  }

  def renderProxyMessages(entry: js.Dynamic, container: dom.html.Element): Unit = {
    val body = entry.selectDynamic("body")
    val msgs: js.Array[js.Dynamic] =
      if (!js.isUndefined(body) && body != null) {
        val m = body.selectDynamic("messages")
        if (!js.isUndefined(m) && m != null) m.asInstanceOf[js.Array[js.Dynamic]]
        else js.Array[js.Dynamic]()
      } else js.Array[js.Dynamic]()

    if (msgs.length == 0) {
      ViewHelpers.setInnerHtml(container, MessageView.buildEmptyFrag(I18n.t("proxy.noMessages")))
      return
    }

    val typedOnly    = AppState.msgFilter == "typed"
    val isUserFilter = AppState.msgFilter == "user" || typedOnly
    val q            = AppState.msgSearchQuery

    val baseFiltered: js.Array[js.Dynamic] =
      if (isUserFilter) msgs.filter(m => m.role.asInstanceOf[String] == "user")
      else if (AppState.msgFilter == "assistant") msgs.filter(m => m.role.asInstanceOf[String] == "assistant")
      else msgs

    val cards = scala.collection.mutable.ListBuffer.empty[MsgCard]
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
        val card = buildCard(role, contents, typedOnly)
        if (cardIsNonEmpty(card) && cardMatchesQuery(card, q)) cards += card
      }
    }

    container.style.cssText = "flex:1;overflow-y:auto;display:block"

    val filterLabels = FilterLabels(
      user      = I18n.t("messages.filterUser"),
      typed     = I18n.t("messages.filterTyped"),
      assistant = I18n.t("messages.filterAssistant"),
      all       = I18n.t("messages.filterAll"),
    )
    val searchLabels = SearchLabels(
      placeholder = I18n.t("messages.searchPlaceholder"),
      clear       = I18n.t("messages.searchClear"),
    )

    val header = MessageView.buildHeaderFrag(
      activeFilter  = AppState.msgFilter,
      filterLabels  = filterLabels,
      msgCountId    = HtmlIds.MsgCountEl,
      searchInputId = HtmlIds.MsgSearchInput,
      searchLabels  = searchLabels,
      query         = q,
    )

    val body2: Frag =
      if (cards.isEmpty) MessageView.buildNoResultsFrag(I18n.t("proxy.noResults"))
      else MessageView.buildCardsFrag(cards.toList, isUserFilter, q)

    val full = frag(
      div(style := "position:sticky;top:0;z-index:1;background:var(--bg)")(header),
      div(style := "display:flex;flex-direction:column;gap:8px;padding:12px")(body2),
    )
    ViewHelpers.setInnerHtml(container, full)

    val countEl = dom.document.getElementById(HtmlIds.MsgCountEl)
    if (countEl != null) {
      countEl.textContent = I18n.t(
        "proxy.msgCount",
        Map("count" -> cards.length.toString, "total" -> msgs.length.toString),
      )
    }

    val inp = dom.document.getElementById(HtmlIds.MsgSearchInput)
    if (inp != null && (q.nonEmpty || AppState.msgSearchWasFocused)) {
      val inputEl = inp.asInstanceOf[dom.html.Input]
      inputEl.focus()
      val len = inputEl.value.length
      inputEl.setSelectionRange(len, len)
    }
  }

  private def buildCard(
    role: String,
    contents: js.Array[js.Dynamic],
    typedOnly: Boolean,
  ): MsgCard = {
    if (role == "user") {
      val parts = scala.collection.mutable.ListBuffer.empty[MsgPart]
      for (c <- contents) {
        val cType = c.selectDynamic("type").asInstanceOf[String]
        if (cType == "text") {
          val cText = c.selectDynamic("text")
          val text  = if (!js.isUndefined(cText) && cText != null) cText.toString else ""
          val parsed = MessageParser.parseUserText(text)
          if (typedOnly) {
            parsed.foreach {
              case MessageParser.TextPart(content) => parts += TextMsgPart(content)
              case _: MessageParser.InjectedPart   => ()
            }
          } else {
            parsed.foreach {
              case MessageParser.TextPart(content) => parts += TextMsgPart(content)
              case MessageParser.InjectedPart(label, content, cls) =>
                parts += InjectedMsgPart(nextBadgeUid(), label, content, cls)
            }
          }
        }
      }
      MsgCard(role, contents = Nil, userParts = parts.toList)
    } else {
      val msgContents = contents.toList.map { c =>
        val cType = c.selectDynamic("type").asInstanceOf[String]
        cType match {
          case "text" =>
            val cText = c.selectDynamic("text")
            val text  = if (!js.isUndefined(cText) && cText != null) cText.toString else ""
            TextContent(text)
          case "tool_use" =>
            val name = c.selectDynamic("name")
            val nameStr = if (!js.isUndefined(name) && name != null) name.toString else ""
            ToolUseContent(nameStr)
          case "tool_result" =>
            val rc = c.selectDynamic("content")
            val raw =
              if (js.typeOf(rc) == "string") rc.asInstanceOf[String]
              else if (js.Array.isArray(rc))
                rc.asInstanceOf[js.Array[js.Dynamic]]
                  .map { x =>
                    val t = x.selectDynamic("text")
                    if (!js.isUndefined(t) && t != null) t.toString else ""
                  }
                  .mkString
              else "[object]"
            val preview   = raw.take(120)
            val truncated = raw.length > 120
            ToolResultContent(preview, truncated)
          case other =>
            OtherContent(other)
        }
      }
      MsgCard(role, contents = msgContents, userParts = Nil)
    }
  }

  private def cardIsNonEmpty(card: MsgCard): Boolean =
    card.userParts.nonEmpty || card.contents.nonEmpty

  /** Approximate the original "rendered HTML contains a match" filter by
    * checking whether any text content of the card matches the query.
    */
  private def cardMatchesQuery(card: MsgCard, query: String): Boolean = {
    if (query.isEmpty) return true
    val q = query.toLowerCase
    def textOf(content: MsgContent): String = content match {
      case TextContent(t)              => t
      case ToolUseContent(n)           => n
      case ToolResultContent(p, _)     => p
      case OtherContent(t)             => t
    }
    def textOfPart(p: MsgPart): String = p match {
      case TextMsgPart(c)                => c
      case InjectedMsgPart(_, l, c, _)   => l + " " + c
    }
    card.userParts.exists(p => textOfPart(p).toLowerCase.contains(q)) ||
    card.contents.exists(c => textOf(c).toLowerCase.contains(q))
  }

  def setMsgFilter(f: String): Unit = {
    AppState.msgFilter = f
    val entry = AppState.proxyCaptures.find(e => e.id == AppState.selectedProxyId.map(_.asInstanceOf[js.Any]).orNull)
    val detail = dom.document.getElementById(HtmlIds.ProxyDetailView)
    entry.foreach { e =>
      if (detail != null) renderProxyMessages(e, detail.asInstanceOf[dom.html.Element])
    }
  }

  def setMsgSearch(q: String): Unit = {
    AppState.msgSearchQuery = q
    // _imeComposing global is shared with AnalysisRenderer/DetailView until
    // A3g/A3h migrate; reading the global keeps the three panels in sync.
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
