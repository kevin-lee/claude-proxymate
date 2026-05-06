package claudeproxymate.renderer.messages

import claudeproxymate.renderer.util.HtmlUtil
import scalatags.Text.all.*

/** A parsed user-text part, ready for rendering. Mirrors
  * [[MessageParser.Part]] but with a pre-assigned UID for injected blocks.
  */
sealed trait MsgPart
final case class TextMsgPart(content: String) extends MsgPart
final case class InjectedMsgPart(uid: String, label: String, content: String, badgeCls: String) extends MsgPart

/** A typed message-content block. */
sealed trait MsgContent
final case class TextContent(text: String) extends MsgContent
final case class ToolUseContent(name: String) extends MsgContent
final case class ToolResultContent(preview: String, truncated: Boolean) extends MsgContent
final case class OtherContent(typeName: String) extends MsgContent

/** One message card: role, the raw content blocks, and (only when role is
  * "user") the parsed user-text parts the orchestrator already produced.
  */
final case class MsgCard(role: String, contents: List[MsgContent], userParts: List[MsgPart])

final case class FilterLabels(user: String, typed: String, assistant: String, all: String)
final case class SearchLabels(placeholder: String, clear: String)

/** Pure view for the messages tab. */
object MessageView {

  val FilterButtonClass: String = "mf-btn"
  val FilterDataAttr: String    = "data-msg-filter"

  val SearchInputClass: String = "msg-search-input"
  val SearchClearClass: String = "msg-search-clear"

  val BadgeClass: String    = "msg-badge"
  val BadgeDataAttr: String = "data-msg-badge-uid"

  def buildEmptyFrag(noMessagesLabel: String): Frag =
    div(cls := "proxy-empty")(span(noMessagesLabel))

  def buildNoResultsFrag(noResultsLabel: String): Frag =
    div(cls := "proxy-empty")(span(noResultsLabel))

  def buildHeaderFrag(
    activeFilter: String,
    filterLabels: FilterLabels,
    msgCountId: String,
    searchInputId: String,
    searchLabels: SearchLabels,
    query: String,
  ): Frag = {
    def filterBtn(key: String, label: String): Frag = {
      val activeSuffix = if (activeFilter == key) " active" else ""
      button(
        cls                := s"$FilterButtonClass$activeSuffix",
        attr(FilterDataAttr) := key,
      )(label)
    }
    frag(
      div(cls := "msg-filter")(
        filterBtn("user", filterLabels.user),
        filterBtn("typed", filterLabels.typed),
        filterBtn("assistant", filterLabels.assistant),
        filterBtn("all", filterLabels.all),
        span(cls := "msg-count", id := msgCountId),
      ),
      div(cls := "msg-search-bar")(
        input(
          tpe         := "text",
          id          := searchInputId,
          cls         := SearchInputClass,
          placeholder := searchLabels.placeholder,
          value       := query,
        ),
        if (query.nonEmpty)
          button(
            cls           := SearchClearClass,
            attr("title") := searchLabels.clear,
          )("✕")
        else frag(),
      ),
    )
  }

  def buildCardsFrag(cards: List[MsgCard], isUserFilter: Boolean, query: String): Frag =
    frag(cards.map(c => buildCardFrag(c, isUserFilter, query)))

  private def buildCardFrag(card: MsgCard, isUserFilter: Boolean, query: String): Frag =
    div(cls := s"msg-card msg-${card.role}")(
      div(cls := "msg-role")(card.role),
      div(cls := "msg-body")(buildBodyFrag(card, isUserFilter, query)),
    )

  private def buildBodyFrag(card: MsgCard, isUserFilter: Boolean, query: String): Frag = {
    if (card.role == "user" && card.userParts.nonEmpty) {
      frag(card.userParts.map(p => buildUserPartFrag(p, query)))
    } else {
      frag(card.contents.map(c => buildContentFrag(c, isUserFilter, query)))
    }
  }

  private def buildUserPartFrag(p: MsgPart, query: String): Frag = p match {
    case TextMsgPart(content) =>
      div(cls := "msg-typed")(HtmlUtil.highlightSearchFrag(content, query))

    case InjectedMsgPart(uid, label, content, badgeCls) =>
      // Auto-expand the badge when the search query matches inside the
      // collapsed content. Without this, search hits inside system-reminders /
      // skills / etc. are wrapped in <mark> but invisible to the user because
      // the parent is display:none.
      val matched      = query.nonEmpty && content.toLowerCase.contains(query.toLowerCase)
      val badgeClasses =
        if (matched) s"$BadgeClass $badgeCls expandable open hl-active"
        else s"$BadgeClass $badgeCls expandable"
      val contentStyle =
        if (matched) "display:block"
        else "display:none"
      val contentClasses =
        if (matched) "badge-expand-content badge-section-hl"
        else "badge-expand-content"
      div(cls := "msg-injected-row")(
        span(
          id                  := s"bb_$uid",
          cls                 := badgeClasses,
          attr(BadgeDataAttr) := uid,
        )(label),
        div(
          id    := s"bc_$uid",
          cls   := contentClasses,
          style := contentStyle,
        )(HtmlUtil.highlightSearchFrag(content, query)),
      )
  }

  private def buildContentFrag(c: MsgContent, isUserFilter: Boolean, query: String): Frag = c match {
    case TextContent(text) =>
      div(cls := "msg-text")(HtmlUtil.highlightSearchFrag(text, query))

    case ToolUseContent(name) =>
      if (isUserFilter) frag()
      else div(cls := "msg-tool")(s"🔧 $name()")

    case ToolResultContent(preview, truncated) =>
      if (isUserFilter) frag()
      else {
        val ellipsis = if (truncated) "…" else ""
        div(cls := "msg-tool-result")(s"📤 $preview$ellipsis")
      }

    case OtherContent(typeName) =>
      div(cls := "msg-other")(s"[$typeName]")
  }
}
