package claudeproxymate.renderer.messages

import cats.syntax.all.*
import claudeproxymate.core.RequestAnatomy
import claudeproxymate.core.filter.{CategoryMode, FilterConfig, SkillsList, TextRules}
import claudeproxymate.renderer.filter.{MessageFilterView, RequestFilterView}
import scalatags.Text.all.*

/** A parsed user-text part, ready for rendering. Mirrors
  * [[MessageParser.Part]] but with a pre-assigned UID for injected blocks.
  *
  * The `uid` is used for badge DOM ids only (`bb_…` / `bc_…` /
  * `data-msg-badge-uid`). It is minted from a global counter on every
  * parse, so it must NOT participate in mask ids — those use the
  * deterministic `partIdx` so reveal state survives re-parses and the
  * Copy path can reconstruct the same ids.
  */
enum MsgPart {
  case TextMsgPart(content: String)
  case InjectedMsgPart(
    uid: String,
    label: String,
    content: String,
    badgeCls: String,
    filter: Option[BadgeFilter] = None,
  )
}

/** Whether the saved config removes a badge: not at all, by key (`RemoveSelected`)
  * or because its whole category is `RemoveAll`.
  */
enum BadgeState {
  case Live
  case RemovedSelected
  case RemovedAll
}

object BadgeState {
  given cats.Eq[BadgeState] = cats.Eq.fromUniversalEquals
}

/** A typed message-content block. */
enum MsgContent {
  case TextContent(text: String)
  case ToolUseContent(name: String)
  case ToolResultContent(preview: String, truncated: Boolean)
  case OtherContent(typeName: String)
}

/** One message card: role, the raw content blocks, and (only when role is
  * "user") the parsed user-text parts the orchestrator already produced.
  *
  * `rawIdx` is the position of this message in the raw
  * `body.messages` array, with the capture's response message (when
  * present) appended one past the end (NOT the filtered view) — used
  * as part of the stable token-mask id prefix so reveal state
  * survives filter changes and re-renders. Stable because the request
  * messages are fixed per capture, so a late-arriving response never
  * shifts existing indices.
  */
final case class MsgCard(
  role: String,
  contents: List[MsgContent],
  userParts: List[MsgPart],
  rawIdx: Int,
  removed: List[RemovedMark],
)

/** Something the request filter removed from this message before it was
  * forwarded: shown as a struck-through ghost badge so the user can see the
  * filter acting. `label` is the same text the live badge would have shown.
  */
final case class RemovedMark(label: String, tokens: Int)

final case class FilterLabels(user: String, typed: String, assistant: String, all: String)

/** Labels for the request-filter ghost rows and the per-skill ✕ buttons;
  * `removed` carries a `{tokens}` placeholder.
  */
final case class GhostLabels(removed: String, skillRemove: String, skillRestore: String)

object GhostLabels {
  val default: GhostLabels = GhostLabels(
    "removed · ~{tokens} tok",
    "Remove this skill from future requests",
    "Keep this skill in future requests",
  )
}
final case class SearchLabels(placeholder: String, clear: String)

/** Pure view for the messages tab. */
object MessageView {

  import MsgContent.*
  import MsgPart.*

  val FilterButtonClass: String = "mf-btn"
  val FilterDataAttr: String    = "data-msg-filter"

  val SearchInputClass: String = "msg-search-input"
  val SearchClearClass: String = "msg-search-clear"

  val BadgeClass: String    = "msg-badge"
  val BadgeDataAttr: String = "data-msg-badge-uid"

  /** Stable `<cardIdx>.<partIdx>` of a badge (uids are re-minted per render). */
  val BadgePartAttr: String = "data-msg-badge-part"

  /** `item` or `skills` on badges the request filter can act on. */
  val BadgeFilterAttr: String   = "data-msg-badge-filter"
  val BadgeFilterItem: String   = "item"
  val BadgeFilterSkills: String = "skills"

  val SkillEntryClass: String        = "skill-entry"
  val SkillEntryRemovedClass: String = "removed"
  val SkillEntryTextClass: String    = "skill-entry-text"
  val SkillXClass: String            = "skill-x"

  val GhostClass: String     = "filter-ghost"
  val GhostMetaClass: String = "filter-ghost-meta"

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
      val activeSuffix = if (activeFilter === key) " active" else ""
      button(
        cls := s"$FilterButtonClass$activeSuffix",
        attr(FilterDataAttr) := key,
      )(label)
    }
    frag(
      div(cls := "msg-filter")(
        filterBtn("all", filterLabels.all),
        filterBtn("user", filterLabels.user),
        filterBtn("typed", filterLabels.typed),
        filterBtn("assistant", filterLabels.assistant),
        span(cls := "msg-count", id := msgCountId),
      ),
      div(cls := "msg-search-bar")(
        input(
          tpe := "text",
          id := searchInputId,
          cls := SearchInputClass,
          placeholder := searchLabels.placeholder,
          value := query,
        ),
        if (query.nonEmpty)
          button(
            cls := SearchClearClass,
            attr("title") := searchLabels.clear,
          )("✕")
        else frag(),
      ),
    )
  }

  def buildCardsFrag(cards: List[MsgCard], isUserFilter: Boolean, query: String): Frag =
    buildCardsFrag(cards, isUserFilter, query, GhostLabels.default)

  def buildCardsFrag(cards: List[MsgCard], isUserFilter: Boolean, query: String, ghostLabels: GhostLabels): Frag =
    buildCardsFrag(cards, isUserFilter, query, ghostLabels, FilterConfig.default)

  def buildCardsFrag(
    cards: List[MsgCard],
    isUserFilter: Boolean,
    query: String,
    ghostLabels: GhostLabels,
    config: FilterConfig,
  ): Frag =
    frag(cards.map(c => buildCardFrag(c, isUserFilter, query, ghostLabels, config)))

  /** What the saved config does to a badge. Nothing is struck through while
    * filtering is switched off, since the filter will not act.
    */
  def badgeState(config: FilterConfig, filter: BadgeFilter): BadgeState =
    if (!config.enabled) BadgeState.Live
    else
      filter match {
        case BadgeFilter.Item(category, key) =>
          val cf = config.category(category)
          if (cf.mode === CategoryMode.RemoveAll) BadgeState.RemovedAll
          else if (cf.mode === CategoryMode.RemoveSelected && cf.keys.contains(key)) BadgeState.RemovedSelected
          else BadgeState.Live
        case BadgeFilter.SkillsReminder =>
          if (config.skills.mode === CategoryMode.RemoveAll) BadgeState.RemovedAll else BadgeState.Live
      }

  private def buildCardFrag(
    card: MsgCard,
    isUserFilter: Boolean,
    query: String,
    ghostLabels: GhostLabels,
    config: FilterConfig,
  ): Frag =
    div(cls := s"msg-card msg-${card.role}")(
      div(cls := "msg-role")(card.role),
      div(cls := "msg-body")(
        frag(card.removed.map(m => buildGhostRowFrag(m, ghostLabels))),
        buildBodyFrag(card, isUserFilter, query, ghostLabels, config),
      ),
    )

  /** A removed item: struck-through badge plus a short "removed · ~N tok" note. */
  private def buildGhostRowFrag(mark: RemovedMark, ghostLabels: GhostLabels): Frag =
    div(cls := "msg-injected-row")(
      span(cls := s"$BadgeClass green $GhostClass")(mark.label),
      span(cls := GhostMetaClass)(ghostLabels.removed.replace("{tokens}", mark.tokens.toString)),
    )

  private def buildBodyFrag(
    card: MsgCard,
    isUserFilter: Boolean,
    query: String,
    ghostLabels: GhostLabels,
    config: FilterConfig,
  ): Frag = {
    if (card.role === "user" && card.userParts.nonEmpty) {
      frag(card.userParts.zipWithIndex.map {
        case (p, idx) => buildUserPartFrag(p, query, card.rawIdx, idx, ghostLabels, config)
      })
    } else {
      frag(
        card.contents.zipWithIndex.map { case (c, idx) => buildContentFrag(c, isUserFilter, query, card.rawIdx, idx) }
      )
    }
  }

  private def buildUserPartFrag(
    p: MsgPart,
    query: String,
    cardIdx: Int,
    partIdx: Int,
    ghostLabels: GhostLabels,
    config: FilterConfig,
  ): Frag = p match {
    case TextMsgPart(content) =>
      val idPrefix = s"m.$cardIdx.user.$partIdx"
      div(cls := "msg-typed")(MessageTokenView.buildTextFrag(content, query, idPrefix))

    case InjectedMsgPart(uid, label, content, badgeCls, filter) =>
      // Auto-expand the badge when the search query matches inside the
      // collapsed content. Without this, search hits inside system-reminders /
      // skills / etc. are wrapped in <mark> but invisible to the user because
      // the parent is display:none.
      val matched                    = query.nonEmpty && content.toLowerCase.contains(query.toLowerCase)
      val state                      = filter.fold(BadgeState.Live)(f => badgeState(config, f))
      val ghost                      = if (state === BadgeState.Live) "" else s" $GhostClass"
      val badgeClasses               =
        if (matched) s"$BadgeClass $badgeCls expandable open hl-active$ghost"
        else s"$BadgeClass $badgeCls expandable$ghost"
      val contentStyle               =
        if (matched) "display:block"
        else "display:none"
      val contentClasses             =
        if (matched) "badge-expand-content badge-section-hl"
        else "badge-expand-content"
      val idPrefix                   = s"m.$cardIdx.inj.$partIdx"
      val filterAttrs: Seq[Modifier] = filter match {
        case Some(BadgeFilter.Item(category, key)) =>
          Seq(
            attr(BadgeFilterAttr) := BadgeFilterItem,
            attr(RequestFilterView.CatAttr) := category.wire,
            attr(RequestFilterView.KeyAttr) := key,
          )
        case Some(BadgeFilter.SkillsReminder) =>
          Seq(attr(BadgeFilterAttr) := BadgeFilterSkills, attr(RequestFilterView.CatAttr) := "skills")
        case None => Seq.empty
      }
      val meta                       =
        if (state === BadgeState.Live) frag()
        else {
          val tokens = RequestAnatomy.estTokens(TextRules.byteLen(content))
          span(cls := GhostMetaClass)(ghostLabels.removed.replace("{tokens}", tokens.toString))
        }
      val body                       = filter match {
        case Some(BadgeFilter.SkillsReminder) => buildSkillsContentFrag(content, query, idPrefix, ghostLabels, config)
        case Some(BadgeFilter.Item(_, _)) | None => MessageTokenView.buildTextFrag(content, query, idPrefix)
      }
      div(cls := "msg-injected-row")(
        span(
          id := s"bb_$uid",
          cls := badgeClasses,
          attr(BadgeDataAttr) := uid,
          attr(BadgePartAttr) := s"$cardIdx.$partIdx",
          filterAttrs,
        )(label),
        meta,
        div(
          id := s"bc_$uid",
          cls := contentClasses,
          style := contentStyle,
        )(body),
      )
  }

  /** The skills reminder rendered entry by entry, each with a ✕ toggle. Mask
    * ids stay those of the whole text thanks to the `offset` argument.
    */
  private def buildSkillsContentFrag(
    content: String,
    query: String,
    idPrefix: String,
    ghostLabels: GhostLabels,
    config: FilterConfig,
  ): Frag = {
    val entries = SkillsList.entries(content)
    entries.headOption match {
      case None => MessageTokenView.buildTextFrag(content, query, idPrefix)
      case Some(first) =>
        val head    = MessageTokenView.buildTextFrag(content.substring(0, first.start), query, idPrefix)
        val rows    = entries.map { e =>
          val removed = config.enabled && config.skills.remove(e.name)
          div(
            cls := (if (removed) s"$SkillEntryClass $SkillEntryRemovedClass" else SkillEntryClass),
            attr(RequestFilterView.KeyAttr) := e.name,
          )(
            button(
              cls := (if (removed) s"x-btn $SkillXClass on" else s"x-btn $SkillXClass"),
              attr(MessageFilterView.ActionAttr) := MessageFilterView.Action.ToggleSkill,
              attr(RequestFilterView.KeyAttr) := e.name,
              attr("aria-label") := (if (removed) ghostLabels.skillRestore else ghostLabels.skillRemove),
              attr("title") := (if (removed) ghostLabels.skillRestore else ghostLabels.skillRemove),
            )("✕"),
            span(cls := SkillEntryTextClass)(
              MessageTokenView.buildTextFrag(content.substring(e.start, e.end), query, idPrefix, e.start)
            ),
          )
        }
        val lastEnd = entries.last.end
        val tail    =
          if (lastEnd < content.length)
            MessageTokenView.buildTextFrag(content.substring(lastEnd), query, idPrefix, lastEnd)
          else frag()
        frag(head, rows, tail)
    }
  }

  private def buildContentFrag(c: MsgContent, isUserFilter: Boolean, query: String, cardIdx: Int, partIdx: Int): Frag =
    c match {
      case TextContent(text) =>
        val idPrefix = s"m.$cardIdx.text.$partIdx"
        div(cls := "msg-text")(MessageTokenView.buildTextFrag(text, query, idPrefix))

      case ToolUseContent(name) =>
        if (isUserFilter) frag()
        else div(cls := "msg-tool")(s"🔧 $name()")

      case ToolResultContent(preview, truncated) =>
        if (isUserFilter) frag()
        else {
          val ellipsis = if (truncated) "…" else ""
          val idPrefix = s"m.$cardIdx.tr.$partIdx"
          div(cls := "msg-tool-result")(
            "📤 ",
            MessageTokenView.buildTextFrag(preview, query, idPrefix),
            ellipsis,
          )
        }

      case OtherContent(typeName) =>
        div(cls := "msg-other")(s"[$typeName]")
    }
}
