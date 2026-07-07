package claudeproxymate.renderer.detail

import scalatags.Text.all.*

/** One badge inside the token pill. `color` is an optional CSS-var-style
  * value (e.g. `"var(--green)"`); `None` means the default colour from
  * `.tt-badge` styling.
  */
final case class TokenBadge(text: String, color: Option[String])

/** The clickable token pill (Request tab). `dataCost` is the JSON string
  * the orchestrator pre-builds; `TokenPopover.handleClick` reads it via
  * `data-cost`.
  */
final case class TokenPill(badges: List[TokenBadge], dataCost: String)

final case class SearchBarLabels(
  placeholder: String,
  clear: String,
  searchPrev: String,
  searchNext: String,
)

/** Pure view for the proxy detail panel header (search bar + optional
  * token pill).
  */
object DetailHeaderView {

  val SearchInputClass: String   = "msg-search-input"
  val SearchClearClass: String   = "msg-search-clear"
  val SearchNavPrevClass: String = "search-nav-prev"
  val SearchNavNextClass: String = "search-nav-next"

  def buildSearchBarFrag(
    searchInputId: String,
    searchCounterId: String,
    query: String,
    labels: SearchBarLabels,
  ): Frag =
    div(cls := "msg-search-bar", style := "flex-shrink:0")(
      input(
        tpe := "text",
        cls := SearchInputClass,
        id := searchInputId,
        placeholder := labels.placeholder,
        value := query,
      ),
      if (query.nonEmpty)
        frag(
          div(cls := "search-nav")(
            span(id := searchCounterId),
            button(cls := s"search-nav-btn $SearchNavPrevClass", attr("title") := labels.searchPrev)("▲"),
            button(cls := s"search-nav-btn $SearchNavNextClass", attr("title") := labels.searchNext)("▼"),
          ),
          button(cls := SearchClearClass, attr("title") := labels.clear)("✕"),
        )
      else frag(),
    )

  def buildTokenPillFrag(pill: TokenPill): Frag =
    div(cls := "proxy-token-pill", attr("data-cost") := pill.dataCost)(
      frag(pill.badges.map(buildBadgeFrag)),
    )

  private def buildBadgeFrag(b: TokenBadge): Frag =
    b.color match {
      case Some(c) => span(cls := "tt-badge", style := s"color:$c")(b.text)
      case None => span(cls := "tt-badge")(b.text)
    }
}
