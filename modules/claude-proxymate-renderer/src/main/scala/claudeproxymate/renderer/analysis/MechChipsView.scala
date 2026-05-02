package claudeproxymate.renderer.analysis

import scalatags.Text.all.*

/** One mechanism chip, ready for rendering. */
final case class Chip(
  key: String,     // unique identifier, used as data-key for click dispatch
  label: String,   // visible text (icon + name)
  cls: String,     // CSS subclass (cm/sc/sk/sa/mc/st)
  metaKey: String, // key into ChipMeta map for the desc banner
)

/** Static metadata for a chip's description banner. */
final case class ChipMeta(color: String, who: String, what: String)

/** Pure view for the mechanism filter chip row + optional description
  * banner. The orchestrator [[MechChips]] is responsible for building the
  * `List[Chip]` from a request body and resolving the `ChipMeta` for the
  * active chip; this view just renders.
  */
object MechChipsView {

  /** CSS class + attribute the document-level click handler in
    * [[MechChips]] uses to identify chip clicks. Constants are exposed so
    * the view and the handler share a single source of truth.
    */
  val ChipClass: String    = "mech-chip"
  val ChipDataAttr: String = "data-key"

  def buildChipsFrag(
    chips: List[Chip],
    activeKey: Option[String],
    descMeta: Option[ChipMeta],
  ): Frag =
    if (chips.isEmpty) frag()
    else
      div(style := "flex-shrink:0;border-bottom:1px solid var(--border)")(
        div(style := "display:flex;flex-wrap:wrap;gap:5px;padding:5px 10px 7px")(
          frag(chips.map(c => buildChipFrag(c, activeKey)))
        ),
        descMeta match {
          case Some(m) => buildDescBanner(m)
          case None    => frag()
        },
      )

  private def buildChipFrag(c: Chip, activeKey: Option[String]): Frag = {
    val activeSuffix = if (activeKey.contains(c.key)) " active" else ""
    val classes      = s"$ChipClass ${c.cls} found btn$activeSuffix"
    span(
      cls                := classes,
      attr(ChipDataAttr) := c.key,
    )(c.label)
  }

  private def buildDescBanner(m: ChipMeta): Frag =
    div(cls := "mech-filter-desc")(
      span(cls := "mech-filter-desc-dot", style := s"background:${m.color}"),
      div(cls := "mech-filter-desc-body")(
        span(cls := "mech-filter-desc-who")(m.who),
        span(cls := "mech-filter-desc-what")(m.what),
      ),
    )
}
