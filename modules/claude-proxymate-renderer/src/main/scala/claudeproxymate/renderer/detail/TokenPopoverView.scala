package claudeproxymate.renderer.detail

import scalatags.Text.all.*

/** A single row in the token-cost popover. */
final case class TokenPopoverRow(
  label: String,
  tokens: String,
  price: String,
  cost: String,
)

/** Dynamic per-capture data driving the token popover content. */
final case class TokenPopoverData(
  model: String,
  kb: String,
  total: String,
  cachePct: Int,
  pricingDate: String,
  rows: List[TokenPopoverRow],
)

/** Static i18n-sourced chrome labels for the token popover.
  *
  * Resolved once by the caller (usually [[TokenPopover]]'s click handler)
  * before calling the view — keeps the view free of i18n deps and
  * trivially testable.
  *
  * `notePricingDate` should be the i18n template already filled with the
  * pricing-date placeholder. `noteCacheSaving` is `Some` only when the
  * cache-hit rate is high enough that the note should appear.
  */
final case class TokenPopoverLabels(
  costTitle: String,
  modelLabel: String,
  reqSizeLabel: String,
  copyBtn: String,
  totalLabel: String,
  cacheHitRateLabel: String,
  notePricingDate: String,
  noteModelPrice: String,
  noteOfficialDoc: String,
  noteMTok: String,
  noteCacheSaving: Option[String],
)

/** Pure view functions for the token-cost popover. */
object TokenPopoverView {

  val DocsHref: String =
    "https://docs.anthropic.com/en/docs/about-claude/models#model-comparison"

  /** Build the human-readable multi-line text that the Copy button places
    * on the clipboard. Pure function; no DOM access.
    */
  def buildCopyText(
    data: TokenPopoverData,
    labels: TokenPopoverLabels,
  ): String = {
    val builder = scala.collection.mutable.ListBuffer.empty[String]
    builder += s"${labels.modelLabel}: ${data.model}"
    builder += s"${labels.reqSizeLabel}: ${data.kb} KB"
    builder += ""
    for (r <- data.rows) {
      builder += s"${r.label}: ${r.tokens} tok × $$${r.price}/MTok = ${r.cost}"
    }
    builder += ""
    builder += s"${labels.totalLabel}: ${data.total}"
    builder += s"${labels.cacheHitRateLabel}: ${data.cachePct}%"
    builder.mkString("\n")
  }

  /** Build the Scalatags fragment for the popover contents. Pure function.
    * Caller renders this into `innerHTML` via
    * [[claudeproxymate.renderer.view.ViewHelpers.setInnerHtml]].
    */
  def buildPopoverFrag(
    data: TokenPopoverData,
    labels: TokenPopoverLabels,
    descriptions: Map[String, String],
  ): Frag = {
    val textForCopy = buildCopyText(data, labels)

    val rowFrags: List[Frag] = data.rows.flatMap { r =>
      val rowFrag: Frag          = div(cls := "token-popover-row")(
        span(cls := "tp-label")(r.label),
        span(cls := "tp-formula")(s"${r.tokens} tok × $$${r.price}/MTok"),
        span(cls := "tp-result")(r.cost),
      )
      val descFrag: Option[Frag] = descriptions.get(r.label).map { desc =>
        div(cls := "tp-desc")(desc)
      }
      rowFrag :: descFrag.toList
    }

    val noteCacheSavingFrag: Frag = labels.noteCacheSaving match {
      case Some(s) => frag(br, s)
      case None => frag()
    }

    frag(
      div(cls := "token-popover-title")(
        span(labels.costTitle),
        span(
          cls := "token-popover-copy",
          attr("data-text") := textForCopy,
        )(labels.copyBtn),
      ),
      div(cls := "token-popover-info")(
        s"${labels.modelLabel}: ${data.model} · ${labels.reqSizeLabel}: ${data.kb} KB",
      ),
      frag(rowFrags),
      div(cls := "token-popover-row tp-total")(
        span(cls := "tp-label")(labels.totalLabel),
        span(cls := "tp-result")(data.total),
      ),
      div(cls := "token-popover-row tp-total")(
        span(cls := "tp-label")(labels.cacheHitRateLabel),
        span(cls := "tp-result")(s"${data.cachePct}%"),
      ),
      div(cls := "token-popover-note")(
        labels.notePricingDate,
        br,
        frag(
          labels.noteModelPrice,
          " (",
          a(
            href := DocsHref,
            cls := TokenPopoverLinks.ExternalLinkClass,
            target := "_blank",
            rel := "noopener",
            style := "color:var(--blue);text-decoration:underline;cursor:pointer",
          )(labels.noteOfficialDoc),
          ")",
        ),
        br,
        labels.noteMTok,
        noteCacheSavingFrag,
      ),
    )
  }
}
