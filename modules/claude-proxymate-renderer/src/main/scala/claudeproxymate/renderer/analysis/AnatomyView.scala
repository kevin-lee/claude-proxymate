package claudeproxymate.renderer.analysis

import cats.syntax.all.*
import claudeproxymate.core.{AnomalyKind, RequestAnatomy, StructureFacts}
import scalatags.Text.all.*

/** Display-resolved segment row (label already i18n-resolved). */
final case class SegmentRow(label: String, estTokens: Int, pct: Int)

/** Display-resolved inventory row. */
final case class InventoryDisplay(label: String, count: Int, estTokens: Int)

/** Display-resolved anomaly (message already i18n-resolved + interpolated). */
final case class AnomalyDisplay(kind: AnomalyKind, message: String)

final case class AnatomyLabels(
  costTitle: String,
  attributionTitle: String,
  structureTitle: String,
  inventoryTitle: String,
  anomaliesTitle: String,
  estimatedTag: String,
  noResponseNote: String,
  lblMessages: String,
  lblTurns: String,
  lblSystemBlocks: String,
  lblTools: String,
  lblToolUse: String,
  lblToolResult: String,
  lblImages: String,
  lblThinking: String,
  lblStream: String,
)

/** Pure Scalatags view for the Request Anatomy dashboard. No DOM, no inline
  * handlers; all dynamic strings auto-escaped by Scalatags text nodes.
  */
object AnatomyView {

  val CardClass: String = "anatomy-card"
  val BarClass: String  = "anatomy-bar"

  def buildFrag(
    cost: CostCard,
    anatomy: RequestAnatomy,
    segments: List[SegmentRow],
    inventory: List[InventoryDisplay],
    anomalies: List[AnomalyDisplay],
    labels: AnatomyLabels,
  ): Frag =
    frag(
      buildCostCard(cost, labels),
      buildAttributionCard(segments, labels),
      buildStructureCard(anatomy.structure, labels),
      buildInventoryCard(inventory, labels),
      buildAnomalyStrip(anomalies, labels),
    )

  private def cardTitle(text: String): Frag =
    div(cls := "anatomy-card-title")(text)

  private def kv(k: String, v: String): Frag =
    div(cls := "anatomy-row")(span(cls := "ak")(k), span(cls := "av")(v))

  private def buildCostCard(cost: CostCard, labels: AnatomyLabels): Frag =
    div(cls := CardClass)(
      cardTitle(labels.costTitle),
      kv("model", if (cost.model.nonEmpty) cost.model else "?"),
      kv("request size", s"${cost.reqKb} KB"),
      if (cost.estimated)
        frag(
          kv(s"~tokens (${labels.estimatedTag})", cost.estTokens.toString),
          div(cls := "anatomy-note")(labels.noResponseNote),
        )
      else {
        val totalTok = math.max(1, cost.totalInputTokens + cost.outputTokens)
        frag(
          div(cls := BarClass)(
            cost.segments.map { s =>
              span(style := s"width:${s.tokens.toDouble / totalTok * 100}%;background:${s.color}")
            },
          ),
          frag(cost.segments.map { s =>
            div(cls := "anatomy-row")(
              span(cls := "ak")(
                span(cls := "anatomy-dot", style := s"background:${s.color}"),
                s.label,
              ),
              span(cls := "av")(s"${fmtTok(s.tokens)} · ${fmtCost(s.costUsd)}"),
            )
          }),
          div(cls := "anatomy-row anatomy-row-strong")(
            span(cls := "ak")("cache hit"),
            span(cls := "av")(s"${cost.cacheHitPct}%"),
          ),
          div(cls := "anatomy-row anatomy-row-strong")(
            span(cls := "ak")("total"),
            span(cls := "av anatomy-cost")(fmtCost(cost.totalCostUsd)),
          ),
        )
      },
    )

  private def buildAttributionCard(segments: List[SegmentRow], labels: AnatomyLabels): Frag =
    div(cls := CardClass)(
      cardTitle(labels.attributionTitle),
      frag(segments.map { s =>
        div(cls := "anatomy-seg")(
          div(cls := "anatomy-row")(
            span(cls := "ak")(s.label),
            span(cls := "av")(s"~${fmtTok(s.estTokens)} · ${s.pct}%"),
          ),
          div(cls := BarClass)(span(style := s"width:${s.pct}%;background:var(--blue)")),
        )
      }),
    )

  private def buildStructureCard(s: StructureFacts, labels: AnatomyLabels): Frag =
    div(cls := CardClass)(
      cardTitle(labels.structureTitle),
      kv(labels.lblMessages, s.messageCount.toString),
      kv(labels.lblTurns, s"${s.userTurns} / ${s.assistantTurns}"),
      kv(labels.lblSystemBlocks, s"${s.systemBlocks} (${s.cachedSystemBlocks})"),
      kv(labels.lblTools, s.toolsDefined.toString),
      kv(labels.lblToolUse, s.toolUseCount.toString),
      kv(labels.lblToolResult, s.toolResultCount.toString),
      kv(labels.lblImages, s.imageCount.toString),
      kv(labels.lblThinking, s.thinkingBlocks.toString),
      kv(labels.lblStream, s.stream.map(_.toString).getOrElse("—")),
    )

  private def buildInventoryCard(inventory: List[InventoryDisplay], labels: AnatomyLabels): Frag =
    if (inventory.isEmpty) frag()
    else
      div(cls := CardClass)(
        cardTitle(labels.inventoryTitle),
        frag(inventory.map { r =>
          div(cls := "anatomy-row")(
            span(cls := "ak")(r.label),
            span(cls := "av")(
              if (r.estTokens > 0) s"${r.count} · ~${fmtTok(r.estTokens)} tok" else r.count.toString,
            ),
          )
        }),
      )

  private def buildAnomalyStrip(anomalies: List[AnomalyDisplay], labels: AnatomyLabels): Frag =
    if (anomalies.isEmpty) frag()
    else
      div(cls := CardClass)(
        cardTitle(labels.anomaliesTitle),
        frag(anomalies.map { a =>
          val (kindCls, glyph) = a.kind match {
            case AnomalyKind.Security => ("anatomy-anomaly-security", "🛡 ")
            case AnomalyKind.Warn => ("anatomy-anomaly-warn", "⚠ ")
            case AnomalyKind.Info => ("anatomy-anomaly-info", "ℹ ")
          }
          div(cls := s"anatomy-anomaly $kindCls")(s"$glyph${a.message}")
        }),
      )

  private def fmtCost(c: Double): String =
    if (c === 0.0) "$0" else f"$$$c%.4f"

  /** Group digits with commas: 35423 -> "35,423 tok". */
  private def fmtTok(n: Int): String = {
    val s   = n.abs.toString
    val grp = s.reverse.grouped(3).mkString(",").reverse
    s"${if (n < 0) "-" else ""}$grp tok"
  }
}
