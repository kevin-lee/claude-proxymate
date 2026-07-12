package claudeproxymate.renderer.proxy

import claudeproxymate.core.QueryParamMask
import claudeproxymate.renderer.view.I18nTemplate
import scalatags.Text.all.*

/** Typed shape of one proxy capture row, resolved by the caller from the
  * `js.Dynamic` capture before calling the view.
  */
final case class ProxyListEntry(
  id: Double,
  method: String,
  path: String,
  ts: String,
  model: Option[String],
  status: Option[Int],
  selected: Boolean,
)

/** Static i18n-sourced chrome labels for the proxy list. */
final case class ProxyListLabels(
  noCapturesTitle: String,
  noCapturesHint: String,
)

/** Pure view functions for the proxy capture list. */
object ProxyListView {

  /** CSS class + attribute used by the document-level click handler in
    * [[ProxyList]] to find an entry. Exposed as constants so the view and
    * the handler stay in sync.
    */
  val EntryClass: String      = "prx-entry"
  val EntryDataIdAttr: String = "data-id"

  def buildListFrag(entries: List[ProxyListEntry], labels: ProxyListLabels): Frag =
    if (entries.isEmpty)
      div(cls := "hist-empty")(
        span(cls := "empty-icon")("📡"),
        span(cls := "empty-title")(labels.noCapturesTitle),
        small(I18nTemplate.render(labels.noCapturesHint)),
      )
    else
      frag(entries.map(buildEntryFrag))

  private def buildEntryFrag(e: ProxyListEntry): Frag = {
    val entryCls = if (e.selected) s"$EntryClass selected" else EntryClass

    val statusBadge: Frag = e.status match {
      case Some(s) =>
        val color = if (s > 0 && s < 400) "var(--green)" else "var(--red)"
        val text  = if (s > 0) s.toString else "ERR"
        span(
          style := s"font-size:10px;font-family:'SF Mono',monospace;flex-shrink:0;color:$color",
        )(text)
      case None =>
        span(style := "font-size:10px;color:var(--dim);flex-shrink:0")("…")
    }

    val modelFrag: Frag = e.model match {
      case Some(m) => div(cls := "prx-model")(m)
      case None => frag()
    }

    div(
      cls := entryCls,
      attr(EntryDataIdAttr) := e.id.toString,
    )(
      div(style := "display:flex;align-items:center;overflow:hidden")(
        span(cls := "prx-method")(e.method),
        span(cls := "prx-path")(QueryParamMask.maskPath(e.path)),
        statusBadge,
        span(cls := "prx-ts")(e.ts),
      ),
      modelFrag,
    )
  }
}
