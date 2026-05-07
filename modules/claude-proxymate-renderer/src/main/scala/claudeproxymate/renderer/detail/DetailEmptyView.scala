package claudeproxymate.renderer.detail

import claudeproxymate.renderer.view.I18nTemplate
import scalatags.Text.all.*

/** Static labels for the "select a request" placeholder. `Hint` may
  * carry `{{...}}` tokens.
  */
final case class DetailEmptyLabels(
  selectRequestTitle: String,
  selectRequestHint: String,
)

/** Pure view for the placeholder shown in the proxy detail panel when no
  * capture is selected.
  */
object DetailEmptyView {

  def buildFrag(labels: DetailEmptyLabels): Frag =
    div(cls := "proxy-empty")(
      span(style := "font-size:28px")("🔍"),
      span(
        labels.selectRequestTitle,
        I18nTemplate.render(labels.selectRequestHint),
      ),
    )
}
