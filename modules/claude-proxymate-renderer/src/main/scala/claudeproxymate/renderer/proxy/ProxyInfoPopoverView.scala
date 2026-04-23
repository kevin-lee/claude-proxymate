package claudeproxymate.renderer.proxy

import claudeproxymate.renderer.view.I18nTemplate
import scalatags.Text.all.*

/** Static i18n-sourced labels for the proxy-info popover.
  *
  * `desc` may contain `{{...}}` tokens understood by
  * [[claudeproxymate.renderer.view.I18nTemplate]].
  */
final case class ProxyInfoPopoverLabels(
  title: String,
  desc: String,
)

/** Pure view for the proxy-info popover. */
object ProxyInfoPopoverView {

  val PopoverClass: String = "proxy-info-popover"

  def buildPopoverFrag(labels: ProxyInfoPopoverLabels): Frag =
    frag(
      div(cls := s"$PopoverClass-title")(labels.title),
      div(cls := s"$PopoverClass-body")(I18nTemplate.render(labels.desc)),
    )
}
