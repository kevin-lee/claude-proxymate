package claudeproxymate.renderer.proxy

import claudeproxymate.renderer.detail.TokenPopoverLinks
import scalatags.Text.all.*

/** Static i18n-sourced labels for the proxy-info (About) popover. */
final case class ProxyInfoPopoverLabels(
  title: String,
  websiteLabel: String,
  bugReportLabel: String,
)

/** Pure view for the proxy-info (About) popover. */
object ProxyInfoPopoverView {

  val PopoverClass: String = "proxy-info-popover"

  val WebsiteHref: String = "https://claude-proxymate.kevinly.dev/"
  val IssuesHref: String  = "https://github.com/kevin-lee/claude-proxymate/issues"

  def buildPopoverFrag(labels: ProxyInfoPopoverLabels): Frag =
    frag(
      div(cls := s"$PopoverClass-title")(labels.title),
      div(cls := s"$PopoverClass-body")(
        div(cls := s"$PopoverClass-link-row")(externalLink(WebsiteHref, labels.websiteLabel)),
        div(cls := s"$PopoverClass-link-row")(externalLink(IssuesHref, labels.bugReportLabel)),
      ),
    )

  /* Same attribute set as the token-popover docs link — the
   * `external-link` class routes clicks through electronAPI.openExternal
   * via TokenPopover's document-level handler. */
  private def externalLink(hrefValue: String, label: String): Frag =
    a(
      href := hrefValue,
      cls := TokenPopoverLinks.ExternalLinkClass,
      target := "_blank",
      rel := "noopener",
      style := "color:var(--blue);text-decoration:underline;cursor:pointer",
    )(label)
}
