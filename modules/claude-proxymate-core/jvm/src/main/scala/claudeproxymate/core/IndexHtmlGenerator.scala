package claudeproxymate.core

import scalatags.Text.all.*
import scalatags.Text.tags2.{title as titleTag}

/** Generates the `index.html` shell for the Electron renderer process using ScalaTags.
  *
  * All element IDs reference [[HtmlIds]] constants so that the renderer (Scala.js)
  * and this generator (JVM) share a single source of truth.
  *
  * Default text content comes from the provided locale map (typically `ko.properties`)
  * and is overwritten at runtime by the renderer's i18n system via `data-i18n` attributes.
  */
object IndexHtmlGenerator {

  private val i18n     = attr("data-i18n")
  private val i18nHtml = attr("data-i18n-html")
  private val dataDtab = attr("data-dtab")

  /** Look up a translation key, falling back to the key itself. */
  private def tx(m: Map[String, String], key: String): String =
    m.getOrElse(key, key)

  def generate(defaultLocale: Map[String, String]): String =
    "<!DOCTYPE html>\n" + html(lang := "en")(
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1.0"),
        titleTag("Claude Proxymate"),
        link(rel := "stylesheet", href := "styles.css"),
      ),
      body(
        onboardingModal(defaultLocale),
        headerSection(defaultLocale),
        proxyBar(defaultLocale),
        mainSection(defaultLocale),
        script(src := "renderer.js"),
      ),
    ).render

  // ── Onboarding Modal ──

  private def onboardingModal(m: Map[String, String]): Frag =
    div(id := HtmlIds.OnboardModal, cls := "onboard-overlay", style := "display:none")(
      div(cls := "onboard-card")(
        div(cls := "onboard-title", i18n := "onboard.title")(tx(m, "onboard.title")),
        div(cls := "onboard-sub", i18n := "onboard.sub")(tx(m, "onboard.sub")),
        ol(cls := "onboard-steps")(
          li(i18nHtml := "onboard.step1")(raw(tx(m, "onboard.step1"))),
          li(
            span(i18nHtml := "onboard.step2")(raw(tx(m, "onboard.step2"))),
            code(cls := "onboard-cmd")("ANTHROPIC_BASE_URL=http://localhost:8888 claude"),
          ),
          li(i18nHtml := "onboard.step3")(raw(tx(m, "onboard.step3"))),
        ),
        div(cls := "onboard-note", i18n := "onboard.note")(tx(m, "onboard.note")),
        button(cls := "onboard-btn", onclick := "closeOnboard()", i18n := "onboard.btn")(tx(m, "onboard.btn")),
      ),
    )

  // ── Header ──

  private def headerSection(m: Map[String, String]): Frag =
    tag("header")(cls := "header")(
      div(cls := "logo")(
        div(cls := "logo-icon")("CP"),
        span(cls := "logo-text")("Claude Proxymate"),
        span(cls := "logo-sub", i18n := "header.logoSub")(tx(m, "header.logoSub")),
        span(cls := "logo-ver", id := HtmlIds.BuildVer),
        a(cls := "update-badge", id := HtmlIds.UpdateBadge, href := "#", target := "_blank", rel := "noopener"),
      ),
      div(cls := "header-right")(
        button(
          id := HtmlIds.LangToggleBtn,
          onclick := "toggleLocale()",
          style := "padding:4px 10px;border-radius:5px;font-size:11px;background:none;border:1px solid var(--border);color:var(--dim);cursor:pointer;transition:all .15s;white-space:nowrap;font-weight:700;",
        )(""),
      ),
    )

  // ── Main ──

  private def mainSection(m: Map[String, String]): Frag =
    div(cls := "main")(
      proxyPanel(m),
    )

  // ── Proxy Panel ──

  private def proxyPanel(m: Map[String, String]): Frag =
    div(id := HtmlIds.ProxyPanel, cls := "proxy-panel", style := "display:none")(
      proxyStream(m),
    )

  // ── Proxy Bar (browser address-bar style) ──

  private def proxyBar(m: Map[String, String]): Frag =
    div(id := HtmlIds.ProxyBar, cls := "proxy-bar", style := "display:none")(
      div(cls := "proxy-bar-left")(
        tag("label")(cls := "proxy-bar-port")(
          span(cls := "proxy-bar-port-label", i18n := "proxy.port")(tx(m, "proxy.port")),
          input(
            tpe := "number",
            id := HtmlIds.ProxyPort,
            value := "8888",
            attr("min") := "1024",
            attr("max") := "65535",
            attr("oninput") := "updateProxyCmd()",
          ),
        ),
        div(id := HtmlIds.ProxyStatus, cls := "proxy-status proxy-bar-status")(
          span(cls := "ps-dot"),
          span(id := HtmlIds.ProxyStatusText, i18n := "proxy.stopped")(tx(m, "proxy.stopped")),
        ),
      ),
      div(id := HtmlIds.ProxyCmdBox, cls := "proxy-bar-cmd")(
        span(id := HtmlIds.ProxyCmdText, style := "color:var(--dim)", i18n := "proxy.startFirst")(
          tx(m, "proxy.startFirst"),
        ),
        button(
          cls := "proxy-bar-cmd-copy",
          onclick := "copyProxyCmd()",
          attr("aria-label") := "Copy command",
          attr("title") := tx(m, "copy.copy"),
        )("\u29C9"),
      ),
      div(cls := "proxy-bar-right")(
        button(
          cls := "btn btn-send proxy-bar-btn-primary",
          id := HtmlIds.ProxyStartBtn,
          onclick := "toggleProxy()",
          i18n := "proxy.startProxy",
        )(tx(m, "proxy.startProxy")),
        button(
          cls := "btn btn-copy proxy-bar-btn-clear",
          onclick := "clearProxyCaptures()",
          i18n := "proxy.clear",
        )(tx(m, "proxy.clear")),
        button(
          id := HtmlIds.ProxyInfoBtn,
          cls := "proxy-bar-info",
          onclick := "toggleProxyInfo()",
          attr("aria-label") := "Info",
          attr("title") := tx(m, "proxy.interceptTitle"),
        )("\u2139"),
      ),
    )

  // ── Proxy Stream ──

  private def proxyStream(m: Map[String, String]): Frag =
    div(cls := "proxy-stream")(
      div(cls := "proxy-list")(
        div(cls := "panel-header")(
          span(i18n := "proxy.capturedRequests")(tx(m, "proxy.capturedRequests")),
          span(style := "font-size:10px;color:var(--dim)", id := HtmlIds.ProxyCount)("0"),
        ),
        div(cls := "hist-list", id := HtmlIds.ProxyList)(
          div(cls := "hist-empty", i18nHtml := "proxy.noCaptures")(
            raw(tx(m, "proxy.noCaptures")),
          ),
        ),
      ),
      div(cls := "proxy-detail")(
        div(cls := "dtabs")(
          button(cls := "dtab active", dataDtab := "messages", onclick := "showDetailTab('messages')")("Messages"),
          button(cls := "dtab", dataDtab := "request", onclick := "showDetailTab('request')")("Request"),
          button(cls := "dtab", dataDtab := "response", onclick := "showDetailTab('response')")("Response"),
          button(
            cls := "dtab",
            dataDtab := "analysis",
            onclick := "showDetailTab('analysis')",
            style := "color:var(--purple)",
          )("Analysis"),
          button(cls := "copy-small", style := "margin-left:auto", onclick := "copyProxyDetail()", i18n := "copy.copy")(
            tx(m, "copy.copy"),
          ),
        ),
        div(id := HtmlIds.ProxyDetailView, style := "flex:1;overflow:hidden;display:flex;flex-direction:column")(
          div(cls := "proxy-empty")(
            span(style := "font-size:28px")("\uD83D\uDD0D"),
            span(i18nHtml := "proxy.selectRequest")(
              raw(tx(m, "proxy.selectRequest")),
            ),
          ),
        ),
      ),
    )
}
