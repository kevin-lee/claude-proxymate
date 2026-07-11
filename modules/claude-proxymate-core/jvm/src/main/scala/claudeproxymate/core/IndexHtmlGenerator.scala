package claudeproxymate.core

import cats.syntax.all.*
import scalatags.Text.all.*
import scalatags.Text.tags2.title as titleTag

/** Generates the `index.html` shell for the Electron renderer process using ScalaTags.
  *
  * All element IDs reference [[HtmlIds]] constants so that the renderer (Scala.js)
  * and this generator (JVM) share a single source of truth.
  *
  * Default text content comes from the provided locale map (typically `ko.properties`)
  * and is overwritten at runtime by the renderer's i18n system via `data-i18n` attributes.
  */
object IndexHtmlGenerator {

  private val i18n             = attr("data-i18n")
  private val i18nHtml         = attr("data-i18n-html")
  private val i18nAlt          = attr("data-i18n-alt")
  private val i18nTitle        = attr("data-i18n-title")
  private val dataDtab         = attr("data-dtab")
  private val dataOnboardSlide = attr("data-onboard-slide")

  /** Strict Content-Security-Policy applied via `<meta http-equiv>`.
    *
    *   - `script-src 'self'` is the high-value lock: no inline JS,
    *     no `eval`, no remote scripts. A3i made this possible by
    *     removing every inline `onclick` / `oninput` handler.
    *   - `style-src 'self' 'unsafe-inline'` is intentional —
    *     `IndexHtmlGenerator` and the Scalatags views use many
    *     inline `style=""` attributes for layout / mechanism colors.
    *     No captured wire data flows into a `style` attribute, so
    *     CSS-injection risk is minimal; tightening this to `'self'`
    *     would require moving ~60+ inline styles into `styles.css`
    *     (separate refactor).
    *   - `connect-src` includes `https://api.github.com` for the
    *     in-app update check (`UpdateChecker.checkForUpdate`).
    */
  val ContentSecurityPolicy: String =
    "default-src 'self'; " +
      "script-src 'self'; " +
      "style-src 'self' 'unsafe-inline'; " +
      "img-src 'self'; " +
      "connect-src 'self' https://api.github.com; " +
      "object-src 'none'; " +
      "base-uri 'self'; " +
      "form-action 'none'; " +
      "frame-ancestors 'none'"

  /** Look up a translation key, falling back to the key itself. */
  private def tx(m: Map[String, String], key: String): String =
    m.getOrElse(key, key)

  def generate(defaultLocale: Map[String, String]): String =
    "<!DOCTYPE html>\n" + html(lang := "en")(
      head(
        meta(charset := "UTF-8"),
        meta(attr("http-equiv") := "Content-Security-Policy", content := ContentSecurityPolicy),
        meta(name := "viewport", content := "width=device-width, initial-scale=1.0"),
        titleTag("Claude Proxymate"),
        link(rel := "icon", attr("type") := "image/png", href := "../assets/logo/dark/icon-32.png"),
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
        div(cls := "onboard-carousel")(
          div(cls := "onboard-viewport")(
            div(id := HtmlIds.OnboardTrack, cls := "onboard-track")(
              // Slide 0 — Start Proxy screenshot (image-only)
              div(cls := "onboard-slide")(
                img(
                  cls := "onboard-img",
                  src := "../assets/getting-started/getting-started-01.png",
                  i18nAlt := "onboard.imgAlt1",
                  alt := tx(m, "onboard.imgAlt1"),
                ),
              ),
              // Slide 1 — Copy command screenshot (image-only)
              div(cls := "onboard-slide")(
                img(
                  cls := "onboard-img",
                  src := "../assets/getting-started/getting-started-02.png",
                  i18nAlt := "onboard.imgAlt2",
                  alt := tx(m, "onboard.imgAlt2"),
                ),
              ),
              // Slide 2 — instructions (text)
              div(cls := "onboard-slide onboard-slide-text")(
                div(cls := "onboard-s3-title", i18n := "onboard.s3.title")(tx(m, "onboard.s3.title")),
                div(cls := "onboard-s3-body")(
                  span(i18n := "onboard.s3.body")(tx(m, "onboard.s3.body")),
                  code(cls := "onboard-cmd")("ANTHROPIC_BASE_URL=http://localhost:8888 claude"),
                  span(i18nHtml := "onboard.s3.after")(raw(tx(m, "onboard.s3.after"))),
                ),
              ),
            ),
          ),
          div(cls := "onboard-nav")(
            button(
              id := HtmlIds.OnboardPrev,
              cls := "onboard-arrow",
              attr("aria-label") := tx(m, "onboard.prev"),
              i18nTitle := "onboard.prev",
              attr("title") := tx(m, "onboard.prev"),
            )("‹"),
            div(id := HtmlIds.OnboardDots, cls := "onboard-dots")(
              (0 until 3).map { i =>
                span(
                  cls := (if (i === 0) "onboard-dot active" else "onboard-dot"),
                  dataOnboardSlide := i.toString,
                )
              },
            ),
            div(cls := "onboard-nav-right")(
              button(
                id := HtmlIds.OnboardNext,
                cls := "onboard-arrow",
                attr("aria-label") := tx(m, "onboard.next"),
                i18nTitle := "onboard.next",
                attr("title") := tx(m, "onboard.next"),
              )("›"),
              // Shown only on the last slide (toggled by Onboarding.render); replaces the › arrow there.
              button(
                id := HtmlIds.OnboardCloseBtn,
                cls := "onboard-btn-pill is-hidden",
                i18n := "onboard.btn",
              )(tx(m, "onboard.btn")),
            ),
          ),
        ),
        div(cls := "onboard-note", i18n := "onboard.note")(tx(m, "onboard.note")),
      ),
    )

  // ── Header ──

  private def headerSection(m: Map[String, String]): Frag =
    tag("header")(cls := "header")(
      div(cls := "logo")(
        img(
          id := HtmlIds.LogoIcon,
          cls := "logo-icon",
          src := "../assets/logo/claude-proxymate-icon-light.svg",
          alt := "Claude Proxymate",
        ),
        span(cls := "logo-text")("Claude Proxymate"),
        span(cls := "logo-sub", i18n := "header.logoSub")(tx(m, "header.logoSub")),
        span(cls := "logo-ver", id := HtmlIds.BuildVer),
        a(cls := "update-badge", id := HtmlIds.UpdateBadge, href := "#", target := "_blank", rel := "noopener"),
      ),
      div(cls := "header-right")(
        button(
          id := HtmlIds.ThemeToggleBtn,
          cls := "theme-toggle-btn",
          attr("aria-label") := "Theme: System",
          attr("title") := "Theme: System",
          attr("data-theme-state") := "system",
        )(
          themeIconSystem,
          themeIconLight,
          themeIconDark,
        ),
        button(
          id := HtmlIds.LangToggleBtn,
          style := "padding:4px 10px;border-radius:5px;font-size:11px;background:none;border:1px solid var(--border);color:var(--dim);cursor:pointer;transition:all .15s;white-space:nowrap;font-weight:700;",
        )(""),
      ),
    )

  private val themeIconSystem: Frag =
    raw(
      """<svg class="theme-icon theme-icon-system" viewBox="0 0 24 24" width="16" height="16" aria-hidden="true">""" +
        """<path fill="currentColor" d="m12 21c4.971 0 9-4.029 9-9s-4.029-9-9-9-9 4.029-9 9 4.029 9 9 9zm4.95-13.95c1.313 1.313 2.05 3.093 2.05 4.95s-0.738 3.637-2.05 4.95c-1.313 1.313-3.093 2.05-4.95 2.05v-14c1.857 0 3.637 0.737 4.95 2.05z"/>""" +
        """</svg>""",
    )

  private val themeIconLight: Frag =
    raw(
      """<svg class="theme-icon theme-icon-light" viewBox="0 0 24 24" width="16" height="16" aria-hidden="true">""" +
        """<path fill="currentColor" d="M12,9c1.65,0,3,1.35,3,3s-1.35,3-3,3s-3-1.35-3-3S10.35,9,12,9 M12,7c-2.76,0-5,2.24-5,5s2.24,5,5,5s5-2.24,5-5 S14.76,7,12,7L12,7z M2,13l2,0c0.55,0,1-0.45,1-1s-0.45-1-1-1l-2,0c-0.55,0-1,0.45-1,1S1.45,13,2,13z M20,13l2,0c0.55,0,1-0.45,1-1s-0.45-1-1-1l-2,0c-0.55,0-1,0.45-1,1S19.45,13,20,13z M11,2v2c0,0.55,0.45,1,1,1s1-0.45,1-1V2c0-0.55-0.45-1-1-1S11,1.45,11,2z M11,20v2c0,0.55,0.45,1,1,1s1-0.45,1-1v-2c0-0.55-0.45-1-1-1C11.45,19,11,19.45,11,20z M5.99,4.58c-0.39-0.39-1.03-0.39-1.41,0 c-0.39,0.39-0.39,1.03,0,1.41l1.06,1.06c0.39,0.39,1.03,0.39,1.41,0s0.39-1.03,0-1.41L5.99,4.58z M18.36,16.95 c-0.39-0.39-1.03-0.39-1.41,0c-0.39,0.39-0.39,1.03,0,1.41l1.06,1.06c0.39,0.39,1.03,0.39,1.41,0c0.39-0.39,0.39-1.03,0-1.41 L18.36,16.95z M19.42,5.99c0.39-0.39,0.39-1.03,0-1.41c-0.39-0.39-1.03-0.39-1.41,0l-1.06,1.06c-0.39,0.39-0.39,1.03,0,1.41 s1.03,0.39,1.41,0L19.42,5.99z M7.05,18.36c0.39-0.39,0.39-1.03,0-1.41c-0.39-0.39-1.03-0.39-1.41,0l-1.06,1.06 c-0.39,0.39-0.39,1.03,0,1.41s1.03,0.39,1.41,0L7.05,18.36z"/>""" +
        """</svg>""",
    )

  private val themeIconDark: Frag =
    raw(
      """<svg class="theme-icon theme-icon-dark" viewBox="0 0 24 24" width="16" height="16" aria-hidden="true">""" +
        """<path fill="currentColor" d="M9.37,5.51C9.19,6.15,9.1,6.82,9.1,7.5c0,4.08,3.32,7.4,7.4,7.4c0.68,0,1.35-0.09,1.99-0.27C17.45,17.19,14.93,19,12,19 c-3.86,0-7-3.14-7-7C5,9.07,6.81,6.55,9.37,5.51z M12,3c-4.97,0-9,4.03-9,9s4.03,9,9,9s9-4.03,9-9c0-0.46-0.04-0.92-0.1-1.36 c-0.98,1.37-2.58,2.26-4.4,2.26c-2.98,0-5.4-2.42-5.4-5.4c0-1.81,0.89-3.42,2.26-4.4C12.92,3.04,12.46,3,12,3L12,3z"/>""" +
        """</svg>""",
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
          ),
        ),
        div(id := HtmlIds.ProxyStatus, cls := "proxy-status proxy-bar-status")(
          span(cls := "ps-dot"),
          span(id := HtmlIds.ProxyStatusText, i18n := "proxy.stopped")(tx(m, "proxy.stopped")),
        ),
        span(
          id := HtmlIds.MaskStateChip,
          cls := "mask-state-chip",
          i18n := "mask.chipMaskAll",
        )(tx(m, "mask.chipMaskAll")),
      ),
      div(id := HtmlIds.ProxyCmdBox, cls := "proxy-bar-cmd")(
        span(id := HtmlIds.ProxyCmdText, style := "color:var(--dim)", i18n := "proxy.startFirst")(
          tx(m, "proxy.startFirst"),
        ),
        button(
          id := HtmlIds.ProxyCmdCopyBtn,
          cls := "proxy-bar-cmd-copy",
          attr("aria-label") := "Copy command",
          attr("title") := tx(m, "copy.copy"),
        )("\u29C9"),
      ),
      div(cls := "proxy-bar-right")(
        button(
          cls := "btn btn-send proxy-bar-btn-primary",
          id := HtmlIds.ProxyStartBtn,
          i18n := "proxy.startProxy",
        )(tx(m, "proxy.startProxy")),
        button(
          id := HtmlIds.ProxyClearBtn,
          cls := "btn btn-copy proxy-bar-btn-clear",
          i18n := "proxy.clear",
        )(tx(m, "proxy.clear")),
        button(
          id := HtmlIds.MaskToggleBtn,
          cls := "btn btn-copy proxy-bar-btn-mask",
          i18n := "mask.toggleRevealAll",
          attr("title") := tx(m, "mask.toggleRevealAll"),
          attr("aria-label") := tx(m, "mask.toggleRevealAll"),
        )(tx(m, "mask.toggleRevealAll")),
        button(
          id := HtmlIds.VsCodeToggleBtn,
          cls := "btn btn-copy proxy-bar-btn-vscode",
          i18n := "vscode.btnOff",
          i18nTitle := "vscode.titleOff",
          attr("title") := tx(m, "vscode.titleOff"),
          attr("aria-label") := tx(m, "vscode.titleOff"),
        )(tx(m, "vscode.btnOff")),
        button(
          id := HtmlIds.ProxyInfoBtn,
          cls := "proxy-bar-info",
          attr("aria-label") := "Info",
          attr("title") := tx(m, "proxy.aboutTitle"),
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
          // Placeholder; replaced at renderer startup by ProxyList.renderProxyList().
          div(cls := "hist-empty", i18n := "proxy.noCapturesTitle")(
            tx(m, "proxy.noCapturesTitle"),
          ),
        ),
      ),
      div(cls := "proxy-detail")(
        div(cls := "dtabs")(
          // dtab clicks are dispatched by the renderer's DtabListeners
          // (doc-level click delegation reading data-dtab). No inline
          // handlers here — Scala.js NoModule's let-exported globals
          // don't reliably resolve from inline `onclick=...` attributes.
          button(cls := "dtab active", dataDtab := "messages")("Messages"),
          button(cls := "dtab", dataDtab := "request")("Request"),
          button(cls := "dtab", dataDtab := "response")("Response"),
          button(
            cls := "dtab",
            dataDtab := "analysis",
            style := "color:var(--purple)",
          )("Analysis"),
          button(
            id := HtmlIds.CopyDetailBtn,
            cls := "copy-small",
            style := "margin-left:auto",
            i18n := "copy.copy",
            attr("data-i18n-title") := "copy.detailTitle",
            attr("title") := tx(m, "copy.detailTitle"),
          )(
            tx(m, "copy.copy"),
          ),
        ),
        div(id := HtmlIds.ProxyDetailView, style := "flex:1;overflow:hidden;display:flex;flex-direction:column")(
          // Renderer immediately replaces this with the view-rendered
          // placeholder once a capture is selected (or stays as a plain-
          // text title until then). Same trick as proxy.noCapturesTitle.
          div(cls := "proxy-empty")(
            span(style := "font-size:28px")("\uD83D\uDD0D"),
            span(i18n := "proxy.selectRequestTitle")(
              tx(m, "proxy.selectRequestTitle"),
            ),
          ),
        ),
      ),
    )
}
