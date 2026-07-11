package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object IndexHtmlGeneratorSpec extends Properties {

  override def tests: List[Test] = List(
    example("no inline event handler attributes are emitted", testNoInlineEventHandlers),
    example("onboard close button has the OnboardCloseBtn id", testOnboardCloseBtnId),
    example("onboard carousel elements have their ids", testOnboardCarouselElements),
    example("onboard slides reference the getting-started screenshots", testOnboardSlideImages),
    example("onboard dots carry their data-onboard-slide attributes", testOnboardDotsDataAttr),
    example("proxy cmd copy button has the ProxyCmdCopyBtn id", testProxyCmdCopyBtnId),
    example("proxy clear button has the ProxyClearBtn id", testProxyClearBtnId),
    example("VS Code toggle button has the VsCodeToggleBtn id", testVsCodeToggleBtnId),
    example("copy detail button has the CopyDetailBtn id", testCopyDetailBtnId),
    example("existing button ids are preserved", testExistingButtonIdsPreserved),
    example("dtab buttons carry their data-dtab attributes", testDtabDataAttrs),
    example("CSP meta tag is emitted in <head>", testCspMetaPresent),
    example("CSP locks script-src to 'self' (no unsafe-inline / unsafe-eval)", testCspScriptSrcStrict),
    example("CSP includes object-src 'none'", testCspObjectSrcNone),
    example("CSP allows api.github.com in connect-src for the update check", testCspGitHubApiAllowed),
    example("CSP includes base-uri / form-action / frame-ancestors hardening", testCspDefenceInDepth),
  )

  private val sampleLocale: Map[String, String] = Map(
    "onboard.title"            -> "title",
    "onboard.sub"              -> "sub",
    "onboard.s3.title"         -> "s3title",
    "onboard.s3.body"          -> "s3body",
    "onboard.s3.after"         -> "s3after",
    "onboard.imgAlt1"          -> "imgAlt1",
    "onboard.imgAlt2"          -> "imgAlt2",
    "onboard.prev"             -> "prev",
    "onboard.next"             -> "next",
    "onboard.note"             -> "note",
    "onboard.btn"              -> "btn",
    "header.logoSub"           -> "Proxy",
    "proxy.port"               -> "port",
    "proxy.stopped"            -> "stopped",
    "proxy.startFirst"         -> "startFirst",
    "proxy.startProxy"         -> "startProxy",
    "proxy.clear"              -> "clear",
    "proxy.aboutTitle"         -> "about",
    "proxy.capturedRequests"   -> "captured",
    "proxy.noCapturesTitle"    -> "noCapturesTitle",
    "proxy.selectRequestTitle" -> "selectRequestTitle",
    "copy.copy"                -> "copy",
  )

  private lazy val rendered: String = IndexHtmlGenerator.generate(sampleLocale)

  private val InlineEventHandlerAttrs: List[String] = List(
    "onclick=",
    "oninput=",
    "oncompositionstart=",
    "oncompositionend=",
    "onload=",
    "onerror=",
    "onmouseover=",
    "onmousedown=",
    "onmouseup=",
    "onfocus=",
    "onblur=",
    "onkeydown=",
    "onkeyup=",
    "onkeypress=",
    "onchange=",
    "onsubmit=",
    "ondblclick=",
    "ontouchstart=",
    "ontouchend=",
  )

  def testNoInlineEventHandlers: Result =
    Result.all(InlineEventHandlerAttrs.map { attr =>
      Result
        .assert(!rendered.contains(attr))
        .log(s"unexpected inline handler attribute `$attr` in generated HTML")
    })

  def testOnboardCloseBtnId: Result =
    Result
      .assert(rendered.contains(s"""id="${HtmlIds.OnboardCloseBtn}""""))
      .log(s"`id=\"${HtmlIds.OnboardCloseBtn}\"` missing from generated HTML")

  def testOnboardCarouselElements: Result =
    Result.all(
      List(HtmlIds.OnboardTrack, HtmlIds.OnboardDots, HtmlIds.OnboardPrev, HtmlIds.OnboardNext).map { id =>
        Result
          .assert(rendered.contains(s"""id="$id""""))
          .log(s"`id=\"$id\"` missing from generated HTML")
      }
    )

  def testOnboardSlideImages: Result =
    Result.all(
      List("getting-started-01.png", "getting-started-02.png").map { img =>
        Result
          .assert(rendered.contains(img))
          .log(s"`$img` missing from generated HTML")
      }
    )

  def testOnboardDotsDataAttr: Result =
    Result.all(
      List("0", "1", "2").map { n =>
        Result
          .assert(rendered.contains(s"""data-onboard-slide="$n""""))
          .log(s"`data-onboard-slide=\"$n\"` missing from generated HTML")
      }
    )

  def testProxyCmdCopyBtnId: Result =
    Result
      .assert(rendered.contains(s"""id="${HtmlIds.ProxyCmdCopyBtn}""""))
      .log(s"`id=\"${HtmlIds.ProxyCmdCopyBtn}\"` missing from generated HTML")

  def testProxyClearBtnId: Result =
    Result
      .assert(rendered.contains(s"""id="${HtmlIds.ProxyClearBtn}""""))
      .log(s"`id=\"${HtmlIds.ProxyClearBtn}\"` missing from generated HTML")

  def testVsCodeToggleBtnId: Result =
    Result
      .assert(rendered.contains(s"""id="${HtmlIds.VsCodeToggleBtn}""""))
      .log(s"`id=\"${HtmlIds.VsCodeToggleBtn}\"` missing from generated HTML")

  def testCopyDetailBtnId: Result =
    Result
      .assert(rendered.contains(s"""id="${HtmlIds.CopyDetailBtn}""""))
      .log(s"`id=\"${HtmlIds.CopyDetailBtn}\"` missing from generated HTML")

  def testExistingButtonIdsPreserved: Result = {
    val expected = List(
      HtmlIds.ThemeToggleBtn,
      HtmlIds.LangToggleBtn,
      HtmlIds.ProxyStartBtn,
      HtmlIds.ProxyPort,
    )
    Result.all(expected.map { id =>
      Result
        .assert(rendered.contains(s"""id="$id""""))
        .log(s"`id=\"$id\"` missing from generated HTML")
    })
  }

  def testDtabDataAttrs: Result =
    Result.all(List("messages", "request", "response", "analysis").map { tab =>
      Result
        .assert(rendered.contains(s"""data-dtab="$tab""""))
        .log(s"`data-dtab=\"$tab\"` missing from generated HTML")
    })

  def testCspMetaPresent: Result =
    Result
      .assert(rendered.contains("""http-equiv="Content-Security-Policy""""))
      .log(s"CSP meta tag missing from generated HTML")

  def testCspScriptSrcStrict: Result =
    Result.all(
      List(
        Result
          .assert(rendered.contains("script-src 'self'"))
          .log("`script-src 'self'` missing"),
        Result
          .assert(!rendered.contains("script-src 'self' 'unsafe-inline'"))
          .log("`script-src` must not include `'unsafe-inline'`"),
        Result
          .assert(!rendered.contains("'unsafe-eval'"))
          .log("`'unsafe-eval'` must never appear in the CSP"),
      )
    )

  def testCspObjectSrcNone: Result =
    Result
      .assert(rendered.contains("object-src 'none'"))
      .log("`object-src 'none'` missing from CSP")

  def testCspGitHubApiAllowed: Result =
    Result
      .assert(rendered.contains("https://api.github.com"))
      .log("`https://api.github.com` missing from CSP connect-src")

  def testCspDefenceInDepth: Result =
    Result.all(
      List(
        Result.assert(rendered.contains("base-uri 'self'")).log("`base-uri 'self'` missing"),
        Result.assert(rendered.contains("form-action 'none'")).log("`form-action 'none'` missing"),
        Result.assert(rendered.contains("frame-ancestors 'none'")).log("`frame-ancestors 'none'` missing"),
      )
    )
}
