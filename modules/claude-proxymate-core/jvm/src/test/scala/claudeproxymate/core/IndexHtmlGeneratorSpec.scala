package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object IndexHtmlGeneratorSpec extends Properties {

  override def tests: List[Test] = List(
    example("no inline event handler attributes are emitted", testNoInlineEventHandlers),
    example("onboarding stage and its controls have their ids", testOnboardControlIds),
    example("guided tour layer shell is emitted hidden", testTourLayerShell),
    example("every guided tour target id exists in the generated HTML", testTourTargetsExist),
    example("no getting-started screenshot is referenced", testNoScreenshotReferences),
    example("proxy cmd copy button has the ProxyCmdCopyBtn id", testProxyCmdCopyBtnId),
    example("proxy clear button has the ProxyClearBtn id", testProxyClearBtnId),
    example("header has the AppHeader id", testAppHeaderId),
    example("address bar carries the port lock", testProxyPortLockId),
    example("address bar carries the request filter button and its count badge", testRequestFilterButtonIds),
    example("request filter modal shell is emitted hidden", testRequestFilterModalIds),
    example("status bar elements have their ids", testStatusBarElementIds),
    example("route segments carry their data-route attributes", testRouteSegDataAttrs),
    example("the Global route segment is the active default (and Manual is not)", testRouteSegDefaultActive),
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
    "onboard.note"             -> "note",
    "onboard.skip"             -> "skip",
    "onboard.replay"           -> "replay",
    "onboard.tourBtn"          -> "tourBtn",
    "onboard.motionAlt"        -> "motionAlt",
    "onboard.motion.prompt"    -> "prompt",
    "onboard.motion.cap1"      -> "cap1",
    "onboard.motion.cap2"      -> "cap2",
    "onboard.motion.cap3"      -> "cap3",
    "onboard.motion.captured"  -> "captured",
    "onboard.motion.tokens"    -> "tokens",
    "onboard.motion.tagline"   -> "tagline",
    "proxy.stopProxy"          -> "stopProxy",
    "header.logoSub"           -> "Proxy",
    "proxy.port"               -> "port",
    "proxy.startFirst"         -> "startFirst",
    "proxy.startProxy"         -> "startProxy",
    "proxy.portLocked"         -> "portLocked",
    "proxy.clear"              -> "clear",
    "proxy.aboutTitle"         -> "about",
    "proxy.capturedRequests"   -> "captured",
    "proxy.noCapturesTitle"    -> "noCapturesTitle",
    "proxy.selectRequestTitle" -> "selectRequestTitle",
    "proxy.selectRequestHint"  -> "selectRequestHint",
    "status.stopped"           -> "stopped",
    "status.running"           -> "running",
    "mask.switchLabel"         -> "maskSecrets",
    "mask.switchTitleOn"       -> "maskTitleOn",
    "route.label"              -> "routeClaude",
    "route.manual"             -> "manual",
    "route.vscode"             -> "vscode",
    "route.global"             -> "global",
    "route.manualTitle"        -> "manualTitle",
    "route.vscodeTitle"        -> "vscodeTitle",
    "route.globalTitle"        -> "globalTitle",
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

  def testOnboardControlIds: Result =
    Result.all(
      List(HtmlIds.OnboardStage, HtmlIds.OnboardSkipBtn, HtmlIds.OnboardReplayBtn, HtmlIds.OnboardTourBtn).map { id =>
        Result
          .assert(rendered.contains(s"""id="$id""""))
          .log(s"`id=\"$id\"` missing from generated HTML")
      }
    )

  def testTourLayerShell: Result =
    Result.all(
      List(
        Result
          .assert(rendered.contains(s"""id="${HtmlIds.TourLayer}" class="tour-layer" style="display:none""""))
          .log(s"hidden `${HtmlIds.TourLayer}` shell missing from generated HTML"),
        Result
          .assert(rendered.contains(s"""id="${HtmlIds.TourSpot}""""))
          .log(s"`id=\"${HtmlIds.TourSpot}\"` missing from generated HTML"),
        Result
          .assert(rendered.contains(s"""id="${HtmlIds.TourCallout}""""))
          .log(s"`id=\"${HtmlIds.TourCallout}\"` missing from generated HTML"),
      )
    )

  def testTourTargetsExist: Result =
    Result.all(
      GettingStarted.tourSteps.map { step =>
        Result
          .assert(rendered.contains(s"""id="${step.targetId}""""))
          .log(s"tour target `id=\"${step.targetId}\"` missing from generated HTML")
      }
    )

  def testNoScreenshotReferences: Result =
    Result
      .assert(!rendered.contains("getting-started-0"))
      .log("the generated HTML still references a getting-started screenshot")

  def testProxyCmdCopyBtnId: Result =
    Result
      .assert(rendered.contains(s"""id="${HtmlIds.ProxyCmdCopyBtn}""""))
      .log(s"`id=\"${HtmlIds.ProxyCmdCopyBtn}\"` missing from generated HTML")

  def testProxyClearBtnId: Result =
    Result
      .assert(rendered.contains(s"""id="${HtmlIds.ProxyClearBtn}""""))
      .log(s"`id=\"${HtmlIds.ProxyClearBtn}\"` missing from generated HTML")

  def testAppHeaderId: Result =
    Result
      .assert(rendered.contains(s"""id="${HtmlIds.AppHeader}""""))
      .log(s"`id=\"${HtmlIds.AppHeader}\"` missing from generated HTML")

  def testProxyPortLockId: Result =
    Result
      .assert(rendered.contains(s"""id="${HtmlIds.ProxyPortLock}""""))
      .log(s"`id=\"${HtmlIds.ProxyPortLock}\"` missing from generated HTML")

  def testRequestFilterButtonIds: Result =
    Result.all(
      List(
        Result
          .assert(rendered.contains(s"""id="${HtmlIds.RequestFilterBtn}""""))
          .log(s"`id=\"${HtmlIds.RequestFilterBtn}\"` missing from generated HTML"),
        Result
          .assert(rendered.contains(s"""id="${HtmlIds.RequestFilterCount}""""))
          .log(s"`id=\"${HtmlIds.RequestFilterCount}\"` missing from generated HTML"),
        Result
          .assert(rendered.contains("""data-i18n="filter.button""""))
          .log("filter button label should be translatable"),
      )
    )

  def testRequestFilterModalIds: Result =
    Result.all(
      List(
        Result
          .assert(
            rendered.contains(s"""id="${HtmlIds.RequestFilterModal}" class="onboard-overlay" style="display:none"""")
          )
          .log(s"hidden `${HtmlIds.RequestFilterModal}` overlay missing from generated HTML"),
        Result
          .assert(rendered.contains(s"""id="${HtmlIds.RequestFilterCard}""""))
          .log(s"`id=\"${HtmlIds.RequestFilterCard}\"` missing from generated HTML"),
      )
    )

  def testStatusBarElementIds: Result =
    Result.all(
      List(
        HtmlIds.StatusBar,
        HtmlIds.StatusPort,
        HtmlIds.StatusReqCount,
        HtmlIds.MaskToggleBtn,
        HtmlIds.RouteSeg,
        HtmlIds.ProxyInfoBtn,
      ).map { id =>
        Result
          .assert(rendered.contains(s"""id="$id""""))
          .log(s"`id=\"$id\"` missing from generated HTML")
      }
    )

  def testRouteSegDataAttrs: Result =
    Result.all(List("manual", "vscode", "global").map { mode =>
      Result
        .assert(rendered.contains(s"""data-route="$mode""""))
        .log(s"`data-route=\"$mode\"` missing from generated HTML")
    })

  def testRouteSegDefaultActive: Result =
    Result.all(
      List(
        Result
          .assert(rendered.contains("""<button class="seg-btn active" data-route="global""""))
          .log("the `global` route segment should be rendered active by default"),
        Result
          .assert(rendered.contains("""<button class="seg-btn" data-route="manual""""))
          .log("the `manual` route segment should NOT be rendered active"),
      )
    )

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
