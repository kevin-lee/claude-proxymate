package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object IndexHtmlGeneratorSpec extends Properties {

  override def tests: List[Test] = List(
    example("no inline event handler attributes are emitted", testNoInlineEventHandlers),
    example("onboard close button has the OnboardCloseBtn id", testOnboardCloseBtnId),
    example("proxy cmd copy button has the ProxyCmdCopyBtn id", testProxyCmdCopyBtnId),
    example("proxy clear button has the ProxyClearBtn id", testProxyClearBtnId),
    example("copy detail button has the CopyDetailBtn id", testCopyDetailBtnId),
    example("existing button ids are preserved", testExistingButtonIdsPreserved),
    example("dtab buttons carry their data-dtab attributes", testDtabDataAttrs),
  )

  private val sampleLocale: Map[String, String] = Map(
    "onboard.title"          -> "title",
    "onboard.sub"            -> "sub",
    "onboard.step1"          -> "step1",
    "onboard.step2"          -> "step2",
    "onboard.step3"          -> "step3",
    "onboard.note"           -> "note",
    "onboard.btn"            -> "btn",
    "header.logoSub"         -> "Proxy",
    "proxy.port"             -> "port",
    "proxy.stopped"          -> "stopped",
    "proxy.startFirst"       -> "startFirst",
    "proxy.startProxy"       -> "startProxy",
    "proxy.clear"            -> "clear",
    "proxy.interceptTitle"   -> "intercept",
    "proxy.capturedRequests" -> "captured",
    "proxy.noCapturesTitle"  -> "noCapturesTitle",
    "proxy.selectRequestTitle" -> "selectRequestTitle",
    "copy.copy"              -> "copy",
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
      Result.assert(!rendered.contains(attr))
        .log(s"unexpected inline handler attribute `$attr` in generated HTML")
    })

  def testOnboardCloseBtnId: Result =
    Result.assert(rendered.contains(s"""id="${HtmlIds.OnboardCloseBtn}""""))
      .log(s"`id=\"${HtmlIds.OnboardCloseBtn}\"` missing from generated HTML")

  def testProxyCmdCopyBtnId: Result =
    Result.assert(rendered.contains(s"""id="${HtmlIds.ProxyCmdCopyBtn}""""))
      .log(s"`id=\"${HtmlIds.ProxyCmdCopyBtn}\"` missing from generated HTML")

  def testProxyClearBtnId: Result =
    Result.assert(rendered.contains(s"""id="${HtmlIds.ProxyClearBtn}""""))
      .log(s"`id=\"${HtmlIds.ProxyClearBtn}\"` missing from generated HTML")

  def testCopyDetailBtnId: Result =
    Result.assert(rendered.contains(s"""id="${HtmlIds.CopyDetailBtn}""""))
      .log(s"`id=\"${HtmlIds.CopyDetailBtn}\"` missing from generated HTML")

  def testExistingButtonIdsPreserved: Result = {
    val expected = List(
      HtmlIds.ThemeToggleBtn,
      HtmlIds.LangToggleBtn,
      HtmlIds.ProxyStartBtn,
      HtmlIds.ProxyPort,
    )
    Result.all(expected.map { id =>
      Result.assert(rendered.contains(s"""id="$id""""))
        .log(s"`id=\"$id\"` missing from generated HTML")
    })
  }

  def testDtabDataAttrs: Result =
    Result.all(List("messages", "request", "response", "analysis").map { tab =>
      Result.assert(rendered.contains(s"""data-dtab="$tab""""))
        .log(s"`data-dtab=\"$tab\"` missing from generated HTML")
    })
}
