package claudeproxymate.renderer.copy

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*

import scala.scalajs.js

/** The click handler of [[CopyUtil]] and the copy methods, in jsdom. The
  * clipboard stand-in records the text at once. The flash of the button
  * runs in a promise, so it is not checked.
  */
object CopyUtilHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("the command copy button copies the command when the proxy runs", testCopyCmd),
    example("the command copy button copies nothing when the proxy is stopped", testCopyCmdStopped),
    example("the command copy button copies nothing when there is no command text", testCopyCmdNoText),
    example("the command copy button inside the detail copy button copies only the command", testCopyCmdOnly),
    example("the detail copy button copies nothing when no capture is selected", testDetailNoCapture),
    example("the detail copy button copies the request body with masks", testDetailRequest),
    example("the detail copy button copies a JSON string body with masks", testDetailJsonString),
    example("the detail copy button copies a string body that is not JSON as it is", testDetailNotJson),
    example("the detail copy button copies nothing on the response tab with no response", testDetailNoResponse),
    example("the detail copy button copies the visible messages on the messages tab", testDetailMessages),
    example("the detail copy button copies nothing when there are no messages", testDetailNoMessages),
    example("a click outside the copy buttons copies nothing", testOutside),
  )

  private val Page: String =
    s"""<span id="${HtmlIds.CopyDetailBtn}"><span id="${HtmlIds.ProxyCmdCopyBtn}">c</span></span><span id="${HtmlIds.ProxyCmdText}">export X=1</span><i id="other"></i>"""

  private def requestBody: js.Dynamic =
    js.JSON.parse("""{"api_key":"secret-value","model":"m"}""")

  private def setUpCapture(requestBody: js.Any, responseBody: js.Any, tab: String): Unit = {
    DomTestKit.setUp(Page)
    DomTestKit.select(DomTestKit.capture(1, requestBody, responseBody))
    AppState.proxyDetailTab = tab
  }

  private def clickDetailCopy(): Unit = {
    /* The command button is inside the detail button: a click on the
     * detail button itself does not reach the command button. */
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.CopyDetailBtn}"))
  }

  def testCopyCmd: Result = {
    DomTestKit.setUp(Page)
    AppState.proxyRunning = true
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ProxyCmdCopyBtn}"))
    Result.all(List(DomTestKit.clipboardTexts ==== List("export X=1"), DomTestKit.listenerErrors ==== Nil))
  }

  def testCopyCmdStopped: Result = {
    DomTestKit.setUp(Page)
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ProxyCmdCopyBtn}"))
    Result.all(List(DomTestKit.clipboardTexts ==== Nil, DomTestKit.listenerErrors ==== Nil))
  }

  def testCopyCmdNoText: Result = {
    DomTestKit.setUp(s"""<button id="${HtmlIds.ProxyCmdCopyBtn}">c</button>""")
    AppState.proxyRunning = true
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ProxyCmdCopyBtn}"))
    Result.all(List(DomTestKit.clipboardTexts ==== Nil, DomTestKit.listenerErrors ==== Nil))
  }

  def testCopyCmdOnly: Result = {
    setUpCapture(requestBody, js.Dynamic.literal(), "request")
    AppState.proxyRunning = true
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ProxyCmdCopyBtn}"))
    Result.all(List(DomTestKit.clipboardTexts ==== List("export X=1"), DomTestKit.listenerErrors ==== Nil))
  }

  def testDetailNoCapture: Result = {
    DomTestKit.setUp(Page)
    clickDetailCopy()
    Result.all(List(DomTestKit.clipboardTexts ==== Nil, DomTestKit.listenerErrors ==== Nil))
  }

  def testDetailRequest: Result = {
    setUpCapture(requestBody, js.Dynamic.literal(), "request")
    clickDetailCopy()
    val texts = DomTestKit.clipboardTexts
    Result.all(
      List(
        texts.length ==== 1,
        Result.assert(texts.forall(t => t.contains("api_key") && !t.contains("secret-value"))).log(s"$texts"),
        Result.assert(texts.forall(_.contains("\"model\": \"m\""))).log(s"$texts"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testDetailJsonString: Result = {
    setUpCapture("""{"api_key":"secret-value","model":"m"}""", js.Dynamic.literal(), "request")
    clickDetailCopy()
    val texts = DomTestKit.clipboardTexts
    Result.all(
      List(
        texts.length ==== 1,
        Result.assert(texts.forall(t => t.contains("api_key") && !t.contains("secret-value"))).log(s"$texts"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testDetailNotJson: Result = {
    setUpCapture("not json {", js.Dynamic.literal(), "request")
    clickDetailCopy()
    Result.all(List(DomTestKit.clipboardTexts ==== List("not json {"), DomTestKit.listenerErrors ==== Nil))
  }

  def testDetailNoResponse: Result = {
    DomTestKit.setUp(Page)
    AppState.proxyCaptures = List(js.Dynamic.literal(id = 1, method = "POST", url = "/v1/messages", body = requestBody))
    AppState.selectedProxyId = Some(1.0)
    AppState.proxyDetailTab = "response"
    clickDetailCopy()
    Result.all(List(DomTestKit.clipboardTexts ==== Nil, DomTestKit.listenerErrors ==== Nil))
  }

  def testDetailMessages: Result = {
    setUpCapture(
      js.JSON.parse("""{"messages":[{"role":"user","content":"hello there"}]}"""),
      js.Dynamic.literal(),
      "messages"
    )
    clickDetailCopy()
    val texts = DomTestKit.clipboardTexts
    Result.all(
      List(
        texts.length ==== 1,
        Result.assert(texts.forall(_.contains("hello there"))).log(s"$texts"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testDetailNoMessages: Result = {
    setUpCapture(js.JSON.parse("""{"messages":[]}"""), js.Dynamic.literal(), "messages")
    clickDetailCopy()
    Result.all(List(DomTestKit.clipboardTexts ==== Nil, DomTestKit.listenerErrors ==== Nil))
  }

  def testOutside: Result = {
    setUpCapture(requestBody, js.Dynamic.literal(), "request")
    AppState.proxyRunning = true
    val _ = DomTestKit.click(DomTestKit.element("#other"))
    Result.all(List(DomTestKit.clipboardTexts ==== Nil, DomTestKit.listenerErrors ==== Nil))
  }
}
