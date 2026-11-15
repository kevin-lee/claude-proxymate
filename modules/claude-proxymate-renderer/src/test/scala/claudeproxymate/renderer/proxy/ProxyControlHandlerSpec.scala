package claudeproxymate.renderer.proxy

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*
import org.scalajs.dom

/** The click and input handlers of [[ProxyControl]] and
  * `renderProxyStatus`, in jsdom. There is no `window.electronAPI`, so a
  * click on Start Proxy does nothing.
  */
object ProxyControlHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("a click on Start Proxy with no Electron API changes nothing", testStartNoApi),
    example("input in the port input shows the port in the status bar and stores a valid port", testPortInput),
    example("input of a port that is not valid shows it but does not store it", testBadPortInput),
    example("input in another input changes nothing", testOtherInput),
    example("renderProxyStatus with no status element changes nothing", testRenderNoStatus),
    example("renderProxyStatus with no Start Proxy button changes nothing", testRenderNoButton),
    example("renderProxyStatus shows the running state", testRenderRunning),
    example("renderProxyStatus shows the stopped state", testRenderStopped),
  )

  private val Page: String =
    s"""<span id="${HtmlIds.ProxyStatus}"><span id="${HtmlIds.ProxyStatusText}">-</span></span><button id="${HtmlIds.ProxyStartBtn}">Start</button>
       |<input id="${HtmlIds.ProxyPort}" value="8888"><span id="${HtmlIds.ProxyPortLock}" class="u-hide"></span><span id="${HtmlIds.StatusPort}">8888</span><input id="other-input">""".stripMargin

  private def storedPort: Option[String] = Option(dom.window.localStorage.getItem("ci-port"))

  private def portInput: dom.html.Input = DomTestKit.html(s"#${HtmlIds.ProxyPort}").asInstanceOf[dom.html.Input] // scalafix:ok DisableSyntax.asInstanceOf

  def testStartNoApi: Result = {
    DomTestKit.setUp(Page)
    val _ = DomTestKit.click(DomTestKit.element(s"#${HtmlIds.ProxyStartBtn}"))
    Result.all(
      List(
        DomTestKit.element(s"#${HtmlIds.ProxyStartBtn}").textContent ==== "Start",
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testPortInput: Result = {
    DomTestKit.setUp(Page)
    dom.window.localStorage.removeItem("ci-port")
    val _ = DomTestKit.typeInto(portInput, "9000")
    Result.all(
      List(
        DomTestKit.element(s"#${HtmlIds.StatusPort}").textContent ==== "9000",
        storedPort ==== Some("9000"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testBadPortInput: Result = {
    DomTestKit.setUp(Page)
    dom.window.localStorage.removeItem("ci-port")
    val _ = DomTestKit.typeInto(portInput, "80")
    Result.all(
      List(
        DomTestKit.element(s"#${HtmlIds.StatusPort}").textContent ==== "80",
        storedPort ==== None,
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testOtherInput: Result = {
    DomTestKit.setUp(Page)
    dom.window.localStorage.removeItem("ci-port")
    val _ = DomTestKit.typeInto(DomTestKit.html("#other-input").asInstanceOf[dom.html.Input], "9000") // scalafix:ok DisableSyntax.asInstanceOf
    Result.all(
      List(
        DomTestKit.element(s"#${HtmlIds.StatusPort}").textContent ==== "8888",
        storedPort ==== None,
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testRenderNoStatus: Result = {
    DomTestKit.setUp(s"""<button id="${HtmlIds.ProxyStartBtn}">Start</button>""")
    AppState.proxyRunning = true
    ProxyControl.renderProxyStatus()
    Result.all(
      List(
        DomTestKit.element(s"#${HtmlIds.ProxyStartBtn}").textContent ==== "Start",
        Result.assert(!DomTestKit.element(s"#${HtmlIds.ProxyStartBtn}").classList.contains("running")).log("no running class"),
      )
    )
  }

  def testRenderNoButton: Result = {
    DomTestKit.setUp(s"""<span id="${HtmlIds.ProxyStatus}"><span id="${HtmlIds.ProxyStatusText}">-</span></span>""")
    AppState.proxyRunning = true
    ProxyControl.renderProxyStatus()
    Result.all(
      List(
        DomTestKit.element(s"#${HtmlIds.ProxyStatusText}").textContent ==== "-",
        Result.assert(!DomTestKit.element(s"#${HtmlIds.ProxyStatus}").classList.contains("running")).log("no running class"),
      )
    )
  }

  def testRenderRunning: Result = {
    DomTestKit.setUp(Page)
    AppState.proxyRunning = true
    ProxyControl.renderProxyStatus()
    Result.all(
      List(
        Result.assert(DomTestKit.element(s"#${HtmlIds.ProxyStatus}").classList.contains("running")).log("status running"),
        DomTestKit.element(s"#${HtmlIds.ProxyStatusText}").textContent ==== "status.running",
        DomTestKit.element(s"#${HtmlIds.ProxyStartBtn}").textContent ==== "proxy.stopProxy",
        portInput.disabled ==== true,
        Result.assert(!DomTestKit.element(s"#${HtmlIds.ProxyPortLock}").classList.contains("u-hide")).log("lock shown"),
      )
    )
  }

  def testRenderStopped: Result = {
    DomTestKit.setUp(Page)
    AppState.proxyRunning = true
    ProxyControl.renderProxyStatus()
    AppState.proxyRunning = false
    ProxyControl.renderProxyStatus()
    Result.all(
      List(
        Result.assert(!DomTestKit.element(s"#${HtmlIds.ProxyStatus}").classList.contains("running")).log("status stopped"),
        DomTestKit.element(s"#${HtmlIds.ProxyStatusText}").textContent ==== "status.stopped",
        DomTestKit.element(s"#${HtmlIds.ProxyStartBtn}").textContent ==== "proxy.startProxy",
        portInput.disabled ==== false,
        Result.assert(DomTestKit.element(s"#${HtmlIds.ProxyPortLock}").classList.contains("u-hide")).log("lock hidden"),
      )
    )
  }
}
