package claudeproxymate.renderer.proxy

import cats.syntax.all.*
import claudeproxymate.core.{HtmlIds, VsCodeEnv}
import claudeproxymate.renderer.facades.ElectronApi
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.Debounce
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Proxy lifecycle: start/stop, status display, command text.
  *
  * Ports `updateProxyCmd`, `renderProxyStatus`, `toggleProxy`,
  * and the debounced render helpers from renderer.js.
  */
object ProxyControl {

  private val debouncedRenderList   = new Debounce(50)
  private val debouncedRenderDetail = new Debounce(100)

  /** A truthful proxy state change decoded from the main process's
    * `proxy-state` push (which itself relays the binary's protocol
    * events) — unlike `proxyStart`'s optimistic spawn-time return.
    */
  enum ProxyStateEvent {
    case Started(port: Int)
    case Stopped
    case ErrorState(message: String)
  }

  def parseStateEvent(state: String, port: Int, message: String): Option[ProxyStateEvent] =
    state match {
      case "started" => ProxyStateEvent.Started(port).some
      case "stopped" => ProxyStateEvent.Stopped.some
      case "error" => ProxyStateEvent.ErrorState(message).some
      case _ => none[ProxyStateEvent]
    }

  private val PortStorageKey = "ci-port"

  /** Install doc-level listeners for the port input (`#proxyPort`) and
    * the start/stop proxy button (`#proxyStartBtn`), plus the
    * `proxy-state` event subscription that owns `AppState.proxyRunning`.
    * Also restores the persisted port.
    */
  def install(): Unit = {
    loadPersistedPort()
    dom.document.addEventListener("click", handleClick(_))
    dom.document.addEventListener("input", handleInput(_))
    ElectronApi.get.foreach { api =>
      api.onProxyState { (payload: js.Dynamic) =>
        handleStateEvent(payload)
      }
    }
  }

  /** Restore the persisted port (localStorage, same convention as
    * `ci-theme` / `ci-lang`) into the address-bar input and the
    * status-bar port display.
    */
  private def loadPersistedPort(): Unit = {
    val stored = dom.window.localStorage.getItem(PortStorageKey)
    if (stored != null) {
      parsePort(stored).foreach { port =>
        val portEl = dom.document.getElementById(HtmlIds.ProxyPort)
        if (portEl != null) portEl.asInstanceOf[dom.html.Input].value = port.toString
        val statusPort = dom.document.getElementById(HtmlIds.StatusPort)
        if (statusPort != null) statusPort.textContent = port.toString
      }
    } else ()
  }

  private def parsePort(value: String): Option[Int] =
    (try value.toInt.some
    catch { case _: Throwable => none[Int] }).filter(port => port >= 1024 && port <= 65535)

  /** Pending-toggle watchdog: if neither a state event nor an invoke
    * failure arrives (e.g. an old binary that emits nothing), reconcile
    * with `proxyStatus()` instead of leaving the button disabled forever.
    */
  private var watchdogId: Option[Int] = none[Int]

  private def armWatchdog(): Unit = {
    cancelWatchdog()
    watchdogId = Some(
      dom.window.setTimeout(
        () => {
          watchdogId = none[Int]
          reconcileViaStatus()
        },
        10000,
      )
    )
  }

  private def cancelWatchdog(): Unit = {
    watchdogId.foreach(id => dom.window.clearTimeout(id))
    watchdogId = none[Int]
  }

  private def reconcileViaStatus(): Unit =
    ElectronApi.get.foreach { api =>
      val _ = api.proxyStatus().`then`[Unit] { (st: js.Dynamic) =>
        AppState.proxyRunning = st.running.asInstanceOf[Boolean]
        if (AppState.proxyRunning) {
          AppState.proxyActualPort = st.port.asInstanceOf[Int]
        } else ()
        enableStartBtn()
        renderProxyStatus()
      }
    }

  private def handleStateEvent(payload: js.Dynamic): Unit = {
    val state   = asString(payload.selectDynamic("state"))
    val portRaw = payload.selectDynamic("port")
    val port    = if (js.typeOf(portRaw) === "number") portRaw.asInstanceOf[Int] else 0
    val message = asString(payload.selectDynamic("message"))
    parseStateEvent(state, port, message) match {
      case Some(ProxyStateEvent.Started(actualPort)) =>
        cancelWatchdog()
        AppState.proxyRunning = true
        AppState.proxyActualPort = actualPort
        val portInput = dom.document.getElementById(HtmlIds.ProxyPort)
        if (portInput != null) {
          portInput.asInstanceOf[dom.html.Input].value = actualPort.toString
        } else ()
        /* The actually bound port is the truth — persist it. */
        dom.window.localStorage.setItem(PortStorageKey, actualPort.toString)
        enableStartBtn()
        renderProxyStatus()
      case Some(ProxyStateEvent.Stopped) =>
        cancelWatchdog()
        AppState.proxyRunning = false
        enableStartBtn()
        renderProxyStatus()
      case Some(ProxyStateEvent.ErrorState(msg)) =>
        cancelWatchdog()
        AppState.proxyRunning = false
        enableStartBtn()
        renderProxyStatus()
        dom.window.alert(I18n.t("proxy.startFail") + msg)
      case None =>
        ()
    }
  }

  private def enableStartBtn(): Unit = {
    val btn = dom.document.getElementById(HtmlIds.ProxyStartBtn)
    if (btn != null) btn.asInstanceOf[dom.html.Button].disabled = false else ()
  }

  private def asString(value: js.Dynamic): String =
    if (js.typeOf(value) === "string") value.asInstanceOf[String] else ""

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    if (target.closest(s"#${HtmlIds.ProxyStartBtn}") != null) toggleProxy()
  }

  /** Port edits only happen while stopped (the input is disabled while
    * running): mirror the value into the status bar and persist it.
    */
  private def handleInput(e: dom.Event): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null || target.id =!= HtmlIds.ProxyPort) return
    val value      = target.asInstanceOf[dom.html.Input].value
    val statusPort = dom.document.getElementById(HtmlIds.StatusPort)
    if (statusPort != null) statusPort.textContent = value else ()
    parsePort(value).foreach { port =>
      dom.window.localStorage.setItem(PortStorageKey, port.toString)
    }
  }

  /** The copy-paste CLI command. Uses the same base URL builder as the
    * VS Code settings sync so the two can never drift.
    */
  private def commandText(port: String): String = {
    val parsedPort = try port.toInt
    catch { case _: Throwable => 8888 }
    s"${VsCodeEnv.EnvVarName}=${VsCodeEnv.baseUrl(parsedPort)} claude"
  }

  /** Render every state-dependent piece of the new layout: the
    * status-bar status/port/request-count, the address-bar Start/Stop
    * button, the port lock, and the command box. Colors come from CSS
    * classes (`running` modifiers), not inline styles.
    */
  @JSExportTopLevel("renderProxyStatus")
  def renderProxyStatus(): Unit = {
    val status   = dom.document.getElementById(HtmlIds.ProxyStatus)
    val statusTx = dom.document.getElementById(HtmlIds.ProxyStatusText)
    val startBtn = dom.document.getElementById(HtmlIds.ProxyStartBtn)
    if (status == null || startBtn == null) return

    val statusEl = status.asInstanceOf[dom.html.Element]
    val btnEl    = startBtn.asInstanceOf[dom.html.Button]
    val running  = AppState.proxyRunning

    if (running) {
      locally { val _ = statusEl.classList.add("running") }
    } else {
      locally { val _ = statusEl.classList.remove("running") }
    }
    if (statusTx != null) {
      statusTx.textContent = I18n.t(if (running) "status.running" else "status.stopped")
    } else ()

    btnEl.textContent = I18n.t(if (running) "proxy.stopProxy" else "proxy.startProxy")
    if (running) {
      locally { val _ = btnEl.classList.add("running") }
    } else {
      locally { val _ = btnEl.classList.remove("running") }
    }

    val portEl = dom.document.getElementById(HtmlIds.ProxyPort)
    if (portEl != null) {
      val input = portEl.asInstanceOf[dom.html.Input]
      input.disabled = running
      if (running) input.title = I18n.t("proxy.portLocked") else input.removeAttribute("title")
    } else ()
    val lockEl = dom.document.getElementById(HtmlIds.ProxyPortLock)
    if (lockEl != null) {
      if (running) {
        locally { val _ = lockEl.classList.remove("u-hide") }
      } else {
        locally { val _ = lockEl.classList.add("u-hide") }
      }
    } else ()

    val statusPort = dom.document.getElementById(HtmlIds.StatusPort)
    if (statusPort != null) {
      statusPort.textContent =
        if (running) AppState.proxyActualPort.toString
        else if (portEl != null) portEl.asInstanceOf[dom.html.Input].value
        else AppState.proxyActualPort.toString
    } else ()

    val reqCount = dom.document.getElementById(HtmlIds.StatusReqCount)
    if (reqCount != null) {
      if (running) {
        locally { val _ = reqCount.classList.remove("u-hide") }
      } else {
        locally { val _ = reqCount.classList.add("u-hide") }
      }
    } else ()
    renderReqCount()

    val cmdBox = dom.document.getElementById(HtmlIds.ProxyCmdBox)
    if (cmdBox != null) {
      if (running) {
        locally { val _ = cmdBox.classList.add("running") }
      } else {
        locally { val _ = cmdBox.classList.remove("running") }
      }
    } else ()
    val cmdEl  = dom.document.getElementById(HtmlIds.ProxyCmdText)
    if (cmdEl != null) {
      cmdEl.textContent =
        if (running) commandText(AppState.proxyActualPort.toString)
        else I18n.t("proxy.startFirst")
    } else ()
    val copyBtn = dom.document.getElementById(HtmlIds.ProxyCmdCopyBtn)
    if (copyBtn != null) copyBtn.asInstanceOf[dom.html.Button].disabled = !running else ()
  }

  /** Status-bar request counter ("{count} requests"); text is
    * locale-dependent, visibility is owned by [[renderProxyStatus]].
    */
  def renderReqCount(): Unit = {
    val el = dom.document.getElementById(HtmlIds.StatusReqCount)
    if (el != null) {
      el.textContent = I18n.t("status.requests", Map("count" -> AppState.proxyCaptures.length.toString))
    } else ()
  }

  def toggleProxy(): Unit = {
    ElectronApi.get match {
      case None => ()
      case Some(api) => doToggleProxy(api)
    }
  }

  /** Start/stop the proxy. `AppState.proxyRunning` is NOT set here: the
    * truthful `proxy-state` events own the state (see
    * [[handleStateEvent]]), so "Running" appears only after the binary
    * actually bound its port, and a bind/spawn failure surfaces as an
    * error alert instead of a lying status. The button stays disabled
    * (spinner) until an event or the watchdog resolves the toggle.
    */
  private def doToggleProxy(api: ElectronApi): Unit = {
    val btn = dom.document.getElementById(HtmlIds.ProxyStartBtn)
    if (btn != null) {
      btn.asInstanceOf[dom.html.Button].disabled = true
      btn.innerHTML = """<span class="spin"></span>"""
    }
    armWatchdog()

    val promise: js.Promise[Unit] =
      if (AppState.proxyRunning) {
        api.proxyStop().`then`[Unit] { (_: js.Dynamic) =>
          api.offProxy()
          /* state flips on the "stopped" event (child exit) */
        }
      } else {
        // Clean up existing listeners before registering new ones
        api.offProxy()

        val portEl = dom.document.getElementById(HtmlIds.ProxyPort)
        val port   = if (portEl != null) {
          try portEl.asInstanceOf[dom.html.Input].value.toInt
          catch { case _: Throwable => 8888 }
        } else 8888

        api.proxyStart(port).`then`[Unit] { (result: js.Dynamic) =>
          if (!js.isUndefined(result.error) && result.error != null) {
            cancelWatchdog()
            dom.window.alert(I18n.t("proxy.startFail") + result.error.toString)
            enableStartBtn()
            renderProxyStatus()
          } else {
            /* running state + true port arrive via the "started" event */
            api.onProxyRequest { (data: js.Dynamic) =>
              AppState.proxyCaptures = data :: AppState.proxyCaptures
              if (AppState.proxyCaptures.length > 50) {
                AppState.proxyCaptures = AppState.proxyCaptures.take(50)
              }
              debouncedRenderList(() => ProxyList.renderProxyList())
            }
            api.onProxyResponse { (data: js.Dynamic) =>
              val id = data.id
              AppState.proxyCaptures = AppState.proxyCaptures.map { e =>
                if (e.id == id) {
                  e.response = data
                  e
                } else e
              }
              debouncedRenderList(() => ProxyList.renderProxyList())
              AppState.selectedProxyId.foreach { selId =>
                if (selId === id.asInstanceOf[Double]) {
                  debouncedRenderDetail(() => callRenderProxyDetail())
                }
              }
            }
          }
        }
      }

    val onSuccess: js.Function1[Unit, Unit]    = { (_: Unit) =>
      () // button stays disabled until a proxy-state event or the watchdog
    }
    val onError: js.Function1[scala.Any, Unit] = { (err: scala.Any) =>
      cancelWatchdog()
      dom.console.error("toggleProxy error:", err.asInstanceOf[js.Any])
      dom.window.alert(I18n.t("proxy.startFail") + err.toString)
      enableStartBtn()
      renderProxyStatus()
    }
    locally { val _ = promise.`then`(onSuccess, onError) }
  }

  /** Trigger debounced proxy list render. */
  def triggerDebouncedRenderList(): Unit =
    debouncedRenderList(() => ProxyList.renderProxyList())

  /** Trigger debounced proxy detail render. */
  def triggerDebouncedRenderDetail(): Unit =
    debouncedRenderDetail(() => callRenderProxyDetail())

  /** Calls renderProxyDetail from DetailView. */
  private[proxy] def callRenderProxyDetail(): Unit =
    claudeproxymate.renderer.detail.DetailView.renderProxyDetail()
}
