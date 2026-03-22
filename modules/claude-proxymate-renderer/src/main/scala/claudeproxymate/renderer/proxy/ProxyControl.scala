package claudeproxymate.renderer.proxy

import claudeproxymate.core.HtmlIds
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

  @JSExportTopLevel("updateProxyCmd")
  def updateProxyCmd(): Unit = {
    val portEl = dom.document.getElementById(HtmlIds.ProxyPort)
    val cmdEl  = dom.document.getElementById(HtmlIds.ProxyCmdText)
    if (portEl == null || cmdEl == null || !AppState.proxyRunning) return

    val port = portEl.asInstanceOf[dom.html.Input].value match {
      case s if s.nonEmpty => s
      case _               => "8888"
    }
    cmdEl.asInstanceOf[dom.html.Element].style.color = "var(--green)"
    cmdEl.textContent = s"ANTHROPIC_BASE_URL=http://localhost:$port claude"
  }

  @JSExportTopLevel("renderProxyStatus")
  def renderProxyStatus(): Unit = {
    val status   = dom.document.getElementById(HtmlIds.ProxyStatus)
    val statusTx = dom.document.getElementById(HtmlIds.ProxyStatusText)
    val startBtn = dom.document.getElementById(HtmlIds.ProxyStartBtn)
    val cmdEl    = dom.document.getElementById(HtmlIds.ProxyCmdText)
    if (status == null || startBtn == null) return

    val statusEl = status.asInstanceOf[dom.html.Element]
    val btnEl    = startBtn.asInstanceOf[dom.html.Button]

    if (AppState.proxyRunning) {
      locally { val _ = statusEl.classList.add("running") }
      if (statusTx != null) statusTx.textContent = I18n.t("proxy.running", Map("port" -> AppState.proxyActualPort.toString))
      btnEl.textContent = I18n.t("proxy.stopProxy")
      btnEl.style.background = "var(--red)"
      if (cmdEl != null) {
        cmdEl.asInstanceOf[dom.html.Element].style.color = "var(--green)"
        cmdEl.textContent = s"ANTHROPIC_BASE_URL=http://localhost:${AppState.proxyActualPort} claude"
      }
    } else {
      locally { val _ = statusEl.classList.remove("running") }
      if (statusTx != null) statusTx.textContent = I18n.t("proxy.stopped")
      btnEl.textContent = I18n.t("proxy.startProxy")
      btnEl.style.background = "var(--blue)"
      if (cmdEl != null) {
        cmdEl.asInstanceOf[dom.html.Element].style.color = "var(--dim)"
        cmdEl.textContent = I18n.t("proxy.startFirst")
      }
    }
  }

  @JSExportTopLevel("toggleProxy")
  def toggleProxy(): Unit = {
    ElectronApi.get match {
      case None      => ()
      case Some(api) => doToggleProxy(api)
    }
  }

  private def doToggleProxy(api: ElectronApi): Unit = {
    val btn = dom.document.getElementById(HtmlIds.ProxyStartBtn)
    if (btn != null) {
      btn.asInstanceOf[dom.html.Button].disabled = true
      btn.innerHTML = """<span class="spin"></span>"""
    }

    val promise: js.Promise[Unit] =
      if (AppState.proxyRunning) {
        api.proxyStop().`then`[Unit] { (_: js.Dynamic) =>
          api.offProxy()
          AppState.proxyRunning = false
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
            dom.window.alert(I18n.t("proxy.startFail") + result.error.toString)
          } else {
            AppState.proxyRunning = true
            AppState.proxyActualPort = result.port.asInstanceOf[Int]
            val portInput = dom.document.getElementById(HtmlIds.ProxyPort)
            if (portInput != null) portInput.asInstanceOf[dom.html.Input].value = result.port.toString

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
                if (selId == id.asInstanceOf[Double]) {
                  debouncedRenderDetail(() => callRenderProxyDetail())
                }
              }
            }
          }
        }
      }

    val onSuccess: js.Function1[Unit, Unit] = { (_: Unit) =>
      if (btn != null) btn.asInstanceOf[dom.html.Button].disabled = false
      renderProxyStatus()
    }
    val onError: js.Function1[scala.Any, Unit] = { (err: scala.Any) =>
      dom.console.error("toggleProxy error:", err.asInstanceOf[js.Any])
      dom.window.alert(I18n.t("proxy.startFail") + err.toString)
      if (btn != null) btn.asInstanceOf[dom.html.Button].disabled = false
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
