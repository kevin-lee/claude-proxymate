package claudeproxymate.renderer

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.detail.TokenPopover
import claudeproxymate.renderer.facades.ElectronApi
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.messages.MessageRenderer
import claudeproxymate.renderer.onboarding.Onboarding
import claudeproxymate.renderer.proxy.ProxyControl
import claudeproxymate.renderer.proxy.ProxyInfoPopover
import claudeproxymate.renderer.search.SearchNavigation
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.update.UpdateChecker
import org.scalajs.dom

import scala.scalajs.js

/** Scala.js renderer entry point for the Electron renderer process.
  *
  * Ports the Init section from renderer.js: keydown handlers,
  * platform detection, build-info fetch, applyI18n, proxy status sync,
  * onboarding, and token popover installation.
  */
object RendererMain {
  def main(args: Array[String]): Unit = {
    // Initialize _imeComposing global used by inline HTML event handlers.
    // Must use dom.window explicitly because js.Dynamic.global compiles to
    // bare `_imeComposing = ...` which throws ReferenceError in strict mode.
    dom.window.asInstanceOf[js.Dynamic]._imeComposing = false

    installKeydownHandler()
    detectPlatform()
    fetchBuildInfo()
    locally { val _ = I18n.loadLocales().`then`[Unit] { (_: Unit) => I18n.applyI18n() }
      .asInstanceOf[js.Dynamic].`catch`({ (_: Any) =>
        // If fetch fails (e.g. files not yet generated), HTML already has Korean defaults
        I18n.applyI18n()
      }: js.Function1[Any, Unit]) }
    showProxyUi()
    syncProxyStatus()
    TokenPopover.install()
    ProxyInfoPopover.install()
    Onboarding.showIfNeeded()
  }

  private def installKeydownHandler(): Unit = {
    dom.document.addEventListener("keydown", { (e: dom.KeyboardEvent) =>
      // Cmd+F / Ctrl+F: focus search input
      if ((e.metaKey || e.ctrlKey) && e.key == "f") {
        val proxyInp = dom.document.getElementById(HtmlIds.ProxyDetailSearchInput)
        val msgInp = dom.document.getElementById(HtmlIds.MsgSearchInput)
        val target = if (proxyInp != null) proxyInp else msgInp
        if (target != null) {
          e.preventDefault()
          target.asInstanceOf[dom.html.Input].focus()
          target.asInstanceOf[dom.html.Input].select()
        }
      }
      // Escape: clear search
      if (e.key == "Escape") {
        val proxyInp = dom.document.getElementById(HtmlIds.ProxyDetailSearchInput)
        val msgInp = dom.document.getElementById(HtmlIds.MsgSearchInput)
        if (proxyInp != null && dom.document.activeElement == proxyInp && AppState.proxyDetailSearch.nonEmpty) {
          SearchNavigation.setProxyDetailSearch("")
        } else if (msgInp != null && dom.document.activeElement == msgInp && AppState.msgSearchQuery.nonEmpty) {
          MessageRenderer.setMsgSearch("")
        }
      }
      // Enter / Shift+Enter: navigate search matches
      if (e.key == "Enter") {
        val proxyInp = dom.document.getElementById(HtmlIds.ProxyDetailSearchInput)
        if (proxyInp != null && dom.document.activeElement == proxyInp) {
          e.preventDefault()
          SearchNavigation.navigateSearchMatch(if (e.shiftKey) -1 else 1)
        }
      }
    })
  }

  private def detectPlatform(): Unit = {
    ElectronApi.get.foreach { api =>
      if (api.platform == "darwin") {
        locally { val _ = dom.document.body.classList.add("darwin") }
      }
    }
  }

  private def fetchBuildInfo(): Unit = {
    locally { val _ = dom.window.fetch("build-info.json")
      .`then`[js.Dynamic] { (r: dom.Response) => r.json().asInstanceOf[js.Promise[js.Dynamic]] }
      .`then`[Unit] { (b: js.Dynamic) =>
        val version = b.version.asInstanceOf[String]
        val hash = b.hash.asInstanceOf[String]
        val buildVer = dom.document.getElementById(HtmlIds.BuildVer)
        if (buildVer != null) buildVer.textContent = s"v$version ($hash)"
        UpdateChecker.checkForUpdate(version)
      }
      .asInstanceOf[js.Dynamic].`catch`({ (_: Any) => () }: js.Function1[Any, Unit]) }
  }

  private def showProxyUi(): Unit = {
    val bar = dom.document.getElementById(HtmlIds.ProxyBar)
    if (bar != null) bar.asInstanceOf[dom.html.Element].style.display = "flex"
    val panel = dom.document.getElementById(HtmlIds.ProxyPanel)
    if (panel != null) panel.asInstanceOf[dom.html.Element].style.display = "flex"
  }

  private def syncProxyStatus(): Unit = {
    ElectronApi.get.foreach { api =>
      val statusFn = dom.window.asInstanceOf[js.Dynamic].electronAPI.selectDynamic("proxyStatus")
      if (!js.isUndefined(statusFn) && statusFn != null) {
        locally { val _ = api.proxyStatus().`then`[Unit] { (st: js.Dynamic) =>
          if (st.running.asInstanceOf[Boolean]) {
            AppState.proxyRunning = true
            AppState.proxyActualPort = st.port.asInstanceOf[Int]
            val portInput = dom.document.getElementById(HtmlIds.ProxyPort)
            if (portInput != null) portInput.asInstanceOf[dom.html.Input].value = st.port.toString

            // Re-register event listeners
            api.offProxy()
            api.onProxyRequest { (data: js.Dynamic) =>
              AppState.proxyCaptures = data :: AppState.proxyCaptures
              if (AppState.proxyCaptures.length > 50) {
                AppState.proxyCaptures = AppState.proxyCaptures.take(50)
              }
              ProxyControl.triggerDebouncedRenderList()
            }
            api.onProxyResponse { (data: js.Dynamic) =>
              val id = data.id
              AppState.proxyCaptures = AppState.proxyCaptures.map { e =>
                if (e.id == id) {
                  e.response = data
                  e
                } else e
              }
              ProxyControl.triggerDebouncedRenderList()
              AppState.selectedProxyId.foreach { selId =>
                if (selId == id.asInstanceOf[Double]) {
                  ProxyControl.triggerDebouncedRenderDetail()
                }
              }
            }
            ProxyControl.renderProxyStatus()
          }
        }.asInstanceOf[js.Dynamic].`catch`({ (_: Any) => () }: js.Function1[Any, Unit]) }
      }
    }
  }
}
