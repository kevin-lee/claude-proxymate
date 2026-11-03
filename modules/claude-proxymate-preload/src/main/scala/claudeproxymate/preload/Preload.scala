package claudeproxymate.preload

import claudeproxymate.core.IpcChannels
import claudeproxymate.preload.facades.{ContextBridge, IpcRenderer}

import scala.scalajs.js

object Preload {

  def main(args: Array[String]): Unit = {
    val process = js.Dynamic.global.process

    val api = js
      .Dynamic
      .literal(
        platform = process.platform,
        proxyStart = { (port: js.Any) =>
          IpcRenderer.invoke(IpcChannels.ProxyStart, port)
        }: js.Function1[js.Any, js.Promise[js.Any]],
        proxyStop = { () =>
          IpcRenderer.invoke(IpcChannels.ProxyStop)
        }: js.Function0[js.Promise[js.Any]],
        proxyStatus = { () =>
          IpcRenderer.invoke(IpcChannels.ProxyStatus)
        }: js.Function0[js.Promise[js.Any]],
        onProxyRequest = { (cb: js.Function1[js.Dynamic, Unit]) =>
          IpcRenderer.on(
            IpcChannels.ProxyRequest,
            { (_: js.Dynamic, data: js.Dynamic) =>
              cb(data)
            }: js.Function2[js.Dynamic, js.Dynamic, Unit]
          )
        }: js.Function1[js.Function1[js.Dynamic, Unit], Unit],
        onProxyResponse = { (cb: js.Function1[js.Dynamic, Unit]) =>
          IpcRenderer.on(
            IpcChannels.ProxyResponse,
            { (_: js.Dynamic, data: js.Dynamic) =>
              cb(data)
            }: js.Function2[js.Dynamic, js.Dynamic, Unit]
          )
        }: js.Function1[js.Function1[js.Dynamic, Unit], Unit],
        offProxy = { () =>
          IpcRenderer.removeAllListeners(IpcChannels.ProxyRequest)
          IpcRenderer.removeAllListeners(IpcChannels.ProxyResponse)
        }: js.Function0[Unit],
        openExternal = { (url: String) =>
          IpcRenderer.invoke(IpcChannels.ShellOpenExternal, url)
        }: js.Function1[String, js.Promise[js.Any]],
        onProxyState = { (cb: js.Function1[js.Dynamic, Unit]) =>
          /* Self-cleaning: a renderer reload re-registers, so drop any
           * previous listener first. */
          IpcRenderer.removeAllListeners(IpcChannels.ProxyState)
          IpcRenderer.on(
            IpcChannels.ProxyState,
            { (_: js.Dynamic, data: js.Dynamic) =>
              cb(data)
            }: js.Function2[js.Dynamic, js.Dynamic, Unit]
          )
        }: js.Function1[js.Function1[js.Dynamic, Unit], Unit],
        routeModeSet = { (mode: js.Any) =>
          IpcRenderer.invoke(IpcChannels.RouteModeSet, mode)
        }: js.Function1[js.Any, js.Promise[js.Any]],
        onRouteModeEvent = { (cb: js.Function1[js.Dynamic, Unit]) =>
          /* Self-cleaning: a renderer reload re-registers, so drop any
           * previous listener first to avoid duplicate alert handlers. */
          IpcRenderer.removeAllListeners(IpcChannels.RouteModeEvent)
          IpcRenderer.on(
            IpcChannels.RouteModeEvent,
            { (_: js.Dynamic, data: js.Dynamic) =>
              cb(data)
            }: js.Function2[js.Dynamic, js.Dynamic, Unit]
          )
        }: js.Function1[js.Function1[js.Dynamic, Unit], Unit],
        filterConfigGet = { () =>
          IpcRenderer.invoke(IpcChannels.FilterConfigGet)
        }: js.Function0[js.Promise[js.Any]],
        filterConfigSet = { (config: js.Any) =>
          IpcRenderer.invoke(IpcChannels.FilterConfigSet, config)
        }: js.Function1[js.Any, js.Promise[js.Any]],
        claudeInventoryScan = { () =>
          IpcRenderer.invoke(IpcChannels.ClaudeInventoryScan)
        }: js.Function0[js.Promise[js.Any]],
        appVersion = { () =>
          IpcRenderer.invoke(IpcChannels.AppVersion)
        }: js.Function0[js.Promise[js.Any]],
        updateStateGet = { () =>
          IpcRenderer.invoke(IpcChannels.UpdateStateGet)
        }: js.Function0[js.Promise[js.Any]],
        updateCheck = { () =>
          IpcRenderer.invoke(IpcChannels.UpdateCheck)
        }: js.Function0[js.Promise[js.Any]],
        updateDownload = { () =>
          IpcRenderer.invoke(IpcChannels.UpdateDownload)
        }: js.Function0[js.Promise[js.Any]],
        updateInstall = { () =>
          IpcRenderer.invoke(IpcChannels.UpdateInstall)
        }: js.Function0[js.Promise[js.Any]],
        updateSettingsGet = { () =>
          IpcRenderer.invoke(IpcChannels.UpdateSettingsGet)
        }: js.Function0[js.Promise[js.Any]],
        updateSettingsSet = { (settings: js.Any) =>
          IpcRenderer.invoke(IpcChannels.UpdateSettingsSet, settings)
        }: js.Function1[js.Any, js.Promise[js.Any]],
        onUpdateState = { (cb: js.Function1[js.Dynamic, Unit]) =>
          /* Self-cleaning: a renderer reload re-registers, so drop any
           * previous listener first. */
          IpcRenderer.removeAllListeners(IpcChannels.UpdateStateEvent)
          IpcRenderer.on(
            IpcChannels.UpdateStateEvent,
            { (_: js.Dynamic, data: js.Dynamic) =>
              cb(data)
            }: js.Function2[js.Dynamic, js.Dynamic, Unit]
          )
        }: js.Function1[js.Function1[js.Dynamic, Unit], Unit],
        onUpdateReveal = { (cb: js.Function1[js.Dynamic, Unit]) =>
          /* Self-cleaning: a renderer reload re-registers, so drop any
           * previous listener first. */
          IpcRenderer.removeAllListeners(IpcChannels.UpdateReveal)
          IpcRenderer.on(
            IpcChannels.UpdateReveal,
            { (_: js.Dynamic, data: js.Dynamic) =>
              cb(data)
            }: js.Function2[js.Dynamic, js.Dynamic, Unit]
          )
        }: js.Function1[js.Function1[js.Dynamic, Unit], Unit],
      )

    ContextBridge.exposeInMainWorld(IpcChannels.BridgeName, api.asInstanceOf[js.Object])
  }
}
