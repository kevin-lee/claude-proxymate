package claudeproxymate.preload

import claudeproxymate.core.IpcChannels
import claudeproxymate.preload.facades.{ContextBridge, IpcRenderer}

import scala.scalajs.js

object Preload {

  def main(args: Array[String]): Unit = {
    val process = js.Dynamic.global.process

    val api = js.Dynamic.literal(
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
        IpcRenderer.on(IpcChannels.ProxyRequest, { (_: js.Dynamic, data: js.Dynamic) =>
          cb(data)
        }: js.Function2[js.Dynamic, js.Dynamic, Unit])
      }: js.Function1[js.Function1[js.Dynamic, Unit], Unit],
      onProxyResponse = { (cb: js.Function1[js.Dynamic, Unit]) =>
        IpcRenderer.on(IpcChannels.ProxyResponse, { (_: js.Dynamic, data: js.Dynamic) =>
          cb(data)
        }: js.Function2[js.Dynamic, js.Dynamic, Unit])
      }: js.Function1[js.Function1[js.Dynamic, Unit], Unit],
      offProxy = { () =>
        IpcRenderer.removeAllListeners(IpcChannels.ProxyRequest)
        IpcRenderer.removeAllListeners(IpcChannels.ProxyResponse)
      }: js.Function0[Unit],
    )

    ContextBridge.exposeInMainWorld(IpcChannels.BridgeName, api.asInstanceOf[js.Object])
  }
}
