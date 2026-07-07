package claudeproxymate.preload.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSImport("electron", "contextBridge")
object ContextBridge extends js.Object {
  def exposeInMainWorld(apiKey: String, api: js.Object): Unit = js.native
}

@js.native
@JSImport("electron", "ipcRenderer")
object IpcRenderer extends js.Object {
  def invoke(channel: String, args: js.Any*): js.Promise[js.Any]                      = js.native
  def on(channel: String, listener: js.Function2[js.Dynamic, js.Dynamic, Unit]): Unit = js.native
  def removeAllListeners(channel: String): Unit                                       = js.native
}
