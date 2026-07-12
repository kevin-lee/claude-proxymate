package claudeproxymate.renderer.facades

import org.scalajs.dom
import scala.scalajs.js

/** Facade for window.electronAPI exposed by preload.js. */
@js.native
trait ElectronApi extends js.Object {
  val platform: String                                          = js.native
  def proxyStart(port: Int): js.Promise[js.Dynamic]             = js.native
  def proxyStop(): js.Promise[js.Dynamic]                       = js.native
  def proxyStatus(): js.Promise[js.Dynamic]                     = js.native
  def onProxyRequest(cb: js.Function1[js.Dynamic, Unit]): Unit  = js.native
  def onProxyResponse(cb: js.Function1[js.Dynamic, Unit]): Unit = js.native
  def offProxy(): Unit                                          = js.native
  def onProxyState(cb: js.Function1[js.Dynamic, Unit]): Unit    = js.native
  def openExternal(url: String): js.Promise[js.Dynamic]         = js.native
  def routeModeSet(mode: String): js.Promise[js.Dynamic]        = js.native
  def onRouteModeEvent(cb: js.Function1[js.Dynamic, Unit]): Unit = js.native
}

object ElectronApi {
  def get: Option[ElectronApi] = {
    val api = dom.window.asInstanceOf[js.Dynamic].selectDynamic("electronAPI")
    Option.unless(js.isUndefined(api) || api == null)(api.asInstanceOf[ElectronApi])
  }
}
