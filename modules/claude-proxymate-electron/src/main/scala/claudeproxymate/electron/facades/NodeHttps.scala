package claudeproxymate.electron.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSImport("https", JSImport.Namespace)
object NodeHttps extends js.Object {
  def request(options: js.Object, callback: js.Function1[js.Dynamic, Unit]): NodeClientRequest = js.native
  def request(options: js.Object): NodeClientRequest                                           = js.native
}

@js.native
trait NodeClientRequest extends js.Object {
  def write(data: String): Boolean             = js.native
  def end(): Unit                              = js.native
  def on(event: String, cb: js.Function): Unit = js.native
}
