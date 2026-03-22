package claudeproxymate.electron.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSImport("crypto", JSImport.Namespace)
object NodeCrypto extends js.Object {
  def randomUUID(): String = js.native
}
