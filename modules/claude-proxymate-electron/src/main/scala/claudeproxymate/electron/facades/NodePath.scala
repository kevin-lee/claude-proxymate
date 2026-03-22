package claudeproxymate.electron.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSImport("path", JSImport.Namespace)
object NodePath extends js.Object {
  def join(paths: String*): String = js.native
}
