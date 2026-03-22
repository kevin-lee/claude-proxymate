package claudeproxymate.electron.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSImport("fs", JSImport.Namespace)
object NodeFs extends js.Object {
  def readFileSync(path: String, encoding: String): String = js.native
  def writeFileSync(path: String, data: String): Unit      = js.native
  def existsSync(path: String): Boolean                    = js.native
}
