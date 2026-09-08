package claudeproxymate.electron.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSImport("fs", JSImport.Namespace)
object NodeFs extends js.Object {
  def readFileSync(path: String, encoding: String): String            = js.native
  def writeFileSync(path: String, data: String): Unit                 = js.native
  def existsSync(path: String): Boolean                               = js.native
  def mkdirSync(path: String, options: js.Object): Unit               = js.native
  def readdirSync(path: String, options: js.Object): js.Array[Dirent] = js.native
}

/** A directory entry from `readdirSync(path, {withFileTypes: true})`. */
@js.native
trait Dirent extends js.Object {
  val name: String           = js.native
  def isDirectory(): Boolean = js.native
  def isFile(): Boolean      = js.native
}
