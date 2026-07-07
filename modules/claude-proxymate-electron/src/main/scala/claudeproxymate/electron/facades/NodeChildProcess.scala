package claudeproxymate.electron.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSImport("child_process", JSImport.Namespace)
object ChildProcessModule extends js.Object {
  def spawn(command: String, args: js.Array[String], options: js.Object): ChildProcess = js.native
}

@js.native
trait ChildProcess extends js.Object {
  val stdout: NodeStream                       = js.native
  val stderr: NodeStream                       = js.native
  val pid: js.UndefOr[Int]                     = js.native
  val killed: Boolean                          = js.native
  def kill(signal: String): Boolean            = js.native
  def on(event: String, cb: js.Function): Unit = js.native
}

@js.native
trait NodeStream extends js.Object {
  def on(event: String, cb: js.Function): Unit = js.native
  def setEncoding(encoding: String): Unit      = js.native
}
