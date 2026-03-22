package claudeproxymate.electron.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSImport("electron", "app")
object ElectronApp extends js.Object {
  def whenReady(): js.Promise[Unit]                   = js.native
  def on(event: String, cb: js.Function0[Unit]): Unit = js.native
  def quit(): Unit                                    = js.native
  def getPath(name: String): String                   = js.native
  def getAppPath(): String                            = js.native
}

@js.native
@JSImport("electron", "BrowserWindow")
class BrowserWindow(@annotation.unused options: js.Object) extends js.Object {
  val webContents: WebContents                                                = js.native
  def loadFile(path: String): js.Promise[Unit]                                = js.native
  def show(): Unit                                                            = js.native
  def isDestroyed(): Boolean                                                  = js.native
  def on(event: String, cb: js.Function0[Unit]): Unit                         = js.native
  def on(event: String, cb: js.Function2[js.Dynamic, js.Dynamic, Unit]): Unit = js.native
}

@js.native
trait WebContents extends js.Object {
  def send(channel: String, args: js.Any*): Unit                               = js.native
  def setWindowOpenHandler(handler: js.Function1[js.Dynamic, js.Object]): Unit = js.native
  def on(event: String, cb: js.Function): Unit                                 = js.native
}

@js.native
@JSImport("electron", "ipcMain")
object IpcMain extends js.Object {
  def handle(channel: String, handler: js.Function2[js.Dynamic, js.Dynamic, js.Any]): Unit = js.native
}

@js.native
@JSImport("electron", "shell")
object Shell extends js.Object {
  def openExternal(url: String): js.Promise[Unit] = js.native
}
