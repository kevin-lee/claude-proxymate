package claudeproxymate.electron.facades

import cats.syntax.all.*

import scala.scalajs.js

/** The subset of electron-updater's `AppUpdater` used by UpdateService. */
@js.native
trait AppUpdater extends js.Object {
  var autoDownload: Boolean                          = js.native
  var autoInstallOnAppQuit: Boolean                  = js.native
  var autoRunAppAfterInstall: Boolean                = js.native
  var forceDevUpdateConfig: Boolean                  = js.native
  var logger: js.Any                                 = js.native
  def checkForUpdates(): js.Promise[js.Any]          = js.native
  def downloadUpdate(): js.Promise[js.Any]           = js.native
  def quitAndInstall(): Unit                         = js.native
  def setFeedURL(options: js.Object): Unit           = js.native
  def on(event: String, cb: js.Function): AppUpdater = js.native
}

/** The subset of the `electron-updater` npm module used by UpdateService. */
@js.native
trait ElectronUpdaterModule extends js.Object {
  val autoUpdater: AppUpdater = js.native
}

/** `electron-updater` loaded via a dynamic `require` in try/catch.
  *
  * Same reason as [[JsoncParser]]: a static `@JSImport` would emit a
  * top-level `require("electron-updater")` and kill the whole main process
  * at startup when `electron-app/node_modules` is stale. With the dynamic
  * load, only the self-update feature degrades to "unsupported".
  */
object ElectronUpdater {

  val MissingModuleMessage: String = "electron-updater module missing - run `npm install` in electron-app"

  lazy val get: Option[ElectronUpdaterModule] =
    attempt(() => js.Dynamic.global.selectDynamic("require"))
      .orElse(attempt(() => js.Dynamic.global.process.selectDynamic("mainModule").selectDynamic("require")))

  private def attempt(requireFn: () => js.Dynamic): Option[ElectronUpdaterModule] =
    try {
      val fn = requireFn()
      if (js.isUndefined(fn) || fn == null) {
        none[ElectronUpdaterModule]
      } else {
        Option(fn("electron-updater")).map(m => m.asInstanceOf[ElectronUpdaterModule])
      }
    } catch {
      case _: Throwable => none[ElectronUpdaterModule]
    }
}
