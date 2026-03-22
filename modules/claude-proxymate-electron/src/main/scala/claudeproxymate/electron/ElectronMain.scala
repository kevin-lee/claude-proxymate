package claudeproxymate.electron

import claudeproxymate.electron.facades._

import java.util.concurrent.atomic.AtomicReference
import scala.scalajs.js

/** Electron main process entry point. */
object ElectronMain {

  private val mainWindow = new AtomicReference[Option[BrowserWindow]](None)

  private val process  = js.Dynamic.global.process
  private val platform = process.platform.asInstanceOf[String]

  def main(args: Array[String]): Unit = {

    ElectronApp.on(
      "ready",
      { () =>
        Analytics.init(ElectronApp.getPath("userData"))
        Analytics.trackEvent("app_open")

        if (platform == "darwin") {
          try {
            val _ = ElectronApp
              .asInstanceOf[js.Dynamic]
              .dock
              .setIcon(
                NodePath.join(ElectronApp.getAppPath(), "assets", "icon.png"),
              )
          } catch { case _: Throwable => () }
        } else ()

        createWindow()
      }
    )

    ElectronApp.on(
      "activate",
      { () =>
        mainWindow.get() match {
          case Some(win) if !win.isDestroyed() => win.show()
          case _ => createWindow()
        }
      }
    )

    ElectronApp.on(
      "window-all-closed",
      { () =>
        if (platform != "darwin") ElectronApp.quit() else ()
      }
    )

    ElectronApp.on(
      "before-quit",
      { () =>
        IpcHandlers.stopProxyIfRunning()
      }
    )

    IpcHandlers.register(() => mainWindow.get())
  }

  private def createWindow(): Unit = {
    val isDarwin = platform == "darwin"
    val appPath  = ElectronApp.getAppPath()

    val windowOptions = js
      .Dynamic
      .literal(
        width = 1320,
        height = 860,
        minWidth = 980,
        minHeight = 620,
        icon = NodePath.join(appPath, "assets", "icon.png"),
        webPreferences = js
          .Dynamic
          .literal(
            preload = NodePath.join(appPath, "preload.js"),
            contextIsolation = true,
            nodeIntegration = false,
          ),
        titleBarStyle = (if (isDarwin) "hiddenInset" else "default"),
        title = "Claude Proxymate",
        backgroundColor = "#1e1e1e",
        show = false,
      )

    if (isDarwin) {
      windowOptions.trafficLightPosition = js.Dynamic.literal(x = 12, y = 19)
    } else ()

    val win = new BrowserWindow(windowOptions.asInstanceOf[js.Object])

    win.loadFile(NodePath.join(appPath, "public", "index.html")): Unit

    // Retry on load failure (macOS quarantine scan can lock the asar on first launch)
    win
      .webContents
      .on(
        "did-fail-load",
        { () =>
          val _ = js
            .Dynamic
            .global
            .setTimeout(
              { () =>
                if (!win.isDestroyed()) {
                  win.loadFile(NodePath.join(appPath, "public", "index.html")): Unit
                } else ()
              }: js.Function0[Unit],
              1500,
            )
        }: js.Function0[Unit]
      )

    // Show window when ready
    win.on(
      "ready-to-show",
      { () =>
        win.show()
      }
    )

    // Fallback: force show if ready-to-show never fires
    val _ = js
      .Dynamic
      .global
      .setTimeout(
        { () =>
          if (!win.isDestroyed()) {
            try {
              val isVisible = win.asInstanceOf[js.Dynamic].isVisible().asInstanceOf[Boolean]
              if (!isVisible) win.show() else ()
            } catch { case _: Throwable => () }
          } else ()
        }: js.Function0[Unit],
        3000,
      )

    // Open external links in default browser, not Electron
    win
      .webContents
      .setWindowOpenHandler({ (details: js.Dynamic) =>
        Shell.openExternal(details.url.asInstanceOf[String]): Unit
        js.Dynamic.literal(action = "deny").asInstanceOf[js.Object]
      }: js.Function1[js.Dynamic, js.Object])

    mainWindow.set(Some(win))
  }
}
