package claudeproxymate.electron

import cats.syntax.all.*
import claudeproxymate.core.IpcChannels
import claudeproxymate.core.update.{UpdatePhase, UpdateState}
import claudeproxymate.electron.facades.*
import io.circe.syntax.*

import java.util.concurrent.atomic.AtomicReference
import scala.scalajs.js
import scala.scalajs.js.JSON

/** Self-update with electron-updater (GitHub provider, Squirrel.Mac underneath).
  *
  * Owns the [[UpdateState]] and pushes every change to the renderer. Squirrel.Mac
  * accepts a downloaded app only when it satisfies the running app's designated
  * requirement (same bundle id and Developer ID team), so a wrongly signed update
  * is rejected without an extra key.
  *
  * Downloads never start by themselves except after the start check with the
  * `autoUpdate` setting on. The app never restarts by itself: a downloaded update
  * installs on "Restart now" or at the next normal quit (`autoInstallOnAppQuit`).
  */
object UpdateService {

  /** Points the updater at a generic feed, for testing with a local feed. */
  val FeedUrlEnv: String = "CLAUDE_PROXYMATE_UPDATE_FEED_URL"

  private val state = new AtomicReference[UpdateState](UpdateState.idle)

  /* Set when the app menu asked for the update info but no window existed. The
   * renderer of the new window reads and clears it with `update-state-get`. */
  private val pendingReveal = new AtomicReference[Boolean](false)

  /* Whether the check in progress was started by the user ("Check now" or the app
   * menu). The start check is the only check that is not. */
  private val checkUserInitiated = new AtomicReference[Boolean](false)

  /* The version of the update found by the last check, for the download states. */
  private val foundVersion = new AtomicReference[Option[String]](none[String])

  private val updater = new AtomicReference[Option[AppUpdater]](none[AppUpdater])

  private val getMainWindowRef =
    new AtomicReference[() => Option[BrowserWindow]](() => none[BrowserWindow])

  private val confirmQuitRef = new AtomicReference[() => Unit](() => ())

  private val createWindowRef = new AtomicReference[() => Unit](() => ())

  private def log(message: String): Unit = {
    val _ = js.Dynamic.global.console.log(s"update: $message")
  }

  private def warn(message: String): Unit = {
    val _ = js.Dynamic.global.console.warn(s"update: $message")
  }

  private def now(): Double = js.Date.now()

  private def stateJs(s: UpdateState): js.Dynamic = JSON.parse(s.asJson.noSpaces)

  private def setState(next: UpdateState): Unit = {
    state.set(next)
    getMainWindowRef.get()().foreach { win =>
      if (!win.isDestroyed()) {
        win.webContents.send(IpcChannels.UpdateStateEvent, stateJs(next))
      } else ()
    }
  }

  def register(
    getMainWindow: () => Option[BrowserWindow],
    confirmQuit: () => Unit,
    createWindow: () => Unit,
  ): Unit = {
    getMainWindowRef.set(getMainWindow)
    confirmQuitRef.set(confirmQuit)
    createWindowRef.set(createWindow)

    IpcMain.handle(
      IpcChannels.AppVersion,
      { (_: js.Dynamic, _: js.Dynamic) =>
        ElectronApp.getVersion()
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.UpdateStateGet,
      { (_: js.Dynamic, _: js.Dynamic) =>
        js.Dynamic.literal(state = stateJs(state.get()), reveal = pendingReveal.getAndSet(false))
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.UpdateCheck,
      { (_: js.Dynamic, _: js.Dynamic) =>
        check(true)
        stateJs(state.get())
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.UpdateDownload,
      { (_: js.Dynamic, _: js.Dynamic) =>
        download()
        stateJs(state.get())
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.UpdateInstall,
      { (_: js.Dynamic, _: js.Dynamic) =>
        js.Dynamic.literal(ok = install())
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.UpdateSettingsGet,
      { (_: js.Dynamic, _: js.Dynamic) =>
        UpdateSettingsStore.toJs(UpdateSettingsStore.read())
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.UpdateSettingsSet,
      { (_: js.Dynamic, settingsArg: js.Dynamic) =>
        UpdateSettingsStore.save(settingsArg)
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )
  }

  /** Called on `ready` after the window is created. Sets up the updater and runs the start check. */
  def start(): Unit =
    ElectronUpdater.get match {
      case None =>
        warn(ElectronUpdater.MissingModuleMessage)
        setState(UpdateState.unsupported)
      case Some(module) =>
        val feedUrl = Config.env(FeedUrlEnv).filter(_.trim.nonEmpty)
        /* Checked before `autoUpdater` is read: reading it creates the updater, which
         * throws for a development version that is not valid semver. */
        if (!ElectronApp.isPackaged && feedUrl.isEmpty) {
          log("not packaged and no feed override, self-update is off")
          setState(UpdateState.unsupported)
        } else {
          /* A packaged build can still have a version that is not valid semver, e.g. one
           * built from a working tree with uncommitted changes (sbt-dynver gives
           * `X.Y.Z+N-sha8+yyyyMMdd-HHmm`). Creating the updater throws then. Self-update is
           * off for such a build, and the app must still start. */
          try {
            val u = module.autoUpdater
            feedUrl.foreach { url =>
              log(s"$FeedUrlEnv is set, using the generic feed $url")
              u.forceDevUpdateConfig = true
              u.setFeedURL(js.Dynamic.literal(provider = "generic", url = url).asInstanceOf[js.Object])
            }
            u.autoDownload = false
            u.autoInstallOnAppQuit = true
            u.autoRunAppAfterInstall = true
            u.logger = js.Dynamic.global.console
            subscribe(u)
            updater.set(u.some)
            check(false)
          } catch {
            case e: Throwable =>
              warn(s"cannot start the updater, self-update is off: ${e.getMessage}")
              setState(UpdateState.unsupported)
          }
        }
    }

  private def subscribe(u: AppUpdater): Unit = {
    val _ = u.on(
      "checking-for-update",
      { () =>
        setState(UpdateState.checking(checkUserInitiated.get()))
      }: js.Function0[Unit]
    )
    val _ = u.on(
      "update-not-available",
      { (_: js.Dynamic) =>
        setState(UpdateState.upToDate(now()))
      }: js.Function1[js.Dynamic, Unit]
    )
    val _ = u.on(
      "update-available",
      { (info: js.Dynamic) =>
        val version = info.version.asInstanceOf[String]
        foundVersion.set(version.some)
        setState(UpdateState.available(version, now()))
        if (!checkUserInitiated.get() && UpdateSettingsStore.read().autoUpdate) download() else ()
      }: js.Function1[js.Dynamic, Unit]
    )
    val _ = u.on(
      "download-progress",
      { (progress: js.Dynamic) =>
        val percent = UpdateState.clampPercent(progress.percent.asInstanceOf[Double])
        setState(UpdateState.downloading(foundVersion.get().getOrElse(""), percent))
      }: js.Function1[js.Dynamic, Unit]
    )
    val _ = u.on(
      "update-downloaded",
      { (info: js.Dynamic) =>
        val version = info.version.asInstanceOf[String]
        foundVersion.set(version.some)
        setState(UpdateState.ready(version))
      }: js.Function1[js.Dynamic, Unit]
    )
    val _ = u.on(
      "error",
      { (err: js.Dynamic) =>
        fail(errorMessage(err))
      }: js.Function1[js.Dynamic, Unit]
    )
  }

  private def errorMessage(err: js.Any): String =
    if (js.isUndefined(err) || err == null) "Unknown error"
    else {
      val message = err.asInstanceOf[js.Dynamic].message
      if (js.isUndefined(message) || message == null) err.toString else message.toString
    }

  /** Moves to `Failed` for the phase in progress. Ignored when already failed, so the
    * `error` event and a rejected promise for the same failure give one state.
    */
  private def fail(message: String): Unit = {
    warn(message)
    state.get() match {
      case UpdateState.Failed(_, _, _, _) =>
        ()
      case UpdateState.Checking(userInitiated) =>
        setState(UpdateState.failed(UpdatePhase.Check, UpdateState.displayMessage(message), userInitiated, now().some))
      case UpdateState.Downloading(_, _) | UpdateState.Available(_, _) =>
        setState(UpdateState.failed(UpdatePhase.Download, UpdateState.displayMessage(message), true, none[Double]))
      case UpdateState.Ready(_) =>
        setState(UpdateState.failed(UpdatePhase.Install, UpdateState.displayMessage(message), true, none[Double]))
      case UpdateState.Unsupported | UpdateState.Idle | UpdateState.UpToDate(_) =>
        setState(
          UpdateState.failed(
            UpdatePhase.Check,
            UpdateState.displayMessage(message),
            checkUserInitiated.get(),
            now().some
          )
        )
    }
  }

  private def onRejected(promise: js.Promise[js.Any]): Unit = {
    val _ = promise
      .asInstanceOf[js.Dynamic]
      .`catch`({ (e: js.Any) =>
        fail(errorMessage(e))
      }: js.Function1[js.Any, Unit])
  }

  def check(userInitiated: Boolean): Unit =
    updater.get() match {
      case Some(u) if UpdateState.canCheck(state.get()) =>
        checkUserInitiated.set(userInitiated)
        setState(UpdateState.checking(userInitiated))
        try {
          onRejected(u.checkForUpdates())
        } catch {
          case e: Throwable => fail(e.getMessage)
        }
      case Some(_) | None =>
        ()
    }

  def download(): Unit =
    updater.get() match {
      case Some(u) if UpdateState.canDownload(state.get()) =>
        setState(UpdateState.downloading(foundVersion.get().getOrElse(""), 0))
        try {
          onRejected(u.downloadUpdate())
        } catch {
          case e: Throwable => fail(e.getMessage)
        }
      case Some(_) | None =>
        ()
    }

  /** Stops the proxy the same way as a quit, then lets Squirrel.Mac install and relaunch.
    *
    * `confirmQuit` is set before `quitAndInstall`, so the `before-quit` handler does not
    * defer this quit a second time. Returns false when no update is ready.
    */
  def install(): Boolean =
    updater.get() match {
      case Some(u) if UpdateState.canInstall(state.get()) =>
        RouteSync.onQuit()
        val _ = IpcHandlers.stopProxy { () =>
          try {
            confirmQuitRef.get()()
            u.quitAndInstall()
          } catch {
            case e: Throwable =>
              warn(e.getMessage)
              setState(
                UpdateState.failed(UpdatePhase.Install, UpdateState.displayMessage(e.getMessage), true, none[Double])
              )
          }
        }
        true
      case Some(_) | None =>
        false
    }

  /** The app menu's "Check for Updates…": show the update info and check. */
  def revealFromMenu(): Unit = {
    getMainWindowRef.get()().filterNot(_.isDestroyed()) match {
      case Some(win) =>
        win.show()
        win.webContents.send(IpcChannels.UpdateReveal)
      case None =>
        pendingReveal.set(true)
        createWindowRef.get()()
    }
    check(true)
  }
}
