package claudeproxymate.electron

import cats.syntax.all.*
import claudeproxymate.core.{IpcChannels, RouteMode, VsCodeEnv}
import claudeproxymate.electron.SyncFileOps.TargetResult
import claudeproxymate.electron.facades._

import java.util.concurrent.atomic.AtomicReference
import scala.scalajs.js
import scala.scalajs.js.JSON

/** The Route Claude coordinator: owns the [[RouteMode]] and drives the
  * two settings backends ([[VsCodeSync]], [[ClaudeSettingsSync]]) so
  * that at most one target carries `ANTHROPIC_BASE_URL` at any time.
  *
  * Owns the mode in the main process because only the main process sees
  * every proxy stop path (explicit stop, child exit/error, app quit).
  * The selection is persisted to `route-mode.json` in `userData` and
  * restored at launch — but the env var itself is only ever written
  * while the proxy actually runs (selecting a route while stopped just
  * arms it), and app quit removes it from every target regardless of
  * mode.
  */
object RouteSync {

  final private case class State(mode: RouteMode, runningPort: Option[Int])

  private object State {
    val initial: State = State(RouteMode.default, none[Int])
  }

  private val state = new AtomicReference[State](State.initial)

  def mode: RouteMode = state.get().mode

  private def modeFilePath: String =
    NodePath.join(ElectronApp.getPath("userData"), "route-mode.json")

  // ── Public API ─────────────────────────────────────────────────────

  /** Switch the route. Removals from the now-inactive targets happen
    * immediately; the newly selected target is written only when the
    * proxy is running (otherwise the mode is just armed). Selecting
    * VS Code with zero detected editors keeps the current mode.
    */
  def setMode(requested: RouteMode, getMainWindow: () => Option[BrowserWindow]): js.Dynamic = {
    val _       = getMainWindow // results are returned directly; no event push needed here
    val current = state.get()
    if (requested === current.mode) {
      resultJs(current.mode, VsCodeSync.detectedCount, Nil)
    } else if (requested === RouteMode.VsCode && VsCodeSync.detectedCount === 0) {
      resultJs(current.mode, 0, Nil)
    } else {
      val prev     = state.getAndUpdate(s => s.copy(mode = requested))
      persistMode(requested)
      val fallback = prev.runningPort.map(VsCodeEnv.baseUrl)
      val removals =
        (if (requested =!= RouteMode.VsCode) VsCodeSync.removeAll(fallback) else Nil) ++
          (if (requested =!= RouteMode.Global) List(ClaudeSettingsSync.removeEnv(fallback)) else Nil)
      val applies  = prev.runningPort match {
        case Some(port) =>
          requested match {
            case RouteMode.VsCode => VsCodeSync.applyAll(port)
            case RouteMode.Global => List(ClaudeSettingsSync.applyEnv(port))
            case RouteMode.Manual => Nil
          }
        case None => Nil
      }
      Analytics.trackEvent(s"route_mode_${requested.wire}")
      resultJs(requested, VsCodeSync.detectedCount, removals ++ applies)
    }
  }

  /** Proxy is up (decoded `ProxyEvent.ProxyStarted`) — apply the active
    * target, if any.
    */
  def onProxyStarted(port: Int, getMainWindow: () => Option[BrowserWindow]): Unit = {
    val st = state.updateAndGet(s => s.copy(runningPort = port.some))
    st.mode match {
      case RouteMode.VsCode => pushEvent("proxy-start", VsCodeSync.applyAll(port), getMainWindow)
      case RouteMode.Global => pushEvent("proxy-start", List(ClaudeSettingsSync.applyEnv(port)), getMainWindow)
      case RouteMode.Manual => ()
    }
  }

  /** Proxy is down (stop / crash / explicit kill) — remove from the
    * active target. Strictly idempotent: the second of a double-fire
    * (SIGTERM then child `exit`) finds nothing to remove and writes
    * nothing.
    */
  def onProxyStopped(getMainWindow: () => Option[BrowserWindow]): Unit = {
    val prev     = state.getAndUpdate(s => s.copy(runningPort = none[Int]))
    val fallback = prev.runningPort.map(VsCodeEnv.baseUrl)
    prev.mode match {
      case RouteMode.VsCode => pushEvent("proxy-stop", VsCodeSync.removeAll(fallback), getMainWindow)
      case RouteMode.Global => pushEvent("proxy-stop", List(ClaudeSettingsSync.removeEnv(fallback)), getMainWindow)
      case RouteMode.Manual => ()
    }
  }

  /** App quit — reset the in-memory state first so teardown child-exit
    * events no-op, then remove synchronously from EVERY target (record
    * driven; no-ops when nothing was written), regardless of mode. The
    * persisted mode file is deliberately kept: the selection survives
    * restarts. No event push: the window may already be gone.
    */
  def onQuit(): Unit = {
    val prev     = state.getAndSet(State.initial)
    val fallback = prev.runningPort.map(VsCodeEnv.baseUrl)
    val _        = VsCodeSync.removeAll(fallback)
    val _        = ClaudeSettingsSync.removeEnv(fallback)
  }

  /** App launch — clean up what a crashed previous run left behind.
    * Record-driven only: with no record file nothing is touched. A dirty
    * record with an unparseable settings file (crash mid-write) restores
    * the backup first.
    */
  def sweepOnLaunch(): Unit =
    SyncFileOps.readRecord().foreach { case (targetId, entry) =>
      if (targetId === ClaudeSettingsSync.TargetId) {
        SyncFileOps.restoreIfDirtyAndBroken(ClaudeSettingsSync.target, entry, ClaudeSettingsSync.stillParseable)
        val _ = ClaudeSettingsSync.removeEnv(none[String])
      } else {
        VsCodeSync.targetForId(targetId).foreach { target =>
          SyncFileOps.restoreIfDirtyAndBroken(target, entry, VsCodeSync.stillParseable)
        }
        val _ = VsCodeSync.removeById(targetId, none[String])
      }
    }

  /** App launch (after [[sweepOnLaunch]]) — restore the persisted route
    * selection. No file is written until the proxy actually starts.
    */
  def loadPersistedMode(): Unit = {
    val persisted =
      try {
        if (!NodeFs.existsSync(modeFilePath)) {
          none[RouteMode]
        } else {
          val parsed = JSON.parse(NodeFs.readFileSync(modeFilePath, "utf8"))
          val value  = parsed.selectDynamic("mode")
          if (js.typeOf(value) === "string") RouteMode.parse(value.asInstanceOf[String])
          else none[RouteMode]
        }
      } catch {
        case _: Throwable => none[RouteMode]
      }
    persisted.foreach { m =>
      state.updateAndGet(s => s.copy(mode = m)): Unit
    }
  }

  private def persistMode(mode: RouteMode): Unit =
    try {
      NodeFs.writeFileSync(modeFilePath, JSON.stringify(js.Dynamic.literal(mode = mode.wire)))
    } catch {
      case e: Throwable =>
        val _ = js.Dynamic.global.console.warn("route-sync: cannot persist route mode:", e.getMessage)
    }

  // ── IPC payloads ───────────────────────────────────────────────────

  def resultJs(mode: RouteMode, detected: Int, results: List[TargetResult]): js.Dynamic =
    js.Dynamic
      .literal(
        mode = mode.wire,
        detected = detected,
        results = js.Array(results.map(SyncFileOps.resultEntryJs)*),
      )

  /** The current state as a set-result payload (used for a no-op reply
    * to an invalid IPC argument).
    */
  def statusResultJs: js.Dynamic =
    resultJs(mode, VsCodeSync.detectedCount, Nil)

  /** Notify the renderer about auto-transition problems (only failures
    * are pushed; clean applies/removals stay silent).
    */
  private def pushEvent(
    trigger: String,
    results: List[TargetResult],
    getMainWindow: () => Option[BrowserWindow],
  ): Unit = {
    val failures = results.filter(result => result.action.isFailure)
    if (failures.nonEmpty) {
      getMainWindow().foreach { win =>
        if (!win.isDestroyed()) {
          val payload =
            js.Dynamic.literal(trigger = trigger, results = js.Array(failures.map(SyncFileOps.resultEntryJs)*))
          win.webContents.send(IpcChannels.RouteModeEvent, payload)
        } else ()
      }
    } else ()
  }
}
