package claudeproxymate.electron

import cats.syntax.all.*
import claudeproxymate.core.{IpcChannels, JsonLineProtocol, ProxyEvent, ProxyLifecycle, RouteMode, UrlScheme}
import claudeproxymate.electron.facades._

import java.util.concurrent.atomic.AtomicReference
import scala.scalajs.js
import scala.scalajs.js.JSON

/** IPC handlers for proxy lifecycle: spawn/kill native binary, parse stdout, forward events. */
object IpcHandlers {

  final private case class ProxyState(
    process: Option[ChildProcess],
    port: Option[Int],
    buffer: String,
    lifecycle: ProxyLifecycle,
  )

  private object ProxyState {
    val empty: ProxyState = ProxyState(none[ChildProcess], none[Int], "", ProxyLifecycle.Idle)
  }

  /** Handles for the two timers armed by [[stopProxy]], so the child's `exit`
    * event can cancel them.
    */
  final private case class PendingTimers(
    kill: Option[js.Dynamic],
    deadline: Option[js.Dynamic],
  )

  private object PendingTimers {
    val none: PendingTimers = PendingTimers(scala.None, scala.None)
  }

  private val state = new AtomicReference[ProxyState](ProxyState.empty)

  /* Callbacks waiting for the child to actually be gone. `before-quit` parks
   * the app quit in here. */
  private val whenGoneCallbacks = new AtomicReference[List[() => Unit]](List.empty[() => Unit])

  private val pendingTimers = new AtomicReference[PendingTimers](PendingTimers.none)

  /** How long the child gets to honour SIGTERM before SIGKILL follows. */
  private val KillGraceMs: Int = 3000

  /** Hard cap: the waiters run at this point whatever the child is doing, so
    * a deferred app quit can never hang.
    */
  private val QuitDeadlineMs: Int = 4000

  def register(getMainWindow: () => Option[BrowserWindow]): Unit = {
    IpcMain.handle(
      IpcChannels.ProxyStart,
      { (_: js.Dynamic, portArg: js.Dynamic) =>
        startProxy(portArg, getMainWindow)
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.ProxyStop,
      { (_: js.Dynamic, _: js.Dynamic) =>
        stopProxy(getMainWindow)
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.RouteModeSet,
      { (_: js.Dynamic, modeArg: js.Dynamic) =>
        val requested =
          if (js.isUndefined(modeArg) || modeArg == null || js.typeOf(modeArg) =!= "string") none[RouteMode]
          else RouteMode.parse(modeArg.asInstanceOf[String])
        requested match {
          case Some(mode) => RouteSync.setMode(mode, getMainWindow)
          case None => RouteSync.statusResultJs
        }
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.ProxyStatus,
      { (_: js.Dynamic, _: js.Dynamic) =>
        getStatus
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.ShellOpenExternal,
      { (_: js.Dynamic, urlArg: js.Dynamic) =>
        openExternal(urlArg)
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.FilterConfigGet,
      { (_: js.Dynamic, _: js.Dynamic) =>
        RequestFilterStore.load()
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.FilterConfigSet,
      { (_: js.Dynamic, configArg: js.Dynamic) =>
        RequestFilterStore.save(configArg)
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.ClaudeInventoryScan,
      { (_: js.Dynamic, _: js.Dynamic) =>
        ClaudeInventoryScan.scan()
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )
  }

  /** Validate a URL against the allowlist and forward to `shell.openExternal`.
    * Returns a Promise resolving to `{ok: true}` on success or
    * `{ok: false, reason: "..."}` on any rejection.
    */
  private def openExternal(urlArg: js.Dynamic): js.Promise[js.Dynamic] = {
    val url =
      if (js.isUndefined(urlArg) || urlArg == null) ""
      else urlArg.asInstanceOf[String]

    UrlScheme.validate(url) match {
      case Left(err) =>
        js.Promise
          .resolve[js.Dynamic](
            js.Dynamic.literal(ok = false, reason = err.message)
          )
      case Right(validUrl) =>
        Shell
          .openExternal(validUrl)
          .`then`[js.Dynamic]({ (_: Unit) =>
            js.Dynamic.literal(ok = true): js.Dynamic
          }: js.Function1[Unit, js.Dynamic])
          .asInstanceOf[js.Dynamic]
          .`catch`({ (e: js.Any) =>
            js.Dynamic.literal(ok = false, reason = s"openExternal failed: ${e.toString}"): js.Dynamic
          }: js.Function1[js.Any, js.Dynamic])
          .asInstanceOf[js.Promise[js.Dynamic]]
    }
  }

  private def clearPendingTimers(): Unit = {
    val timers = pendingTimers.getAndSet(PendingTimers.none)
    timers.kill.foreach { handle =>
      val _ = js.Dynamic.global.clearTimeout(handle)
    }
    timers.deadline.foreach { handle =>
      val _ = js.Dynamic.global.clearTimeout(handle)
    }
  }

  private def drainWhenGone(): Unit = {
    /* Taken atomically and reversed: callbacks are prepended, and a waiter
     * must never run twice (the app quit is one of them). */
    val callbacks = whenGoneCallbacks.getAndSet(List.empty[() => Unit])
    callbacks.reverse.foreach(callback => callback())
  }

  private def signal(child: ChildProcess, name: String): Unit =
    try { child.kill(name): Unit }
    catch { case _: Throwable => () }

  /** Arm the escalation: SIGKILL when SIGTERM has not been honoured in time,
    * then an unconditional drain so nothing waits on this child forever.
    */
  private def armEscalation(): Unit = {
    val killTimer     = js
      .Dynamic
      .global
      .setTimeout(
        { () =>
          state.get().process.foreach(child => signal(child, "SIGKILL"))
        }: js.Function0[Unit],
        KillGraceMs,
      )
    val deadlineTimer = js
      .Dynamic
      .global
      .setTimeout(
        { () =>
          clearPendingTimers()
          drainWhenGone()
        }: js.Function0[Unit],
        QuitDeadlineMs,
      )
    pendingTimers.set(PendingTimers(killTimer.some, deadlineTimer.some))
  }

  /** Stop the proxy child process, running `whenGone` once it is actually
    * gone. Called by the `proxy-stop` IPC handler and by `before-quit`.
    *
    * The child's handle deliberately stays in [[state]] until its `exit`
    * event arrives: Node's `child.killed` only reports that a signal was
    * *sent*, so dropping the handle on the send is what previously left a
    * surviving child unreachable and still holding the port.
    */
  def stopProxy(whenGone: () => Unit): StopOutcome = {
    whenGoneCallbacks.updateAndGet(callbacks => whenGone :: callbacks): Unit
    val current = state.get()
    ProxyLifecycle.onStopRequested(current.lifecycle) match {
      case Some(next) =>
        current.process.foreach(child => signal(child, "SIGTERM"))
        state.set(current.copy(lifecycle = next))
        armEscalation()
        StopOutcome.Deferred

      case None =>
        current.lifecycle match {
          case ProxyLifecycle.Stopping =>
            /* Already winding down; the timers are armed and this waiter
             * joins the queue. */
            StopOutcome.Deferred
          case ProxyLifecycle.Idle | ProxyLifecycle.Running =>
            drainWhenGone()
            StopOutcome.NothingToStop
        }
    }
  }

  private def startProxy(
    portArg: js.Dynamic,
    getMainWindow: () => Option[BrowserWindow],
  ): js.Dynamic = {
    val port =
      if (js.isUndefined(portArg) || portArg == null) 8888
      else {
        try { portArg.asInstanceOf[Int] }
        catch { case _: Throwable => 8888 }
      }

    if (port < 1024 || port > 65535) {
      js.Dynamic.literal(error = "Invalid port: must be 1024\u201365535")
    } else {
      val current = state.get()
      ProxyLifecycle.onSpawn(current.lifecycle) match {
        case None =>
          current.lifecycle match {
            case ProxyLifecycle.Running =>
              js.Dynamic.literal(running = true, port = current.port.getOrElse(port))
            case ProxyLifecycle.Idle | ProxyLifecycle.Stopping =>
              /* Spawning on top of a dying child races it for the port and
               * loses with `Address already in use`. The renderer surfaces
               * this through `proxy.startFail` (see ProxyControl). */
              js.Dynamic.literal(error = "The previous proxy is still shutting down. Try again in a moment.")
          }

        case Some(nextLifecycle) =>
          val binaryPath = Config.proxyBinaryPath
          /* The filter config file must exist before the binary starts reading it per request. */
          RequestFilterStore.ensureExists()
          val child      = ChildProcessModule.spawn(
            binaryPath,
            js.Array(
              "--port",
              port.toString,
              "--filter-config",
              RequestFilterStore.filePath,
              /* Tie the binary's lifetime to this stdin pipe: whatever kills
               * the main process - Force Quit, a crash, SIGKILL - closes it,
               * and the child exits rather than outliving the app. */
              "--exit-on-stdin-close",
            ),
            js.Dynamic
              .literal(
                stdio = js.Array("pipe", "pipe", "pipe"),
              )
              .asInstanceOf[js.Object],
          )

          state.set(ProxyState(child.some, port.some, "", nextLifecycle))

          child.stdout.setEncoding("utf8")

          child
            .stdout
            .on(
              "data",
              { (chunk: js.Any) =>
                val text    = chunk.toString
                val updated = state.updateAndGet(s => s.copy(buffer = s.buffer + text))
                val parts   = updated.buffer.split("\n", -1)
                state.updateAndGet(s => s.copy(buffer = parts.last)): Unit
                parts.init.foreach { line =>
                  val trimmed = line.trim
                  if (trimmed.nonEmpty) processProxyEvent(trimmed, getMainWindow) else ()
                }
              }: js.Function1[js.Any, Unit]
            )

          child.on(
            "exit",
            { (_: js.Any) =>
              /* The only authority on the child being gone. */
              clearPendingTimers()
              state.set(ProxyState.empty)
              RouteSync.onProxyStopped(getMainWindow)
              pushProxyState(js.Dynamic.literal(state = "stopped"), getMainWindow)
              drainWhenGone()
            }: js.Function1[js.Any, Unit]
          )

          child.on(
            "error",
            { (_: js.Any) =>
              clearPendingTimers()
              state.updateAndGet(s =>
                s.copy(process = none[ChildProcess], port = none[Int], lifecycle = ProxyLifecycle.Idle)
              ): Unit
              RouteSync.onProxyStopped(getMainWindow)
              pushProxyState(
                js.Dynamic.literal(state = "error", message = "failed to launch proxy binary"),
                getMainWindow,
              )
              drainWhenGone()
            }: js.Function1[js.Any, Unit]
          )

          Analytics.trackEvent("proxy_started")
          js.Dynamic.literal(running = true, port = port)
      }
    }
  }

  /** Validate a JSON line from the native proxy and forward the
    * renderer-facing events.
    *
    * The line is decoded through [[JsonLineProtocol.decode]] — the
    * exact typed counterpart of the proxy's `EventEmitter.encode` — so
    * a malformed line or an unexpected event shape produces a `Left`
    * that is logged rather than silently swallowed, and the
    * event-type dispatch is an exhaustive match on the `ProxyEvent`
    * ADT (a new event case becomes a compile-time prompt here).
    *
    * On success we forward the original `JSON.parse`d `js.Dynamic`
    * payload over IPC — the renderer consumes captures as
    * `js.Dynamic`, so re-encoding the typed ADT back to JS would add
    * a per-event JSON round-trip for no benefit. The typed `event`
    * drives channel selection; the raw dynamic carries the payload.
    */
  private def processProxyEvent(
    line: String,
    getMainWindow: () => Option[BrowserWindow],
  ): Unit = {
    JsonLineProtocol.decode(line) match {
      case Left(err) =>
        /* No longer silent: a malformed or unexpected line is logged.
         * The proxy's stdout is the IPC protocol channel (see
         * `EventEmitter`); only encoded `ProxyEvent`s appear there, so
         * this warns only on genuine drift, never on benign output. */
        val _ = js.Dynamic.global.console.warn("Dropped proxy event line:", err)
      case Right(event) =>
        event match {
          case ProxyEvent.ProxyStarted(port) =>
            /* Authoritative "proxy is up" signal: carries the port the
             * binary actually bound, unlike startProxy's optimistic
             * return. */
            RouteSync.onProxyStarted(port, getMainWindow)
            pushProxyState(js.Dynamic.literal(state = "started", port = port), getMainWindow)
          case ProxyEvent.ProxyStopped =>
            RouteSync.onProxyStopped(getMainWindow)
            pushProxyState(js.Dynamic.literal(state = "stopped"), getMainWindow)
          case ProxyEvent.ProxyError(message) =>
            RouteSync.onProxyStopped(getMainWindow)
            pushProxyState(js.Dynamic.literal(state = "error", message = message), getMainWindow)
          case _: ProxyEvent.RequestCaptured | _: ProxyEvent.ResponseCaptured =>
            getMainWindow().foreach { win =>
              if (!win.isDestroyed()) {
                // Forced only for the two renderer-facing cases; internal
                // events skip the second parse.
                lazy val parsed = JSON.parse(line)
                event match {
                  case _: ProxyEvent.RequestCaptured =>
                    win.webContents.send(IpcChannels.ProxyRequest, parsed.selectDynamic("request"))
                  case _: ProxyEvent.ResponseCaptured =>
                    win.webContents.send(IpcChannels.ProxyResponse, parsed.selectDynamic("response"))
                  case ProxyEvent.ProxyStarted(_) | ProxyEvent.ProxyStopped | ProxyEvent.ProxyError(_) =>
                    () // handled above — unreachable in this branch
                }
              } else ()
            }
        }
    }
  }

  /** Forward a truthful proxy state change to the renderer's proxy bar. */
  private def pushProxyState(payload: js.Dynamic, getMainWindow: () => Option[BrowserWindow]): Unit = {
    getMainWindow().foreach { win =>
      if (!win.isDestroyed()) {
        win.webContents.send(IpcChannels.ProxyState, payload)
      } else ()
    }
  }

  private def stopProxy(getMainWindow: () => Option[BrowserWindow]): js.Dynamic = {
    stopProxy(() => ()): Unit
    RouteSync.onProxyStopped(getMainWindow)
    js.Dynamic.literal(stopped = true)
  }

  /* Reports the lifecycle, never `child.killed`: that flag only says a signal
   * was sent, so trusting it used to drop the handle of a child that was still
   * alive and still holding the port. */
  private def getStatus: js.Dynamic = {
    val current = state.get()
    js.Dynamic
      .literal(
        running = ProxyLifecycle.reportsRunning(current.lifecycle),
        port = current.port.getOrElse(0),
        routeMode = RouteSync.mode.wire,
      )
  }
}
