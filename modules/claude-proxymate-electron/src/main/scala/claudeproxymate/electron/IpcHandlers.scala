package claudeproxymate.electron

import cats.syntax.all.*
import claudeproxymate.core.{IpcChannels, JsonLineProtocol, ProxyEvent, RouteMode, UrlScheme}
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
  )

  private object ProxyState {
    val empty: ProxyState = ProxyState(none[ChildProcess], none[Int], "")
  }

  private val state = new AtomicReference[ProxyState](ProxyState.empty)

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

  /** Kill the proxy child process if running. Called on app quit. */
  def stopProxyIfRunning(): Unit = {
    state.get().process.foreach { child =>
      try { child.kill("SIGTERM") }
      catch { case _: Throwable => () }
    }
    state.set(ProxyState.empty)
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
      current.process match {
        case Some(_) =>
          js.Dynamic.literal(running = true, port = current.port.getOrElse(port))

        case None =>
          val binaryPath = Config.proxyBinaryPath
          val child      = ChildProcessModule.spawn(
            binaryPath,
            js.Array("--port", port.toString),
            js.Dynamic
              .literal(
                stdio = js.Array("pipe", "pipe", "pipe"),
              )
              .asInstanceOf[js.Object],
          )

          state.set(ProxyState(child.some, port.some, ""))

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
              state.set(ProxyState.empty)
              RouteSync.onProxyStopped(getMainWindow)
              pushProxyState(js.Dynamic.literal(state = "stopped"), getMainWindow)
            }: js.Function1[js.Any, Unit]
          )

          child.on(
            "error",
            { (_: js.Any) =>
              state.updateAndGet(s => s.copy(process = none[ChildProcess], port = none[Int])): Unit
              RouteSync.onProxyStopped(getMainWindow)
              pushProxyState(
                js.Dynamic.literal(state = "error", message = "failed to launch proxy binary"),
                getMainWindow,
              )
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
    stopProxyIfRunning()
    RouteSync.onProxyStopped(getMainWindow)
    js.Dynamic.literal(stopped = true)
  }

  private def getStatus: js.Dynamic = {
    val current = state.get()
    current.process match {
      case Some(child) if !child.killed =>
        js.Dynamic.literal(running = true, port = current.port.getOrElse(0), routeMode = RouteSync.mode.wire)
      case Some(_) | None =>
        state.updateAndGet(s => s.copy(process = none[ChildProcess], port = none[Int])): Unit
        js.Dynamic.literal(running = false, routeMode = RouteSync.mode.wire)
    }
  }
}
