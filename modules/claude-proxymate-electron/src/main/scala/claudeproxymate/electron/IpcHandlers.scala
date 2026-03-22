package claudeproxymate.electron

import claudeproxymate.core.IpcChannels
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
    val empty: ProxyState = ProxyState(None, None, "")
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
        stopProxy()
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )

    IpcMain.handle(
      IpcChannels.ProxyStatus,
      { (_: js.Dynamic, _: js.Dynamic) =>
        getStatus()
      }: js.Function2[js.Dynamic, js.Dynamic, js.Any]
    )
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

          state.set(ProxyState(Some(child), Some(port), ""))

          child.stdout.setEncoding("utf8")

          child
            .stdout
            .on(
              "data",
              { (chunk: js.Any) =>
                val text    = chunk.toString()
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
            }: js.Function1[js.Any, Unit]
          )

          child.on(
            "error",
            { (_: js.Any) =>
              state.updateAndGet(s => s.copy(process = None, port = None)): Unit
            }: js.Function1[js.Any, Unit]
          )

          Analytics.trackEvent("proxy_started")
          js.Dynamic.literal(running = true, port = port)
      }
    }
  }

  /** Parse a JSON line from the native proxy and forward to the renderer. */
  private def processProxyEvent(
    line: String,
    getMainWindow: () => Option[BrowserWindow],
  ): Unit = {
    try {
      val parsed    = JSON.parse(line)
      val eventType = parsed.selectDynamic("type").asInstanceOf[String]

      getMainWindow().foreach { win =>
        if (!win.isDestroyed()) {
          eventType match {
            case "request_captured" =>
              win.webContents.send(IpcChannels.ProxyRequest, parsed.selectDynamic("request"))
            case "response_captured" =>
              win.webContents.send(IpcChannels.ProxyResponse, parsed.selectDynamic("response"))
            case _ => () // proxy_started, proxy_stopped, proxy_error — internal events
          }
        } else ()
      }
    } catch {
      case _: Throwable => () // ignore malformed lines
    }
  }

  private def stopProxy(): js.Dynamic = {
    stopProxyIfRunning()
    js.Dynamic.literal(stopped = true)
  }

  private def getStatus(): js.Dynamic = {
    val current = state.get()
    current.process match {
      case Some(child) if !child.killed =>
        js.Dynamic.literal(running = true, port = current.port.getOrElse(0))
      case _ =>
        state.updateAndGet(s => s.copy(process = None, port = None)): Unit
        js.Dynamic.literal(running = false)
    }
  }
}
