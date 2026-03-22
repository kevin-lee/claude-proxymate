package claudeproxymate.electron

import claudeproxymate.electron.facades._

import scala.scalajs.js
import scala.scalajs.js.JSON

/** IPC handlers for proxy lifecycle: spawn/kill native binary, parse stdout, forward events. */
object IpcHandlers {

  private var proxyProcess: Option[ChildProcess] = None
  private var proxyPort: Option[Int]             = None
  private var lineBuffer: String                 = ""

  def register(getMainWindow: () => Option[BrowserWindow]): Unit = {
    IpcMain.handle("proxy-start", { (_: js.Dynamic, portArg: js.Dynamic) =>
      startProxy(portArg, getMainWindow)
    }: js.Function2[js.Dynamic, js.Dynamic, js.Any])

    IpcMain.handle("proxy-stop", { (_: js.Dynamic, _: js.Dynamic) =>
      stopProxy()
    }: js.Function2[js.Dynamic, js.Dynamic, js.Any])

    IpcMain.handle("proxy-status", { (_: js.Dynamic, _: js.Dynamic) =>
      getStatus()
    }: js.Function2[js.Dynamic, js.Dynamic, js.Any])
  }

  /** Kill the proxy child process if running. Called on app quit. */
  def stopProxyIfRunning(): Unit = {
    proxyProcess.foreach { child =>
      try { child.kill("SIGTERM") } catch { case _: Throwable => () }
    }
    proxyProcess = None
    proxyPort = None
    lineBuffer = ""
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
      proxyProcess match {
        case Some(_) =>
          js.Dynamic.literal(running = true, port = proxyPort.getOrElse(port))

        case None =>
          val binaryPath = Config.proxyBinaryPath
          val child = ChildProcessModule.spawn(
            binaryPath,
            js.Array("--port", port.toString),
            js.Dynamic.literal(
              stdio = js.Array("pipe", "pipe", "pipe"),
            ).asInstanceOf[js.Object],
          )

          proxyProcess = Some(child)
          proxyPort = Some(port)
          lineBuffer = ""

          child.stdout.setEncoding("utf8")

          child.stdout.on("data", { (chunk: js.Any) =>
            val text = chunk.toString()
            lineBuffer += text
            val parts = lineBuffer.split("\n", -1)
            lineBuffer = parts.last
            parts.init.foreach { line =>
              val trimmed = line.trim
              if (trimmed.nonEmpty) {
                processProxyEvent(trimmed, getMainWindow)
              }
            }
          }: js.Function1[js.Any, Unit])

          child.on("exit", { (_: js.Any) =>
            proxyProcess = None
            proxyPort = None
            lineBuffer = ""
          }: js.Function1[js.Any, Unit])

          child.on("error", { (_: js.Any) =>
            proxyProcess = None
            proxyPort = None
          }: js.Function1[js.Any, Unit])

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
              win.webContents.send("proxy-request", parsed.selectDynamic("request"))
            case "response_captured" =>
              win.webContents.send("proxy-response", parsed.selectDynamic("response"))
            case _ => () // proxy_started, proxy_stopped, proxy_error — internal events
          }
        }
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
    proxyProcess match {
      case Some(child) if !child.killed =>
        js.Dynamic.literal(running = true, port = proxyPort.getOrElse(0))
      case _ =>
        proxyProcess = None
        proxyPort = None
        js.Dynamic.literal(running = false)
    }
  }
}
