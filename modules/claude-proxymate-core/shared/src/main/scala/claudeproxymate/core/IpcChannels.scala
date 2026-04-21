package claudeproxymate.core

/** IPC channel names shared between Electron main process, preload bridge, and renderer. */
object IpcChannels {
  val ProxyStart: String        = "proxy-start"
  val ProxyStop: String         = "proxy-stop"
  val ProxyStatus: String       = "proxy-status"
  val ProxyRequest: String      = "proxy-request"
  val ProxyResponse: String     = "proxy-response"
  val ShellOpenExternal: String = "shell-open-external"
  val BridgeName: String        = "electronAPI"

  val invokeChannels: List[String] = List(ProxyStart, ProxyStop, ProxyStatus, ShellOpenExternal)
  val eventChannels: List[String]  = List(ProxyRequest, ProxyResponse)
}
