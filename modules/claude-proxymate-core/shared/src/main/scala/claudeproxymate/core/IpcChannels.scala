package claudeproxymate.core

/** IPC channel names shared between Electron main process, preload bridge, and renderer. */
object IpcChannels {
  val ProxyStart: String          = "proxy-start"
  val ProxyStop: String           = "proxy-stop"
  val ProxyStatus: String         = "proxy-status"
  val ProxyRequest: String        = "proxy-request"
  val ProxyResponse: String       = "proxy-response"
  val ProxyState: String          = "proxy-state"
  val ShellOpenExternal: String   = "shell-open-external"
  val RouteModeSet: String        = "route-mode-set"
  val RouteModeEvent: String      = "route-mode-event"
  val FilterConfigGet: String     = "filter-config-get"
  val FilterConfigSet: String     = "filter-config-set"
  val ClaudeInventoryScan: String = "claude-inventory-scan"
  val AppVersion: String          = "app-version"
  val UpdateStateGet: String      = "update-state-get"
  val UpdateCheck: String         = "update-check"
  val UpdateDownload: String      = "update-download"
  val UpdateInstall: String       = "update-install"
  val UpdateSettingsGet: String   = "update-settings-get"
  val UpdateSettingsSet: String   = "update-settings-set"
  val UpdateStateEvent: String    = "update-state"
  val UpdateReveal: String        = "update-reveal"
  val BridgeName: String          = "electronAPI"

  val invokeChannels: List[String] =
    List(
      ProxyStart,
      ProxyStop,
      ProxyStatus,
      ShellOpenExternal,
      RouteModeSet,
      FilterConfigGet,
      FilterConfigSet,
      ClaudeInventoryScan,
      AppVersion,
      UpdateStateGet,
      UpdateCheck,
      UpdateDownload,
      UpdateInstall,
      UpdateSettingsGet,
      UpdateSettingsSet,
    )
  val eventChannels: List[String]  =
    List(ProxyRequest, ProxyResponse, ProxyState, RouteModeEvent, UpdateStateEvent, UpdateReveal)
}
