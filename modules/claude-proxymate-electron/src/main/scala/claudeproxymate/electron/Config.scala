package claudeproxymate.electron

import claudeproxymate.electron.facades.NodePath

import scala.scalajs.js

/** Environment variable access and configuration. */
object Config {

  private val process = js.Dynamic.global.process

  def env(name: String): Option[String] = {
    val value = process.env.selectDynamic(name)
    if (js.isUndefined(value) || value == null) None
    else Some(value.asInstanceOf[String])
  }

  /** Path to the native proxy binary.
    * In packaged app: process.resourcesPath / "claude-proxymate"
    * In development: PROXY_BINARY_PATH env var, or "claude-proxymate" in PATH
    */
  def proxyBinaryPath: String = {
    env("PROXY_BINARY_PATH").getOrElse {
      val resourcesPath = process.resourcesPath
      if (js.isUndefined(resourcesPath) || resourcesPath == null) {
        "claude-proxymate"
      } else {
        NodePath.join(resourcesPath.asInstanceOf[String], "claude-proxymate")
      }
    }
  }
}
