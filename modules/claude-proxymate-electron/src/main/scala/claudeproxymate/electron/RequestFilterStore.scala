package claudeproxymate.electron

import claudeproxymate.core.filter.FilterConfig
import claudeproxymate.electron.facades.{ElectronApp, NodeFs, NodePath}
import io.circe.parser.decode
import io.circe.syntax.*

import scala.scalajs.js
import scala.scalajs.js.JSON

/** Owns `<userData>/request-filter.json`, the request filter configuration.
  *
  * The proxy binary re-reads this file on every request (its path is passed as
  * `--filter-config`), so a successful [[save]] applies without a restart. The
  * file is validated with the shared [[FilterConfig]] codec on both read and
  * write; a missing or invalid file reads as [[FilterConfig.default]].
  */
object RequestFilterStore {

  private val FileName: String = "request-filter.json"

  val filePath: String = NodePath.join(ElectronApp.getPath("userData"), FileName)

  private def warn(message: String): Unit = {
    val _ = js.Dynamic.global.console.warn(s"request-filter: $message")
  }

  private def toJs(config: FilterConfig): js.Dynamic = JSON.parse(config.asJson.noSpaces)

  /** The persisted config, or the default when the file is missing or invalid. */
  def readConfig(): FilterConfig =
    try {
      if (!NodeFs.existsSync(filePath)) FilterConfig.default
      else {
        decode[FilterConfig](NodeFs.readFileSync(filePath, "utf8")) match {
          case Right(config) => config
          case Left(err) =>
            warn(s"invalid $filePath, using defaults: ${err.getMessage}")
            FilterConfig.default
        }
      }
    } catch {
      case e: Throwable =>
        warn(s"cannot read $filePath, using defaults: ${e.getMessage}")
        FilterConfig.default
    }

  /** IPC: the config as a plain JS object. */
  def load(): js.Dynamic = toJs(readConfig())

  /** IPC: validate and persist. `{ok: true, config}` or `{ok: false, reason}`. */
  def save(raw: js.Dynamic): js.Dynamic = {
    val text = if (js.isUndefined(raw) || raw == null) "" else JSON.stringify(raw)
    decode[FilterConfig](text) match {
      case Left(err) =>
        js.Dynamic.literal(ok = false, reason = err.getMessage)
      case Right(config) =>
        try {
          NodeFs.writeFileSync(filePath, config.asJson.spaces2)
          Analytics.trackEvent("filter_config_saved")
          js.Dynamic.literal(ok = true, config = toJs(config))
        } catch {
          case e: Throwable =>
            js.Dynamic.literal(ok = false, reason = s"cannot write $filePath: ${e.getMessage}")
        }
    }
  }

  /** Write the default config when no file exists yet (called before the proxy is spawned). */
  def ensureExists(): Unit =
    try {
      if (!NodeFs.existsSync(filePath)) NodeFs.writeFileSync(filePath, FilterConfig.default.asJson.spaces2) else ()
    } catch {
      case e: Throwable => warn(s"cannot create $filePath: ${e.getMessage}")
    }
}
