package claudeproxymate.electron

import claudeproxymate.core.update.UpdateSettings
import claudeproxymate.electron.facades.{ElectronApp, NodeFs, NodePath}
import io.circe.parser.decode
import io.circe.syntax.*

import scala.scalajs.js
import scala.scalajs.js.JSON

/** Owns `<userData>/update-settings.json`, the self-update settings.
  *
  * The file is validated with the shared [[UpdateSettings]] codec on both read
  * and write; a missing or invalid file reads as [[UpdateSettings.default]].
  */
object UpdateSettingsStore {

  private val FileName: String = "update-settings.json"

  lazy val filePath: String = NodePath.join(ElectronApp.getPath("userData"), FileName)

  private def warn(message: String): Unit = {
    val _ = js.Dynamic.global.console.warn(s"update-settings: $message")
  }

  def toJs(settings: UpdateSettings): js.Dynamic = JSON.parse(settings.asJson.noSpaces)

  /** The persisted settings, or the default when the file is missing or invalid. */
  def read(): UpdateSettings =
    try {
      if (!NodeFs.existsSync(filePath)) UpdateSettings.default
      else {
        decode[UpdateSettings](NodeFs.readFileSync(filePath, "utf8")) match {
          case Right(settings) => settings
          case Left(err) =>
            warn(s"invalid $filePath, using defaults: ${err.getMessage}")
            UpdateSettings.default
        }
      }
    } catch {
      case e: Throwable =>
        warn(s"cannot read $filePath, using defaults: ${e.getMessage}")
        UpdateSettings.default
    }

  /** IPC: validate and persist. `{ok: true, settings}` or `{ok: false, reason}`. */
  def save(raw: js.Dynamic): js.Dynamic = {
    val text = if (js.isUndefined(raw) || raw == null) "" else JSON.stringify(raw)
    decode[UpdateSettings](text) match {
      case Left(err) =>
        js.Dynamic.literal(ok = false, reason = err.getMessage)
      case Right(settings) =>
        try {
          NodeFs.writeFileSync(filePath, settings.asJson.spaces2)
          js.Dynamic.literal(ok = true, settings = toJs(settings))
        } catch {
          case e: Throwable =>
            js.Dynamic.literal(ok = false, reason = s"cannot write $filePath: ${e.getMessage}")
        }
    }
  }
}
