package claudeproxymate.core.update

import cats.*
import cats.derived.strict.*
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

/** The persisted self-update settings (`update-settings.json`).
  *
  * `autoUpdate`: when the app starts and finds an update, it downloads the update without a click.
  */
final case class UpdateSettings(autoUpdate: Boolean) derives CanEqual, Eq, Show
object UpdateSettings {
  val default: UpdateSettings = UpdateSettings(autoUpdate = true)

  given Encoder[UpdateSettings] = Encoder.instance { s =>
    Json.obj("autoUpdate" -> s.autoUpdate.asJson)
  }

  given Decoder[UpdateSettings] = Decoder.instance { c =>
    c.getOrElse[Boolean]("autoUpdate")(default.autoUpdate).map(UpdateSettings(_))
  }
}
