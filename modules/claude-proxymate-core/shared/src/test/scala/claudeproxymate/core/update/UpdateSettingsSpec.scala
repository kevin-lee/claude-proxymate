package claudeproxymate.core.update

import hedgehog.*
import hedgehog.runner.*
import io.circe.parser.decode
import io.circe.syntax.*

object UpdateSettingsSpec extends Properties {

  override def tests: List[Test] = List(
    property("encode then decode round-trips", testRoundTrip),
    example("empty object decodes to the default settings", testEmptyObject),
    example("the default turns auto update on", testDefault),
  )

  def testRoundTrip: Property =
    for {
      autoUpdate <- Gen.boolean.log("autoUpdate")
    } yield {
      val settings = UpdateSettings(autoUpdate)
      val json     = settings.asJson.noSpaces
      (decode[UpdateSettings](json) ==== Right(settings)).log(s"round-trip mismatch: json=$json")
    }

  def testEmptyObject: Result =
    (decode[UpdateSettings]("{}") ==== Right(UpdateSettings.default)).log("{} should decode to the default")

  def testDefault: Result =
    UpdateSettings.default ==== UpdateSettings(autoUpdate = true)
}
