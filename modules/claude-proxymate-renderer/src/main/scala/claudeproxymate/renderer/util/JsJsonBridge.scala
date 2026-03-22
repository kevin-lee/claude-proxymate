package claudeproxymate.renderer.util

import io.circe.Json
import io.circe.parser.parse

import scala.scalajs.js

/** Converts `js.Dynamic` values to `io.circe.Json`.
  *
  * Strategy: `js.JSON.stringify()` then `io.circe.parser.parse()`.
  * Simple, correct, and adequate performance for UI data sizes.
  */
object JsJsonBridge {

  def toCirceJson(dyn: js.Dynamic): Either[String, Json] = {
    if (js.isUndefined(dyn) || dyn == null) Right(Json.Null)
    else {
      val str = js.JSON.stringify(dyn)
      parse(str).left.map(_.getMessage)
    }
  }

  def toCirceJsonUnsafe(dyn: js.Dynamic): Json =
    toCirceJson(dyn).getOrElse(Json.Null)

  def toJsDynamic(json: Json): js.Dynamic =
    js.JSON.parse(json.noSpaces)
}
