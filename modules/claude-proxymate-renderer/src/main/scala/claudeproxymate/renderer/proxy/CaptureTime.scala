package claudeproxymate.renderer.proxy

import scala.scalajs.js

/** Formats the capture time of a request in the local time zone.
  *
  * The proxy sends the capture time as epoch milliseconds (UTC). The
  * functions here take the epoch milliseconds and the offset in minutes, in
  * the convention of JavaScript `Date.getTimezoneOffset`: UTC minus local
  * time, so UTC+11 is -660. Only [[localOffsetMinutes]] reads the time zone
  * of the machine, so [[formatTime]] and [[formatDateTime]] give the same
  * result on every machine.
  */
object CaptureTime {

  /** The local time as `HH:mm:ss`. */
  def formatTime(epochMillis: Double, offsetMinutes: Int): String = {
    val d = shifted(epochMillis, offsetMinutes)
    f"${d.getUTCHours().toInt}%02d:${d.getUTCMinutes().toInt}%02d:${d.getUTCSeconds().toInt}%02d"
  }

  /** The local date, time and offset as `yyyy-MM-dd HH:mm:ss UTC±hh:mm`. */
  def formatDateTime(epochMillis: Double, offsetMinutes: Int): String = {
    val d    = shifted(epochMillis, offsetMinutes)
    val date = f"${d.getUTCFullYear().toInt}%04d-${d.getUTCMonth().toInt + 1}%02d-${d.getUTCDate().toInt}%02d"
    s"$date ${formatTime(epochMillis, offsetMinutes)} ${offsetText(offsetMinutes)}"
  }

  /** The offset of the local time zone of the machine at that instant, in
    * the `getTimezoneOffset` convention. It is the offset at that instant, so
    * daylight saving time is correct.
    */
  def localOffsetMinutes(epochMillis: Double): Int =
    new js.Date(epochMillis).getTimezoneOffset().toInt

  /* A Date whose UTC fields are the local fields. */
  private def shifted(epochMillis: Double, offsetMinutes: Int): js.Date =
    new js.Date(epochMillis - offsetMinutes * 60000.0)

  private def offsetText(offsetMinutes: Int): String = {
    val east = -offsetMinutes
    val sign = if (east >= 0) "+" else "-"
    val abs  = math.abs(east)
    f"UTC$sign${abs / 60}%02d:${abs % 60}%02d"
  }
}
