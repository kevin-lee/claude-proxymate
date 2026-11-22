package claudeproxymate.renderer.proxy

import hedgehog.*
import hedgehog.runner.*

object CaptureTimeSpec extends Properties {

  override def tests: List[Test] = List(
    example("formatTime in UTC+11", testTimeUtcPlus11),
    example("formatDateTime in UTC+11", testDateTimeUtcPlus11),
    example("formatTime and formatDateTime in UTC", testUtc),
    example("formatDateTime in UTC+11 moves to the next day", testNextDay),
    example("formatDateTime in UTC-5 moves to the previous day", testPreviousDay),
    example("formatDateTime in UTC+5:30", testHalfHourOffset),
    property("formatTime and formatDateTime have the expected shape", testShape),
    property("formatTime with an offset equals formatTime of the shifted instant in UTC", testOffsetShift),
  )

  /* 2026-11-15T11:16:04.123Z */
  private val Sample: Double = 1794741364123.0

  def testTimeUtcPlus11: Result =
    CaptureTime.formatTime(Sample, -660) ==== "22:16:04"

  def testDateTimeUtcPlus11: Result =
    CaptureTime.formatDateTime(Sample, -660) ==== "2026-11-15 22:16:04 UTC+11:00"

  def testUtc: Result =
    Result.all(
      List(
        CaptureTime.formatTime(Sample, 0) ==== "11:16:04",
        CaptureTime.formatDateTime(Sample, 0) ==== "2026-11-15 11:16:04 UTC+00:00",
      )
    )

  /* 2026-11-15T13:30:00Z */
  def testNextDay: Result =
    CaptureTime.formatDateTime(1794749400000.0, -660) ==== "2026-11-16 00:30:00 UTC+11:00"

  /* 2026-11-15T02:05:09Z */
  def testPreviousDay: Result =
    CaptureTime.formatDateTime(1794708309000.0, 300) ==== "2026-11-14 21:05:09 UTC-05:00"

  def testHalfHourOffset: Result =
    CaptureTime.formatDateTime(Sample, -330) ==== "2026-11-15 16:46:04 UTC+05:30"

  def testShape: Property =
    for {
      ms     <- Gen.long(Range.linear(0L, 4102444800000L)).log("epochMillis")
      offset <- Gen.int(Range.linear(-840, 720)).log("offsetMinutes")
    } yield {
      val time     = CaptureTime.formatTime(ms.toDouble, offset)
      val dateTime = CaptureTime.formatDateTime(ms.toDouble, offset)
      Result.all(
        List(
          Result.assert(time.matches("""\d{2}:\d{2}:\d{2}""")).log(s"time: $time"),
          Result
            .assert(dateTime.matches("""\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} UTC[+-]\d{2}:\d{2}"""))
            .log(s"dateTime: $dateTime"),
          Result.assert(dateTime.contains(s" $time ")).log(s"dateTime $dateTime does not contain time $time"),
        )
      )
    }

  def testOffsetShift: Property =
    for {
      ms     <- Gen.long(Range.linear(0L, 4102444800000L)).log("epochMillis")
      offset <- Gen.int(Range.linear(-840, 720)).log("offsetMinutes")
    } yield CaptureTime.formatTime(ms.toDouble, offset) ==== CaptureTime.formatTime(ms.toDouble - offset * 60000.0, 0)
}
