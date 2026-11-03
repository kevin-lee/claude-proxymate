package claudeproxymate.core.update

import hedgehog.*
import hedgehog.runner.*
import io.circe.parser.decode
import io.circe.syntax.*

object UpdateStateSpec extends Properties {

  override def tests: List[Test] = List(
    property("encode then decode round-trips", testRoundTrip),
    property("clampPercent is always within 0..100", testClampPercentRange),
    example("clampPercent of NaN is 0", testClampPercentNaN),
    example("clampPercent rounds down", testClampPercentFloor),
    example("canCheck for every state", testCanCheck),
    example("canDownload for every state", testCanDownload),
    example("canInstall for every state", testCanInstall),
    example("unknown kind fails to decode", testUnknownKind),
    example("displayMessage keeps the first line of a GitHub 404 message", testDisplayMessageGitHub404),
    example("displayMessage keeps a short one-line message", testDisplayMessageShort),
    example("displayMessage of a blank message is Unknown error", testDisplayMessageBlank),
    example("displayMessage skips blank leading lines", testDisplayMessageLeadingBlank),
    example("displayMessage cuts a long line with …", testDisplayMessageLong),
    property("displayMessage is one line within the length limit", testDisplayMessageOneLine),
  )

  private def genVersion: Gen[String] =
    for {
      major <- Gen.int(Range.linear(0, 20))
      minor <- Gen.int(Range.linear(0, 20))
      patch <- Gen.int(Range.linear(0, 20))
    } yield s"$major.$minor.$patch"

  private def genTime: Gen[Double] =
    Gen.long(Range.linear(0L, 4102444800000L)).map(_.toDouble)

  private def genPhase: Gen[UpdatePhase] =
    Gen.element1(UpdatePhase.Check, UpdatePhase.Download, UpdatePhase.Install)

  def genState: Gen[UpdateState] =
    Gen.choice1(
      Gen.constant(UpdateState.unsupported),
      Gen.constant(UpdateState.idle),
      Gen.boolean.map(UpdateState.checking),
      genTime.map(UpdateState.upToDate),
      for {
        version   <- genVersion
        checkedAt <- genTime
      } yield UpdateState.available(version, checkedAt),
      for {
        version <- genVersion
        percent <- Gen.int(Range.linear(0, 100))
      } yield UpdateState.downloading(version, percent),
      genVersion.map(UpdateState.ready),
      for {
        phase         <- genPhase
        message       <- Gen.string(Gen.alphaNum, Range.linear(0, 40))
        userInitiated <- Gen.boolean
        checkedAt     <- genTime.option
      } yield UpdateState.failed(phase, message, userInitiated, checkedAt),
    )

  def testRoundTrip: Property =
    for {
      state <- genState.log("state")
    } yield {
      val json = state.asJson.noSpaces
      (decode[UpdateState](json) ==== Right(state)).log(s"round-trip mismatch: json=$json")
    }

  def testClampPercentRange: Property =
    for {
      d <- Gen.double(Range.linearFrac(-1000.0, 1000.0)).log("d")
    } yield {
      val p = UpdateState.clampPercent(d)
      Result.assert(p >= 0 && p <= 100).log(s"clampPercent($d) = $p")
    }

  def testClampPercentNaN: Result =
    UpdateState.clampPercent(Double.NaN) ==== 0

  def testClampPercentFloor: Result =
    Result.all(
      List(
        UpdateState.clampPercent(42.9) ==== 42,
        UpdateState.clampPercent(100.0) ==== 100,
        UpdateState.clampPercent(-0.5) ==== 0,
        UpdateState.clampPercent(250.0) ==== 100,
      )
    )

  private val failedCheck: UpdateState    = UpdateState.failed(UpdatePhase.Check, "x", false, None)
  private val failedDownload: UpdateState = UpdateState.failed(UpdatePhase.Download, "x", true, None)
  private val failedInstall: UpdateState  = UpdateState.failed(UpdatePhase.Install, "x", true, None)

  def testCanCheck: Result =
    Result.all(
      List(
        UpdateState.canCheck(UpdateState.unsupported) ==== false,
        UpdateState.canCheck(UpdateState.idle) ==== true,
        UpdateState.canCheck(UpdateState.checking(true)) ==== false,
        UpdateState.canCheck(UpdateState.upToDate(1.0)) ==== true,
        UpdateState.canCheck(UpdateState.available("1.0.0", 1.0)) ==== true,
        UpdateState.canCheck(UpdateState.downloading("1.0.0", 5)) ==== false,
        UpdateState.canCheck(UpdateState.ready("1.0.0")) ==== false,
        UpdateState.canCheck(failedCheck) ==== true,
        UpdateState.canCheck(failedDownload) ==== true,
        UpdateState.canCheck(failedInstall) ==== true,
      )
    )

  def testCanDownload: Result =
    Result.all(
      List(
        UpdateState.canDownload(UpdateState.unsupported) ==== false,
        UpdateState.canDownload(UpdateState.idle) ==== false,
        UpdateState.canDownload(UpdateState.checking(false)) ==== false,
        UpdateState.canDownload(UpdateState.upToDate(1.0)) ==== false,
        UpdateState.canDownload(UpdateState.available("1.0.0", 1.0)) ==== true,
        UpdateState.canDownload(UpdateState.downloading("1.0.0", 5)) ==== false,
        UpdateState.canDownload(UpdateState.ready("1.0.0")) ==== false,
        UpdateState.canDownload(failedCheck) ==== false,
        UpdateState.canDownload(failedDownload) ==== true,
        UpdateState.canDownload(failedInstall) ==== false,
      )
    )

  def testCanInstall: Result =
    Result.all(
      List(
        UpdateState.canInstall(UpdateState.unsupported) ==== false,
        UpdateState.canInstall(UpdateState.idle) ==== false,
        UpdateState.canInstall(UpdateState.checking(false)) ==== false,
        UpdateState.canInstall(UpdateState.upToDate(1.0)) ==== false,
        UpdateState.canInstall(UpdateState.available("1.0.0", 1.0)) ==== false,
        UpdateState.canInstall(UpdateState.downloading("1.0.0", 5)) ==== false,
        UpdateState.canInstall(UpdateState.ready("1.0.0")) ==== true,
        UpdateState.canInstall(failedCheck) ==== false,
        UpdateState.canInstall(failedDownload) ==== false,
        UpdateState.canInstall(failedInstall) ==== false,
      )
    )

  def testUnknownKind: Result =
    Result.assert(decode[UpdateState]("""{"kind":"bogus"}""").isLeft).log("an unknown kind must fail decoding")

  def testDisplayMessageGitHub404: Result = {
    val firstLine =
      "Cannot find latest-mac.yml in the latest release artifacts " +
        "(https://github.com/kevin-lee/claude-proxymate/releases/download/v0.7.0/latest-mac.yml): HttpError: 404"
    val raw       =
      firstLine + " \n" +
        "\"method: GET url: https://github.com/kevin-lee/claude-proxymate/releases/download/v0.7.0/latest-mac.yml\"\n" +
        "\n" +
        "Headers: {\n  \"content-type\": \"text/plain; charset=utf-8\"\n}\n" +
        "    at createHttpError (/app/node_modules/builder-util-runtime/out/httpExecutor.js:55:12)\n"
    val actual    = UpdateState.displayMessage(raw)
    Result.all(
      List(
        actual ==== firstLine,
        Result.assert(!actual.contains("Headers:")).log(actual),
        Result.assert(!actual.contains("\n")).log(actual),
      )
    )
  }

  def testDisplayMessageShort: Result =
    UpdateState.displayMessage("net::ERR_CONNECTION_REFUSED") ==== "net::ERR_CONNECTION_REFUSED"

  def testDisplayMessageBlank: Result =
    Result.all(
      List(
        UpdateState.displayMessage("") ==== "Unknown error",
        UpdateState.displayMessage("  \n \n") ==== "Unknown error",
      )
    )

  def testDisplayMessageLeadingBlank: Result =
    UpdateState.displayMessage("\n\n  boom  \nmore") ==== "boom"

  def testDisplayMessageLong: Result = {
    val actual = UpdateState.displayMessage("x" * 500)
    Result.all(
      List(
        Result.assert(actual.length <= UpdateState.MaxDisplayMessageLength).log(s"length: ${actual.length}"),
        Result.assert(actual.endsWith("…")).log(actual),
      )
    )
  }

  def testDisplayMessageOneLine: Property =
    for {
      lines     <- Gen
                     .choice1(
                       Gen.string(Gen.alphaNum, Range.linear(0, 400)),
                       Gen.string(Gen.constant(' '), Range.linear(0, 5)),
                     )
                     .list(Range.linear(0, 6))
                     .log("lines")
      separator <- Gen.element1("\n", "\r\n").log("separator")
    } yield {
      val actual = UpdateState.displayMessage(lines.mkString(separator))
      Result.all(
        List(
          Result.assert(!actual.contains("\n") && !actual.contains("\r")).log(s"not one line: $actual"),
          Result.assert(actual.nonEmpty).log("empty result"),
          Result
            .assert(actual.length <= UpdateState.MaxDisplayMessageLength)
            .log(s"too long: ${actual.length}"),
        )
      )
    }
}
