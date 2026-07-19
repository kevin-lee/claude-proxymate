package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object RouteModeSpec extends Properties {

  override def tests: List[Test] = List(
    property("wire/parse round-trip for every mode", testWireParseRoundTrip),
    property("parse of a non-wire string => None", testParseUnknownNone),
    example("wire strings are exactly manual/vscode/global", testWireStrings),
    example("default route mode is Global", testDefaultIsGlobal),
  )

  private def genMode: Gen[RouteMode] =
    Gen.element1(RouteMode.Manual, RouteMode.VsCode, RouteMode.Global)

  def testWireParseRoundTrip: Property =
    for {
      mode <- genMode.log("mode")
    } yield {
      RouteMode.parse(mode.wire) ==== Some(mode)
    }

  def testParseUnknownNone: Property =
    for {
      s <- Gen.string(Gen.alphaNum, Range.linear(0, 12)).log("s")
    } yield {
      val candidate = s"x$s"
      RouteMode.parse(candidate) ==== None
    }

  def testWireStrings: Result =
    Result.all(
      List(
        RouteMode.Manual.wire ==== "manual",
        RouteMode.VsCode.wire ==== "vscode",
        RouteMode.Global.wire ==== "global",
      )
    )

  def testDefaultIsGlobal: Result =
    RouteMode.default ==== RouteMode.Global
}
