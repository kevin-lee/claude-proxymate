package claudeproxymate.electron

import hedgehog.*
import hedgehog.runner.*

import scala.scalajs.js

object ProxyCloseSpec extends Properties {

  override def tests: List[Test] = List(
    property("exitCode of a JS number is that number", testExitCodeNumber),
    example("exitCode of null, undefined and a string is None", testExitCodeNotNumber),
    property("exitSignal of a JS string is that string", testExitSignalString),
    example("exitSignal of null, undefined and a number is None", testExitSignalNotString),
    example("isCurrentChild is true only for the same object", testIsCurrentChild),
  )

  /* -2 is the code that Node gives after a spawn error (ENOENT). */
  def testExitCodeNumber: Property =
    for {
      code <- Gen.int(Range.linear(-2, 255)).log("code")
    } yield {
      ProxyClose.exitCode(code: js.Any) ==== Some(code)
    }

  def testExitCodeNotNumber: Result =
    Result.all(
      List(
        ProxyClose.exitCode(null) ==== None,
        ProxyClose.exitCode(js.undefined) ==== None,
        ProxyClose.exitCode("1": js.Any) ==== None,
      )
    )

  def testExitSignalString: Property =
    for {
      signal <- Gen.element1("SIGABRT", "SIGSEGV", "SIGKILL", "SIGTERM").log("signal")
    } yield {
      ProxyClose.exitSignal(signal: js.Any) ==== Some(signal)
    }

  def testExitSignalNotString: Result =
    Result.all(
      List(
        ProxyClose.exitSignal(null) ==== None,
        ProxyClose.exitSignal(js.undefined) ==== None,
        ProxyClose.exitSignal(9: js.Any) ==== None,
      )
    )

  def testIsCurrentChild: Result = {
    val a = new js.Object()
    val b = new js.Object()
    Result.all(
      List(
        Result.assert(ProxyClose.isCurrentChild(Some(a), a)).log("Some(a), a"),
        Result.assert(!ProxyClose.isCurrentChild(Some(a), b)).log("Some(a), b"),
        Result.assert(!ProxyClose.isCurrentChild(None, a)).log("None, a"),
      )
    )
  }
}
