package claudeproxymate.renderer.proxy

import hedgehog.*
import hedgehog.runner.*

object ProxyStateEventSpec extends Properties {

  override def tests: List[Test] = List(
    property("started carries the port", testStarted),
    example("stopped parses", testStopped),
    property("error carries the message", testError),
    property("unknown state strings parse to None", testUnknown),
  )

  def testStarted: Property =
    for {
      port <- Gen.int(Range.linear(1024, 65535)).log("port")
    } yield {
      ProxyControl.parseStateEvent("started", port, "") ====
        Some(ProxyControl.ProxyStateEvent.Started(port))
    }

  def testStopped: Result =
    ProxyControl.parseStateEvent("stopped", 0, "") ==== Some(ProxyControl.ProxyStateEvent.Stopped)

  def testError: Property =
    for {
      message <- Gen.string(Gen.alphaNum, Range.linear(0, 40)).log("message")
    } yield {
      ProxyControl.parseStateEvent("error", 0, message) ====
        Some(ProxyControl.ProxyStateEvent.ErrorState(message))
    }

  def testUnknown: Property =
    for {
      state <- Gen.element1("", "STARTED", "starting", "bogus", "proxy_started").log("state")
    } yield {
      ProxyControl.parseStateEvent(state, 8888, "m") ==== None
    }
}
