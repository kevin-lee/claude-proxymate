package claudeproxymate.renderer.testkit

import hedgehog.*
import hedgehog.runner.*
import org.scalajs.dom

import scala.scalajs.js

/** Checks that the test environment (jsdom with the stand-ins of
  * [[DomTestKit]]) has the browser APIs that the DOM handler tests use.
  */
object DomProbeSpec extends Properties {

  override def tests: List[Test] = List(
    example("jsdom gives a document, CSS.escape and TextEncoder", testJsdomApis),
    example("requestAnimationFrame callbacks wait until runAnimationFrames", testAnimationFrames),
    example("navigator.clipboard.writeText records the text", testClipboard),
    example("a click bubbles to the document", testClickBubbles),
    example("an exception in a listener is recorded", testListenerError),
  )

  private def window: js.Dynamic = dom.window.asInstanceOf[js.Dynamic] // scalafix:ok DisableSyntax.asInstanceOf

  private def testJsdomApis: Result = {
    DomTestKit.setUp("""<div id="x"></div>""")
    Result.all(
      List(
        Result.assert(DomTestKit.elementOption("#x").isDefined).log("document.querySelector"),
        Result.assert(js.typeOf(window.CSS.escape) == "function").log("CSS.escape"),
        Result.assert(js.typeOf(window.TextEncoder) == "function").log("TextEncoder"),
      )
    )
  }

  private def testAnimationFrames: Result = {
    DomTestKit.setUp("")
    val ran    = js.Array[Int]()
    val _      = dom.window.requestAnimationFrame(_ => { val _ = ran.push(1) })
    val before = ran.length
    val count  = DomTestKit.runAnimationFrames()
    Result.all(List(before ==== 0, count ==== 1, ran.length ==== 1))
  }

  private def testClipboard: Result = {
    DomTestKit.setUp("")
    val _ = window.navigator.clipboard.writeText("copied")
    DomTestKit.clipboardTexts ==== List("copied")
  }

  private def testClickBubbles: Result = {
    DomTestKit.setUp("""<div id="outer"><span id="inner"></span></div>""")
    val seen     = js.Array[String]()
    val listener = { (e: dom.MouseEvent) =>
      val _ = seen.push(e.target.asInstanceOf[dom.Element].id) // scalafix:ok DisableSyntax.asInstanceOf
    }: js.Function1[dom.MouseEvent, Unit]
    dom.document.addEventListener("click", listener)
    val _        = DomTestKit.click(DomTestKit.element("#inner"))
    dom.document.removeEventListener("click", listener)
    seen.toList ==== List("inner")
  }

  private def testListenerError: Result = {
    DomTestKit.setUp("""<div id="x"></div>""")
    val listener = { (_: dom.MouseEvent) =>
      val _ = js.Dynamic.global.window.missingFunction()
    }: js.Function1[dom.MouseEvent, Unit]
    dom.document.addEventListener("click", listener)
    val _        = DomTestKit.click(DomTestKit.element("#x"))
    dom.document.removeEventListener("click", listener)
    DomTestKit.listenerErrors.length ==== 1
  }
}
