package claudeproxymate.renderer.proxy

import claudeproxymate.renderer.facades.ElectronApi
import claudeproxymate.renderer.state.{AppState, ProxyCaptures}
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*

import scala.scalajs.js

/** [[ProxyControl.registerCaptureListeners]] in jsdom, with a fake Electron
  * API. The tests call the registered handlers the same way the preload
  * script does, and check [[AppState.proxyCaptures]].
  *
  * The handlers render the list and the detail later through `setTimeout`.
  * The page is empty, so those renders find no element and do nothing.
  */
object ProxyCaptureListenersHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("registerCaptureListeners registers 1 request handler and 1 response handler", testRegisters),
    example("a request from the proxy goes to the front of the captures", testRequestFront),
    example("the captures keep the newest 50 requests", testKeepNewest),
    example("a response from the proxy goes only to the capture with its id", testResponseMatch),
    example("a response for an id that is not in the captures changes nothing", testResponseUnknownId),
    example("a response without a numeric id changes nothing", testResponseNoNumericId),
    example("the selected capture shows the response that came after it was selected", testSelectedCaptureResponse),
    example("a response stays on its capture when newer requests arrive", testResponseKeptAfterRequests),
  )

  /** Stores the handlers that `registerCaptureListeners` registers. */
  final private class FakeApi {
    val requestHandlers: js.Array[js.Function1[js.Dynamic, Unit]]  = js.Array()
    val responseHandlers: js.Array[js.Function1[js.Dynamic, Unit]] = js.Array()

    private val onProxyRequest: js.Function1[js.Function1[js.Dynamic, Unit], Unit]  = { cb =>
      val _ = requestHandlers.push(cb)
    }
    private val onProxyResponse: js.Function1[js.Function1[js.Dynamic, Unit], Unit] = { cb =>
      val _ = responseHandlers.push(cb)
    }

    val api: ElectronApi =
      js.Dynamic
        .literal(onProxyRequest = onProxyRequest, onProxyResponse = onProxyResponse)
        .asInstanceOf[ElectronApi] // scalafix:ok DisableSyntax.asInstanceOf

    def sendRequest(data: js.Dynamic): Unit  = requestHandlers.foreach(handler => handler(data))
    def sendResponse(data: js.Dynamic): Unit = responseHandlers.foreach(handler => handler(data))
  }

  private def request(id: Double): js.Dynamic = js.Dynamic.literal(id = id, method = "POST", url = "/v1/messages")

  private def response(id: Double, status: Int): js.Dynamic = js.Dynamic.literal(id = id, status = status)

  /* Sets up an empty page and a fake API with the capture handlers. */
  private def registered(): FakeApi = {
    DomTestKit.setUp("")
    val fake = new FakeApi
    ProxyControl.registerCaptureListeners(fake.api)
    fake
  }

  private def captureIds: List[Double] = AppState.proxyCaptures.flatMap(ProxyCaptures.idOf)

  private def responseOf(capture: js.Dynamic): js.Dynamic = capture.selectDynamic("response")

  private def captureWithId(id: Double): Option[js.Dynamic] = ProxyCaptures.findById(AppState.proxyCaptures, Some(id))

  def testRegisters: Result = {
    val fake = registered()
    Result.all(
      List(
        fake.requestHandlers.length ==== 1,
        fake.responseHandlers.length ==== 1,
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testRequestFront: Result = {
    val fake = registered()
    fake.sendRequest(request(1.0))
    fake.sendRequest(request(2.0))
    Result.all(List(captureIds ==== List(2.0, 1.0), DomTestKit.listenerErrors ==== Nil))
  }

  def testKeepNewest: Result = {
    val fake = registered()
    (1 to 55).foreach(id => fake.sendRequest(request(id.toDouble)))
    Result.all(
      List(
        AppState.proxyCaptures.length ==== 50,
        captureIds ==== List.range(55, 5, -1).map(_.toDouble),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testResponseMatch: Result = {
    val fake = registered()
    fake.sendRequest(request(1.0))
    fake.sendRequest(request(2.0))
    fake.sendResponse(response(1.0, 201))
    Result.all(
      List(
        captureIds ==== List(2.0, 1.0),
        captureWithId(1.0).map(capture => responseOf(capture).selectDynamic("status").toString) ==== Some("201"),
        captureWithId(2.0).map(capture => js.isUndefined(responseOf(capture))) ==== Some(true),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testResponseUnknownId: Result = {
    val fake   = registered()
    fake.sendRequest(request(1.0))
    fake.sendRequest(request(2.0))
    val before = AppState.proxyCaptures
    fake.sendResponse(response(3.0, 200))
    Result.all(
      List(
        Result
          .assert(AppState.proxyCaptures.length == 2 && AppState.proxyCaptures.zip(before).forall {
            case (a, b) => a eq b
          })
          .log("the captures must be the same objects"),
        Result
          .assert(AppState.proxyCaptures.forall(capture => js.isUndefined(responseOf(capture))))
          .log("no capture may have a response"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testResponseNoNumericId: Result = {
    val fake   = registered()
    fake.sendRequest(request(1.0))
    val before = AppState.proxyCaptures
    fake.sendResponse(js.Dynamic.literal(id = "1", status = 200))
    fake.sendResponse(js.Dynamic.literal(status = 200))
    Result.all(
      List(
        Result
          .assert(AppState.proxyCaptures.length == 1 && AppState.proxyCaptures.headOption.exists(_ eq before.head))
          .log("the capture must be the same object"),
        Result
          .assert(js.isUndefined(responseOf(AppState.proxyCaptures.head)))
          .log("the capture may not have a response"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testSelectedCaptureResponse: Result = {
    val fake = registered()
    fake.sendRequest(request(1.0))
    fake.sendRequest(request(2.0))
    AppState.selectedProxyId = Some(1.0)
    fake.sendResponse(response(1.0, 201))
    Result.all(
      List(
        AppState.selectedCapture.flatMap(ProxyCaptures.idOf) ==== Some(1.0),
        AppState.selectedCapture.map(capture => responseOf(capture).selectDynamic("status").toString) ==== Some("201"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }

  def testResponseKeptAfterRequests: Result = {
    val fake = registered()
    fake.sendRequest(request(1.0))
    fake.sendResponse(response(1.0, 201))
    fake.sendRequest(request(2.0))
    fake.sendRequest(request(3.0))
    Result.all(
      List(
        captureIds ==== List(3.0, 2.0, 1.0),
        captureWithId(1.0).map(capture => responseOf(capture).selectDynamic("status").toString) ==== Some("201"),
        DomTestKit.listenerErrors ==== Nil,
      )
    )
  }
}
