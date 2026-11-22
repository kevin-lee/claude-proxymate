package claudeproxymate.renderer.state

import hedgehog.*
import hedgehog.runner.*

import scala.scalajs.js

/** The pure functions of [[ProxyCaptures]]: adding requests, attaching
  * responses and finding a capture by its id.
  */
object ProxyCapturesSpec extends Properties {

  override def tests: List[Test] = List(
    property("addRequest puts the request at the front", testAddRequestFront),
    property("addRequest keeps at most MaxCaptures captures, the newest first", testAddRequestLimit),
    property("addResponse attaches the response only to the capture with its id", testAddResponseMatch),
    property("addResponse for an id that is not in the list changes nothing", testAddResponseUnknownId),
    example("addResponse for a response without a numeric id changes nothing", testAddResponseNoNumericId),
    property("a second addResponse for the same id replaces the first response", testAddResponseTwice),
    example("idOf reads a numeric id", testIdOf),
    example("findById with no id is None", testFindByIdNone),
    property("findById finds the capture with the id", testFindById),
    example("findById with an id that is not in the list is None", testFindByIdUnknown),
  )

  private def captureWithId(id: Double): js.Dynamic =
    js.Dynamic.literal(id = id, method = "POST", url = "/v1/messages")

  /* The ids are 1 to n. */
  private def capturesOf(n: Int): List[js.Dynamic] = List.tabulate(n)(i => captureWithId((i + 1).toDouble))

  private def responseFor(id: Double): js.Dynamic = js.Dynamic.literal(id = id, status = 200)

  private def sameObjects(a: List[js.Dynamic], b: List[js.Dynamic]): Boolean =
    a.length == b.length && a.zip(b).forall { case (x, y) => x eq y }

  private def noResponse(captures: List[js.Dynamic]): Boolean =
    captures.forall(capture => js.isUndefined(capture.selectDynamic("response")))

  def testAddRequestFront: Property =
    for {
      n <- Gen.int(Range.linear(0, 60)).forAll
    } yield {
      val request = captureWithId(1000.0)
      val result  = ProxyCaptures.addRequest(capturesOf(n), request)
      Result.assert(result.headOption.exists(_ eq request)).log(s"n=$n")
    }

  def testAddRequestLimit: Property =
    for {
      n <- Gen.int(Range.linear(0, 60)).forAll
    } yield {
      val captures = capturesOf(n)
      val result   = ProxyCaptures.addRequest(captures, captureWithId(1000.0))
      Result.all(
        List(
          result.length ==== math.min(n + 1, ProxyCaptures.MaxCaptures),
          Result
            .assert(sameObjects(result.drop(1), captures.take(ProxyCaptures.MaxCaptures - 1)))
            .log("the older captures must follow the new request in the same order"),
        )
      )
    }

  def testAddResponseMatch: Property =
    for {
      n     <- Gen.int(Range.linear(1, 60)).forAll
      index <- Gen.int(Range.linear(0, n - 1)).forAll
    } yield {
      val id       = (index + 1).toDouble
      val captures = capturesOf(n)
      val response = responseFor(id)
      val result   = ProxyCaptures.addResponse(captures, response)
      Result.all(
        List(
          result.length ==== n,
          Result.assert(result(index).selectDynamic("response") eq response).log("the response must be attached"),
          ProxyCaptures.idOf(result(index)) ==== Some(id),
          Result
            .assert(sameObjects(result.patch(index, Nil, 1), captures.patch(index, Nil, 1)))
            .log("the other captures must stay the same objects"),
          Result
            .assert(js.isUndefined(captures(index).selectDynamic("response")))
            .log("the original capture must not change"),
        )
      )
    }

  def testAddResponseUnknownId: Property =
    for {
      n <- Gen.int(Range.linear(0, 60)).forAll
    } yield {
      val captures = capturesOf(n)
      val result   = ProxyCaptures.addResponse(captures, responseFor(1000.0))
      Result.all(
        List(
          Result.assert(sameObjects(result, captures)).log("the list must have the same objects"),
          Result.assert(noResponse(result)).log("no capture may have a response"),
        )
      )
    }

  def testAddResponseNoNumericId: Result = {
    val captures  = capturesOf(3)
    val responses = List(js.Dynamic.literal(status = 200), js.Dynamic.literal(id = "1", status = 200))
    Result.all(
      responses.map { response =>
        val result = ProxyCaptures.addResponse(captures, response)
        Result
          .assert(sameObjects(result, captures) && noResponse(result))
          .log(s"response=${js.JSON.stringify(response)}")
      }
    )
  }

  def testAddResponseTwice: Property =
    for {
      n     <- Gen.int(Range.linear(1, 60)).forAll
      index <- Gen.int(Range.linear(0, n - 1)).forAll
    } yield {
      val id     = (index + 1).toDouble
      val first  = responseFor(id)
      val second = js.Dynamic.literal(id = id, status = 500)
      val result = ProxyCaptures.addResponse(ProxyCaptures.addResponse(capturesOf(n), first), second)
      Result.all(
        List(
          result.length ==== n,
          Result.assert(result(index).selectDynamic("response") eq second).log("the second response must win"),
        )
      )
    }

  def testIdOf: Result =
    Result.all(
      List(
        ProxyCaptures.idOf(captureWithId(7.0)) ==== Some(7.0),
        ProxyCaptures.idOf(js.Dynamic.literal()) ==== None,
        ProxyCaptures.idOf(js.Dynamic.literal(id = "7")) ==== None,
      )
    )

  def testFindByIdNone: Result =
    Result.assert(ProxyCaptures.findById(capturesOf(3), None).isEmpty)

  def testFindById: Property =
    for {
      n     <- Gen.int(Range.linear(1, 60)).forAll
      index <- Gen.int(Range.linear(0, n - 1)).forAll
    } yield {
      val captures = capturesOf(n)
      Result
        .assert(ProxyCaptures.findById(captures, Some((index + 1).toDouble)).exists(_ eq captures(index)))
        .log(s"n=$n, index=$index")
    }

  def testFindByIdUnknown: Result =
    Result.assert(ProxyCaptures.findById(capturesOf(3), Some(1000.0)).isEmpty)
}
