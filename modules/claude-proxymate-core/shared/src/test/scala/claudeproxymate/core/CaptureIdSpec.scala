package claudeproxymate.core

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import io.circe.Json
import io.circe.syntax.*

object CaptureIdSpec extends Properties {

  override def tests: List[Test] = List(
    property("the ids increase strictly, and the first id is greater than the start", testIdsIncreaseStrictly),
    property("each id is not less than its clock value", testIdNotLessThanClock),
    property("the ids are unique", testIdsUnique),
    property("the final state is the last id, or the start when there are no clock values", testFinalStateIsLastId),
    property("a CaptureId encodes as a JSON number and decodes back", testCodecRoundTrip),
    example("a clock that does not move gives 1000, 1001, 1002 from Start", testClockDoesNotMove),
    example("a clock that goes back gives 2000, 2001, 2002 for 2000, 1500, 1000", testClockGoesBack),
    example("a clock that jumps past the last id gives the clock value", testClockJumpsPastLastId),
    example("from Start, the first id is the clock value", testFirstIdFromStartIsClock),
  )

  /* Clock values that move by -5 to 5 ms at each step, so the clock often
   * stays the same and often goes back. */
  private def genClocks: Gen[List[Long]] =
    for {
      base   <- Gen.long(Range.linear(1_000L, 2_000_000_000_000L))
      deltas <- Gen.long(Range.linear(-5L, 5L)).list(Range.linear(0, 100))
    } yield deltas.scanLeft(base)(_ + _).drop(1)

  private def genStart: Gen[CaptureId] =
    Gen.long(Range.linear(0L, 2_000_000_000_000L)).map(CaptureId(_))

  /* The final state and the ids for the clock values, from the start state. */
  private def run(start: CaptureId, clocks: List[Long]): (CaptureId, List[CaptureId]) =
    clocks.traverse(CaptureId.next).run(start).value

  private def values(ids: List[CaptureId]): List[Long] = ids.map(_.value)

  def testIdsIncreaseStrictly: Property =
    for {
      start  <- genStart.log("start")
      clocks <- genClocks.log("clocks")
    } yield {
      val ids = values(run(start, clocks)._2)
      Result.all(
        List(
          Result.assert(ids.headOption.forall(_ > start.value)).log(s"first id is not greater than start: $ids"),
          Result.assert(ids.zip(ids.drop(1)).forall { case (a, b) => a < b }).log(s"ids do not increase: $ids"),
        )
      )
    }

  def testIdNotLessThanClock: Property =
    for {
      start  <- genStart.log("start")
      clocks <- genClocks.log("clocks")
    } yield {
      val ids = values(run(start, clocks)._2)
      Result
        .assert(ids.zip(clocks).forall { case (id, clock) => id >= clock })
        .log(s"an id is less than its clock value: ids=$ids")
    }

  def testIdsUnique: Property =
    for {
      start  <- genStart.log("start")
      clocks <- genClocks.log("clocks")
    } yield {
      val ids = values(run(start, clocks)._2)
      (ids.distinct.length ==== clocks.length).log(s"ids=$ids")
    }

  def testFinalStateIsLastId: Property =
    for {
      start  <- genStart.log("start")
      clocks <- genClocks.log("clocks")
    } yield {
      val (state, ids) = run(start, clocks)
      state.value ==== ids.lastOption.getOrElse(start).value
    }

  def testCodecRoundTrip: Property =
    for {
      n <- Gen.long(Range.linear(0L, 2_000_000_000_000L)).log("n")
    } yield {
      val json = CaptureId(n).asJson
      Result.all(
        List(
          json ==== Json.fromLong(n),
          json.as[CaptureId].map(_.value) ==== Right(n),
        )
      )
    }

  def testClockDoesNotMove: Result =
    values(run(CaptureId.Start, List(1000L, 1000L, 1000L))._2) ==== List(1000L, 1001L, 1002L)

  def testClockGoesBack: Result =
    values(run(CaptureId.Start, List(2000L, 1500L, 1000L))._2) ==== List(2000L, 2001L, 2002L)

  def testClockJumpsPastLastId: Result =
    values(run(CaptureId(1004L), List(2000L))._2) ==== List(2000L)

  def testFirstIdFromStartIsClock: Result =
    values(run(CaptureId.Start, List(1_763_000_000_000L))._2) ==== List(1_763_000_000_000L)
}
