package claudeproxymate.core

import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*

/* munit port of the shared hedgehog CaptureIdSpec for Scala Native, where the
 * hedgehog runner hangs. Every hedgehog example is ported 1:1, and every
 * hedgehog property is checked on fixed inputs.
 */
class CaptureIdSpec extends munit.FunSuite {

  /* The final state and the ids for the clock values, from the start state. */
  private def run(start: CaptureId, clocks: List[Long]): (CaptureId, List[CaptureId]) =
    clocks.traverse(CaptureId.next).run(start).value

  private def values(ids: List[CaptureId]): List[Long] = ids.map(_.value)

  private val Inputs: List[(String, CaptureId, List[Long])] = List(
    ("no clock values", CaptureId(1000L), Nil),
    ("a clock that does not move", CaptureId.Start, List(1000L, 1000L, 1000L, 1000L)),
    ("a clock that goes back", CaptureId.Start, List(2000L, 1500L, 1000L, 500L)),
    ("a mixed clock", CaptureId.Start, List(1000L, 1000L, 999L, 1003L, 1003L, 5000L, 4999L)),
    ("a start that is ahead of the clock", CaptureId(9000L), List(1000L, 8999L, 9000L, 9001L, 12000L)),
  )

  /* ── Properties on fixed inputs ───────────────────────────────────────── */

  Inputs.foreach {
    case (name, start, clocks) =>
      test(s"the ids increase strictly, and the first id is greater than the start ($name)") {
        val ids = values(run(start, clocks)._2)
        assert(ids.headOption.forall(_ > start.value), s"first id is not greater than start: $ids")
        assert(ids.zip(ids.drop(1)).forall { case (a, b) => a < b }, s"ids do not increase: $ids")
      }

      test(s"each id is not less than its clock value ($name)") {
        val ids = values(run(start, clocks)._2)
        assert(ids.zip(clocks).forall { case (id, clock) => id >= clock }, s"an id is less than its clock: $ids")
      }

      test(s"the ids are unique ($name)") {
        val ids = values(run(start, clocks)._2)
        assertEquals(ids.distinct.length, clocks.length, s"ids=$ids")
      }

      test(s"the final state is the last id, or the start when there are no clock values ($name)") {
        val (state, ids) = run(start, clocks)
        assertEquals(state.value, ids.lastOption.getOrElse(start).value)
      }
  }

  List(0L, 1L, 1_763_000_000_000L).foreach { n =>
    test(s"a CaptureId encodes as a JSON number and decodes back ($n)") {
      val json = CaptureId(n).asJson
      assertEquals(json, Json.fromLong(n))
      assertEquals(json.as[CaptureId].map(_.value), Right(n))
    }
  }

  /* ── Examples ─────────────────────────────────────────────────────────── */

  test("a clock that does not move gives 1000, 1001, 1002 from Start") {
    assertEquals(values(run(CaptureId.Start, List(1000L, 1000L, 1000L))._2), List(1000L, 1001L, 1002L))
  }

  test("a clock that goes back gives 2000, 2001, 2002 for 2000, 1500, 1000") {
    assertEquals(values(run(CaptureId.Start, List(2000L, 1500L, 1000L))._2), List(2000L, 2001L, 2002L))
  }

  test("a clock that jumps past the last id gives the clock value") {
    assertEquals(values(run(CaptureId(1004L), List(2000L))._2), List(2000L))
  }

  test("from Start, the first id is the clock value") {
    assertEquals(values(run(CaptureId.Start, List(1_763_000_000_000L))._2), List(1_763_000_000_000L))
  }
}
