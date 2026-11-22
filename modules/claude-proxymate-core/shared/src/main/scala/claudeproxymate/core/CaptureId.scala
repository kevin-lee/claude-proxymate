package claudeproxymate.core

import cats.data.State
import refined4s.Newtype
import refined4s.modules.circe.derivation.CirceNewtypeCodec

/** The id of one captured request and of its response.
  *
  * The renderer uses the id to put a response on the capture of its request,
  * so two requests must never get the same id. The proxy makes the ids with
  * [[CaptureId.next]]: an id is never less than the clock in milliseconds and
  * always greater than the last id. So the ids increase strictly, also when
  * many requests arrive in the same millisecond or the clock goes back.
  *
  * In JSON, the id is a number.
  */
type CaptureId = CaptureId.Type

object CaptureId extends Newtype[Long], CirceNewtypeCodec[Long] {

  /** The state before the first id. The first id is then the clock value. */
  val Start: CaptureId = CaptureId(0L)

  /** The next id after the last id (the state): `max(nowMillis, last + 1)`.
    *
    * The id is never less than the clock, and it is always greater than the
    * last id, also when the clock does not move or goes back. The new state is
    * the new id.
    */
  def next(nowMillis: Long): State[CaptureId, CaptureId] =
    State { last =>
      val id = CaptureId(math.max(nowMillis, last.value + 1L))
      (id, id)
    }
}
