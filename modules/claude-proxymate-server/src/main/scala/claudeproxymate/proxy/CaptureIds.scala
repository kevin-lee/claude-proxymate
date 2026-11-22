package claudeproxymate.proxy

import cats.effect.{IO, Ref}
import claudeproxymate.core.CaptureId

/** The source of the capture ids for one proxy process. */
object CaptureIds {

  /** The wall clock in milliseconds. */
  val realTimeMillis: IO[Long] = IO.realTime.map(_.toMillis)

  /** Make an id source for one proxy process.
    *
    * The source keeps the last id in a `Ref`, and `Ref.modify` applies
    * [[claudeproxymate.core.CaptureId.next]] atomically. So parallel requests
    * get different ids, also in the same millisecond. The id stays close to
    * `nowMillis`, so it does not repeat the ids of the captures from an earlier
    * proxy run that the renderer still shows.
    */
  def make(nowMillis: IO[Long]): IO[IO[CaptureId]] =
    Ref
      .of[IO, CaptureId](CaptureId.Start)
      .map(ref => nowMillis.flatMap(now => ref.modify(last => CaptureId.next(now).run(last).value)))
}
