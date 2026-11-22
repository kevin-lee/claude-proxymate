package claudeproxymate.proxy

import cats.effect.{IO, Ref}
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*

import scala.concurrent.Future
import scala.concurrent.duration.*

class CaptureIdsSpec extends munit.FunSuite {

  private given IORuntime = IORuntime.global

  private def run[A](io: IO[A]): Future[A] =
    io.timeout(10.seconds).unsafeToFuture()

  test("a clock that does not move gives 100 different ids to 100 parallel callers") {
    run(for {
      nextId <- CaptureIds.make(IO.pure(1000L))
      ids    <- List.fill(100)(nextId).parSequence
    } yield {
      assertEquals(ids.map(_.value).sorted, (1000L to 1099L).toList)
    })
  }

  test("a clock that goes back still gives increasing ids") {
    run(for {
      clock <- Ref.of[IO, List[Long]](List(2000L, 1500L, 2500L))
      now = clock.modify(values => (values.drop(1), values.headOption.getOrElse(0L)))
      nextId <- CaptureIds.make(now)
      ids    <- List.fill(3)(nextId).sequence
    } yield {
      assertEquals(ids.map(_.value), List(2000L, 2001L, 2500L))
    })
  }
}
