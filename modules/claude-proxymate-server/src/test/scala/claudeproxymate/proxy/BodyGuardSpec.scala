package claudeproxymate.proxy

import cats.effect.{IO, Ref}
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import fs2.{text, Stream}
import org.http4s.{Response, Status}

import scala.concurrent.Future
import scala.concurrent.duration.*

class BodyGuardSpec extends munit.FunSuite {

  private given IORuntime = IORuntime.global

  private val StartTimeout: FiniteDuration = 200.millis
  private val AfterTimeout: FiniteDuration = 600.millis

  private val Chunks: List[String] = List("event: a\n\n", "event: b\n\n", "event: c\n\n")

  final private case class Counters(released: Ref[IO, Int], abandoned: Ref[IO, Int])

  final private case class Guarded(response: Response[IO], counters: Counters)

  final private case class SourceFailed() extends RuntimeException("the source body failed")

  private def chunkedBody: Stream[IO, Byte] =
    Stream.emits(Chunks).covary[IO].through(text.utf8.encode)

  private def guarded(body: Stream[IO, Byte]): IO[Guarded] =
    for {
      released  <- Ref.of[IO, Int](0)
      abandoned <- Ref.of[IO, Int](0)
      response  <- BodyGuard(
                     Forwarded(Response[IO](Status.Ok).withBodyStream(body), released.update(_ + 1)),
                     StartTimeout,
                     abandoned.update(_ + 1),
                   )
    } yield Guarded(response, Counters(released, abandoned))

  private def counts(counters: Counters): IO[(Int, Int)] =
    (counters.released.get, counters.abandoned.get).tupled

  private def run[A](io: IO[A]): Future[A] =
    io.timeout(10.seconds).unsafeToFuture()

  test("a body that is never run is released by the watchdog, which also runs onAbandoned") {
    run(for {
      g      <- guarded(chunkedBody)
      _      <- IO.sleep(AfterTimeout)
      result <- counts(g.counters)
    } yield {
      assertEquals(result, (1, 1))
    })
  }

  test("a body that is run at once is unchanged and released once, without onAbandoned") {
    run(for {
      g      <- guarded(chunkedBody)
      text   <- g.response.bodyText.compile.string
      _      <- IO.sleep(AfterTimeout)
      result <- counts(g.counters)
    } yield {
      assertEquals(text, Chunks.mkString)
      assertEquals(result, (1, 0))
    })
  }

  test("a body that is run after the start timeout fails with BodyStartedTooLate") {
    run(for {
      g      <- guarded(chunkedBody)
      _      <- IO.sleep(AfterTimeout)
      body   <- g.response.body.compile.drain.attempt
      result <- counts(g.counters)
    } yield {
      assertEquals(body, Left(BodyStartedTooLate(StartTimeout)))
      assertEquals(result, (1, 1))
    })
  }

  test("a body that fails is released once, without onAbandoned") {
    val failing = Stream.emit(Chunks.head).covary[IO].through(text.utf8.encode) ++ Stream.raiseError[IO](SourceFailed())
    run(for {
      g      <- guarded(failing)
      body   <- g.response.body.compile.drain.attempt
      _      <- IO.sleep(AfterTimeout)
      result <- counts(g.counters)
    } yield {
      assertEquals(body, Left(SourceFailed()))
      assertEquals(result, (1, 0))
    })
  }

  test("a body that is run two times fails the second time and is released once") {
    run(for {
      g      <- guarded(chunkedBody)
      first  <- g.response.body.compile.drain.attempt
      second <- g.response.body.compile.drain.attempt
      result <- counts(g.counters)
    } yield {
      assertEquals(first, Right(()))
      assertEquals(second, Left(BodyStartedTooLate(StartTimeout)))
      assertEquals(result, (1, 0))
    })
  }
}
