package claudeproxymate.proxy

import cats.effect.{Deferred, IO, Ref}
import cats.effect.unsafe.IORuntime
import claudeproxymate.core.{CaptureId, ProxyResponse}
import fs2.{text, Stream}
import org.http4s.{Response, Status}

import scala.concurrent.Future
import scala.concurrent.duration.*

class ProxyServerTeeSpec extends munit.FunSuite {

  private given IORuntime = IORuntime.global

  private val Chunks: List[String] = List("event: a\n\n", "event: b\n\n")

  final private case class UpstreamFailed() extends RuntimeException("the upstream failed")

  final private case class Teed(response: Response[IO], captured: Ref[IO, List[ProxyResponse]])

  /* Each string is its own chunk. */
  private def chunks(parts: List[String]): Stream[IO, Byte] =
    Stream.emits(parts).covary[IO].flatMap(part => Stream.emit(part).through(text.utf8.encode))

  private def teed(body: Stream[IO, Byte]): IO[Teed] =
    for {
      captured <- Ref.of[IO, List[ProxyResponse]](Nil)
      response <- ProxyServer.tee(
                    CaptureId(1L),
                    Response[IO](Status.Ok).withBodyStream(body),
                    r => captured.update(_ :+ r),
                  )
    } yield Teed(response, captured)

  private def errors(t: Teed): IO[List[Option[String]]] =
    t.captured.get.map(_.map(_.error))

  private def run[A](io: IO[A]): Future[A] =
    io.timeout(10.seconds).unsafeToFuture()

  test("a body that is read to the end gives one captured response without an error") {
    run(for {
      t        <- teed(chunks(Chunks))
      received <- t.response.bodyText.compile.string
      captured <- t.captured.get
    } yield {
      assertEquals(received, Chunks.mkString)
      assertEquals(captured.map(_.error), List(None))
    })
  }

  test("an upstream body that fails gives the upstream error text") {
    run(for {
      t      <- teed(chunks(Chunks.take(1)) ++ Stream.raiseError[IO](UpstreamFailed()))
      result <- t.response.body.compile.drain.attempt
      errs   <- errors(t)
    } yield {
      assertEquals(result, Left(UpstreamFailed()))
      assertEquals(errs.size, 1)
      assert(
        errs.headOption.flatten.exists(_.startsWith("the upstream response stopped before its end:")),
        errs.toString,
      )
    })
  }

  test("a consumer that fails gives the client connection error text") {
    run(for {
      t      <- teed(chunks(Chunks))
      result <- t
                  .response
                  .body
                  .chunks
                  .evalMap(_ => IO.raiseError[Unit](new java.io.IOException("Broken pipe")))
                  .compile
                  .drain
                  .attempt
      errs   <- errors(t)
    } yield {
      assert(result.isLeft, result.toString)
      assertEquals(errs, List(Some("the connection to the client failed before the end of the response: Broken pipe")))
    })
  }

  test("a run that is cancelled gives the client closed error text") {
    run(for {
      t          <- teed(chunks(Chunks.take(1)) ++ Stream.never[IO])
      firstChunk <- Deferred[IO, Unit]
      fiber      <- t.response.body.chunks.evalTap(_ => firstChunk.complete(()).void).compile.drain.start
      _          <- firstChunk.get
      _          <- fiber.cancel
      errs       <- errors(t)
    } yield {
      assertEquals(errs, List(Some("the client closed the connection before the end of the response")))
    })
  }
}
