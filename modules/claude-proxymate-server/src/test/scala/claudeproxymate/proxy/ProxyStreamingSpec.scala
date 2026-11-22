package claudeproxymate.proxy

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import claudeproxymate.core.{ProxyError, ProxyEvent, ProxyRequest, ProxyResponse}
import com.comcast.ip4s.*
import fs2.{text, Stream}
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.`Content-Type`
import org.http4s.implicits.*
import org.typelevel.ci.*

import scala.concurrent.Future
import scala.concurrent.duration.*

/* Integration tests for the streaming libcurl client (issue #100). The proxy
 * routes run in-process with CurlHttpClient, and the upstream is a local
 * Ember server on plain HTTP, so no network and no TLS are needed. */
class ProxyStreamingSpec extends munit.FunSuite {

  private given IORuntime = IORuntime.global

  private val Settings: CurlHttpClient.Settings = CurlHttpClient.Settings.Default

  private val FirstEvent: String  = "event: content_block_delta\ndata: {\"n\":1}\n\n"
  private val SecondEvent: String = "event: message_stop\ndata: {\"n\":2}\n\n"

  private val EventStream: `Content-Type` = `Content-Type`(MediaType.`text/event-stream`)

  final private case class Gates(release: Deferred[IO, Unit], upstreamClosed: Deferred[IO, Unit])

  private def gates: IO[Gates] =
    (Deferred[IO, Unit], Deferred[IO, Unit]).mapN(Gates.apply)

  private def upstreamApp(g: Gates): HttpApp[IO] =
    HttpRoutes
      .of[IO] {
        case POST -> Root / "v1" / "gated" =>
          /* The second event waits for the test. A proxy that buffers the
           * whole body never gives the first event, and the test times out. */
          IO.pure(
            Response[IO](Status.Ok)
              .withContentType(EventStream)
              .withBodyStream(
                (Stream.emit(FirstEvent) ++ Stream.exec(g.release.get) ++ Stream.emit(SecondEvent))
                  .through(text.utf8.encode)
              )
          )
        case POST -> Root / "v1" / "pings" =>
          /* Ping comments after the first event, as the Anthropic API sends.
           * Ember sees a closed connection only when it writes, so the
           * upstream must keep writing for upstreamClosed to complete. */
          IO.pure(
            Response[IO](Status.Ok)
              .withContentType(EventStream)
              .withBodyStream(
                (Stream.emit(FirstEvent) ++ Stream.awakeEvery[IO](200.millis).as(": ping\n\n"))
                  .through(text.utf8.encode)
                  .onFinalize(g.upstreamClosed.complete(()).void)
              )
          )
        case POST -> Root / "v1" / "silent" =>
          IO.pure(
            Response[IO](Status.Ok)
              .withContentType(EventStream)
              .withBodyStream(
                Stream.emit(FirstEvent).through(text.utf8.encode) ++ Stream.never[IO]
              )
          )
        case req @ POST -> Root / "v1" / "echo" =>
          IO.pure(
            Response[IO](Status.Ok)
              .putHeaders(Header.Raw(ci"x-method", req.method.name))
              .withBodyStream(req.body)
          )
        case HEAD -> Root / "api" / "hello" =>
          /* Content-Length 5 and no body, as a HEAD response may have. A client
           * without CURLOPT_NOBODY waits for the 5 bytes. */
          IO.pure(
            Response[IO](Status.Ok)
              .putHeaders(Header.Raw(ci"x-test", "v"), headers.`Content-Length`.unsafeFromLong(5L))
          )
        case GET -> Root / "v1" / "overloaded" =>
          IO.pure(
            Response[IO](Status.fromInt(529).getOrElse(Status.ServiceUnavailable))
              .putHeaders(Header.Raw(ci"x-test", "v"))
              .withContentType(`Content-Type`(MediaType.application.json))
              .withEntity("""{"type":"error","error":{"type":"overloaded_error"}}""")
          )
      }
      .orNotFound

  private def upstreamServer(g: Gates): Resource[IO, Upstream] =
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"127.0.0.1")
      .withPort(port"0")
      .withHttpApp(upstreamApp(g))
      .withShutdownTimeout(1.second)
      .build
      .map(server => Upstream(Uri.Scheme.http, host"127.0.0.1", server.address.port))

  /* An emit that drops the events, for the tests that do not check them. */
  private val ignoreEvents: ProxyEvent => IO[Unit] = _ => IO.unit

  private def proxyApp(
    settings: CurlHttpClient.Settings,
    bodyStartTimeout: FiniteDuration,
    upstream: Upstream,
    emit: ProxyEvent => IO[Unit],
  ): IO[HttpApp[IO]] =
    CurlHttpClient.globalInit *>
      (FilterConfigLoader.make(none[String]), CaptureIds.make(CaptureIds.realTimeMillis)).mapN { (loader, nextId) =>
        ProxyServer.routes(CurlHttpClient.client(settings), upstream, bodyStartTimeout, loader, nextId, emit)
      }

  private def run[A](io: IO[A]): Future[A] =
    io.timeout(20.seconds).unsafeToFuture()

  private def withProxy[A](
    settings: CurlHttpClient.Settings,
    bodyStartTimeout: FiniteDuration,
    emit: ProxyEvent => IO[Unit],
  )(
    f: (HttpApp[IO], Gates) => IO[A]
  ): IO[A] =
    gates.flatMap { g =>
      upstreamServer(g).use(upstream => proxyApp(settings, bodyStartTimeout, upstream, emit).flatMap(app => f(app, g)))
    }

  /* The events that the proxy emits, in the order of emit. */
  private def recorder: IO[(Ref[IO, List[ProxyEvent]], ProxyEvent => IO[Unit])] =
    Ref.of[IO, List[ProxyEvent]](Nil).map(ref => (ref, event => ref.update(_ :+ event)))

  private def requestsOf(events: List[ProxyEvent]): List[ProxyRequest] =
    events.collect { case ProxyEvent.RequestCaptured(r) => r }

  private def responsesOf(events: List[ProxyEvent]): List[ProxyResponse] =
    events.collect { case ProxyEvent.ResponseCaptured(r) => r }

  private def post(path: String, body: String): Request[IO] =
    Request[IO](Method.POST, Uri.unsafeFromString(path)).withEntity(body)

  test("the first event reaches the client before the upstream ends the response") {
    run(withProxy(Settings, ProxyServer.DefaultBodyStartTimeout, ignoreEvents) { (app, g) =>
      for {
        resp <- app.run(post("/v1/gated", "{}"))
        all  <- resp
                  .body
                  .through(text.utf8.decode)
                  .evalScan("") { (received, s) =>
                    val next = received + s
                    g.release.complete(()).whenA(next.contains(FirstEvent)).as(next)
                  }
                  .compile
                  .lastOrError
                  .timeout(10.seconds)
      } yield {
        assertEquals(resp.status, Status.Ok)
        assertEquals(resp.headers.get[`Content-Type`].map(_.mediaType), Some(MediaType.`text/event-stream`))
        assertEquals(all, FirstEvent + SecondEvent)
      }
    })
  }

  test("a HEAD request gives the status and the headers, and an empty body that ends without an error") {
    run(withProxy(Settings, ProxyServer.DefaultBodyStartTimeout, ignoreEvents) { (app, _) =>
      for {
        resp <- app.run(Request[IO](Method.HEAD, uri"/api/hello"))
        body <- resp.body.compile.to(Array).attempt.timeout(10.seconds)
      } yield {
        assertEquals(resp.status, Status.Ok)
        assertEquals(resp.headers.get(ci"x-test").map(_.head.value), Some("v"))
        assertEquals(body.map(_.length), Right(0))
      }
    })
  }

  test("status, headers and body of an error response are forwarded") {
    run(withProxy(Settings, ProxyServer.DefaultBodyStartTimeout, ignoreEvents) { (app, _) =>
      for {
        resp <- app.run(Request[IO](Method.GET, uri"/v1/overloaded"))
        body <- resp.bodyText.compile.string
      } yield {
        assertEquals(resp.status.code, 529)
        assertEquals(resp.headers.get(ci"x-test").map(_.head.value), Some("v"))
        assertEquals(resp.headers.get(ci"Transfer-Encoding"), None)
        assertEquals(body, """{"type":"error","error":{"type":"overloaded_error"}}""")
      }
    })
  }

  test("a large request body and a large response body pass through the pipe unchanged") {
    /* 2 MiB: much larger than the pipe buffer (16 KiB to 64 KiB), so libcurl's
     * fwrite must wait for the reader many times. */
    val body = (0 until 2 * 1024 * 1024).map(i => ('a' + i % 26).toChar).mkString
    run(withProxy(Settings, ProxyServer.DefaultBodyStartTimeout, ignoreEvents) { (app, _) =>
      for {
        resp     <- app.run(post("/v1/echo", body))
        received <- resp.bodyText.compile.string
      } yield {
        assertEquals(resp.headers.get(ci"x-method").map(_.head.value), Some("POST"))
        assertEquals(received.length, body.length)
        assert(received === body)
      }
    })
  }

  test("when the client stops reading, the transfer stops and the upstream connection closes") {
    run(withProxy(Settings, ProxyServer.DefaultBodyStartTimeout, ignoreEvents) { (app, g) =>
      for {
        resp  <- app.run(post("/v1/pings", "{}"))
        first <- resp
                   .body
                   .through(text.utf8.decode)
                   .scan("")(_ + _)
                   .find(_.contains(FirstEvent))
                   .compile
                   .lastOrError
                   .timeout(10.seconds)
        _     <- g.upstreamClosed.get.timeout(10.seconds)
      } yield {
        assertEquals(first, FirstEvent)
      }
    })
  }

  test("a response whose body is never read is released, and the upstream connection closes") {
    run(withProxy(Settings, 500.millis, ignoreEvents) { (app, g) =>
      for {
        resp <- app.run(post("/v1/pings", "{}"))
        _    <- g.upstreamClosed.get.timeout(10.seconds)
      } yield {
        assertEquals(resp.status, Status.Ok)
      }
    })
  }

  test("an upstream that sends nothing for the idle timeout fails the body stream") {
    val settings = Settings.copy(idleTimeout = 2.seconds)
    run(withProxy(settings, ProxyServer.DefaultBodyStartTimeout, ignoreEvents) { (app, _) =>
      for {
        resp   <- app.run(post("/v1/silent", "{}"))
        result <- resp.body.compile.drain.attempt.timeout(15.seconds)
      } yield {
        assertEquals(resp.status, Status.Ok)
        assertEquals(result, Left(ProxyErrorException(ProxyError.CurlPerformFailed(28))))
      }
    })
  }

  test("an upstream that is not reachable gives 502 Bad Gateway") {
    /* Bind a port, then close the server, so nothing listens on it. */
    val closedPort = gates.flatMap(g => upstreamServer(g).use(upstream => IO.pure(upstream.port)))
    run(closedPort.flatMap { p =>
      proxyApp(
        Settings,
        ProxyServer.DefaultBodyStartTimeout,
        Upstream(Uri.Scheme.http, host"127.0.0.1", p),
        ignoreEvents,
      ).flatMap { app =>
        app.run(post("/v1/echo", "{}")).flatMap { resp =>
          resp.bodyText.compile.string.map { body =>
            assertEquals(resp.status, Status.BadGateway)
            assert(body.contains("curl_easy_perform failed with code 7"), body)
          }
        }
      }
    })
  }

  /* ── Capture ids (issue #99) ──────────────────────────────────────────── */

  /* The test for parallel requests is in ProxyServerCaptureIdSpec. It needs no
   * sockets: fs2 on Scala Native listens with a backlog of 0, and on Linux many
   * connections that arrive at the same time wait for SYN retries. */

  test("an upstream that is not reachable gives a captured response with the id of its request") {
    val closedPort = gates.flatMap(g => upstreamServer(g).use(upstream => IO.pure(upstream.port)))
    run(for {
      p              <- closedPort
      (events, emit) <- recorder
      app            <- proxyApp(
                          Settings,
                          ProxyServer.DefaultBodyStartTimeout,
                          Upstream(Uri.Scheme.http, host"127.0.0.1", p),
                          emit,
                        )
      resp           <- app.run(post("/v1/echo", "{}"))
      /* CurlHttpClient gives the connect failure as a 502 response, so the tee
       * gives its ResponseCaptured when the body is read to the end. */
      _              <- resp.bodyText.compile.string
      all            <- events.get
    } yield {
      val requests  = requestsOf(all)
      val responses = responsesOf(all)
      assertEquals(resp.status, Status.BadGateway)
      assertEquals(requests.size, 1)
      assertEquals(responses.map(r => (r.id, r.status)), requests.map(r => (r.id, Status.BadGateway.code)))
    })
  }

  test("a response whose body is never read gives a captured response with the id of its request") {
    run(recorder.flatMap {
      case (events, emit) =>
        withProxy(Settings, 500.millis, emit) { (app, g) =>
          for {
            resp      <- app.run(post("/v1/pings", "{}"))
            _         <- g.upstreamClosed.get.timeout(10.seconds)
            /* The abandoned capture is emitted when the body guard releases the
             * transfer. Wait until it is there. */
            responses <- events.get.map(responsesOf).iterateUntil(_.nonEmpty).timeout(10.seconds)
            all       <- events.get
          } yield {
            val requests = requestsOf(all)
            assertEquals(resp.status, Status.Ok)
            assertEquals(requests.size, 1)
            assertEquals(responses.map(_.id), requests.map(_.id))
            assert(responses.forall(_.error.isDefined), s"responses=$responses")
          }
        }
    })
  }
}
