package claudeproxymate.proxy

import cats.effect.{Deferred, IO, Resource}
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import claudeproxymate.core.ProxyError
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
      .map(server => Upstream(Uri.Scheme.http, "127.0.0.1", server.address.port.value))

  private def proxyApp(settings: CurlHttpClient.Settings, upstream: Upstream): IO[HttpApp[IO]] =
    CurlHttpClient.globalInit *>
      FilterConfigLoader.make(none[String]).map(ProxyServer.routes(CurlHttpClient.client(settings), upstream, _))

  private def run[A](io: IO[A]): Future[A] =
    io.timeout(20.seconds).unsafeToFuture()

  private def withProxy[A](settings: CurlHttpClient.Settings)(f: (HttpApp[IO], Gates) => IO[A]): IO[A] =
    gates.flatMap { g =>
      upstreamServer(g).use(upstream => proxyApp(settings, upstream).flatMap(app => f(app, g)))
    }

  private def post(path: String, body: String): Request[IO] =
    Request[IO](Method.POST, Uri.unsafeFromString(path)).withEntity(body)

  test("the first event reaches the client before the upstream ends the response") {
    run(withProxy(Settings) { (app, g) =>
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

  test("status, headers and body of an error response are forwarded") {
    run(withProxy(Settings) { (app, _) =>
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
    run(withProxy(Settings) { (app, _) =>
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
    run(withProxy(Settings) { (app, g) =>
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

  test("an upstream that sends nothing for the idle timeout fails the body stream") {
    val settings = Settings.copy(idleTimeout = 2.seconds)
    run(withProxy(settings) { (app, _) =>
      for {
        resp   <- app.run(post("/v1/silent", "{}"))
        result <- resp.body.compile.drain.attempt.timeout(15.seconds)
      } yield {
        assertEquals(resp.status, Status.Ok)
        result match {
          case Left(ProxyErrorException(ProxyError.CurlPerformFailed(code))) =>
            assertEquals(code, 28)
          case other =>
            fail(s"expected CurlPerformFailed(28), got $other")
        }
      }
    })
  }

  test("an upstream that is not reachable gives 502 Bad Gateway") {
    /* Bind a port, then close the server, so nothing listens on it. */
    val closedPort = gates.flatMap(g => upstreamServer(g).use(upstream => IO.pure(upstream.port)))
    run(closedPort.flatMap { p =>
      proxyApp(Settings, Upstream(Uri.Scheme.http, "127.0.0.1", p)).flatMap { app =>
        app.run(post("/v1/echo", "{}")).flatMap { resp =>
          resp.bodyText.compile.string.map { body =>
            assertEquals(resp.status, Status.BadGateway)
            assert(body.contains("curl_easy_perform failed with code 7"), body)
          }
        }
      }
    })
  }
}
