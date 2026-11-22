package claudeproxymate.proxy

import cats.effect.{IO, Ref}
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import claudeproxymate.core.{EpochMillis, ProxyEvent}
import org.http4s.*
import org.http4s.client.Client

import scala.concurrent.Future
import scala.concurrent.duration.*

/* The capture ids of the proxy routes (issue #99). The upstream is an echo
 * HttpApp behind Client.fromHttpApp, so the test opens no sockets and does not
 * use libcurl. The clock does not move, so all requests get the same
 * millisecond, as parallel requests from Claude Code can. */
class ProxyServerCaptureIdSpec extends munit.FunSuite {

  private given IORuntime = IORuntime.global

  private val Count: Int = 50

  private val FrozenClockMillis: Long = 1000L

  private val CapturedAtMillis: Long = 1794741364123L

  private val echo: HttpApp[IO] =
    HttpApp[IO](req => IO.pure(Response[IO](Status.Ok).withBodyStream(req.body)))

  private def run[A](io: IO[A]): Future[A] =
    io.timeout(20.seconds).unsafeToFuture()

  private def post(i: Int): Request[IO] =
    Request[IO](Method.POST, Uri.unsafeFromString("/v1/messages")).withEntity(s"""{"n":$i}""")

  test(
    "parallel requests in the same millisecond get different capture ids, and each captured response has the id of its request"
  ) {
    run(for {
      events <- Ref.of[IO, List[ProxyEvent]](Nil)
      loader <- FilterConfigLoader.make(none[String])
      nextId <- CaptureIds.make(IO.pure(FrozenClockMillis))
      app = ProxyServer.routes(
              Client.fromHttpApp(echo),
              Upstream.Anthropic,
              ProxyServer.DefaultBodyStartTimeout,
              loader,
              nextId,
              IO.pure(FrozenClockMillis),
              event => events.update(_ :+ event),
            )
      /* Read each body to the end, so the tee gives its ResponseCaptured. */
      _   <- (1 to Count).toList.parTraverse(i => app.run(post(i)).flatMap(_.bodyText.compile.string))
      all <- events.get
    } yield {
      val requests  = all.collect { case ProxyEvent.RequestCaptured(r) => r }
      val responses = all.collect { case ProxyEvent.ResponseCaptured(r) => r }
      assertEquals(
        requests.map(_.id.value).sorted,
        (FrozenClockMillis until FrozenClockMillis + Count).toList,
      )
      assertEquals(responses.size, Count)
      /* The echo upstream returns the request body. So when each id has the
       * same body in its request and in its response, each response has the id
       * of its own request. */
      assertEquals(
        responses.map(r => r.id -> r.body.toOption).toMap,
        requests.map(r => r.id -> r.body).toMap,
      )
    })
  }

  test("each captured request has the capture time from the clock as epoch milliseconds") {
    run(for {
      events <- Ref.of[IO, List[ProxyEvent]](Nil)
      loader <- FilterConfigLoader.make(none[String])
      nextId <- CaptureIds.make(IO.pure(FrozenClockMillis))
      app = ProxyServer.routes(
              Client.fromHttpApp(echo),
              Upstream.Anthropic,
              ProxyServer.DefaultBodyStartTimeout,
              loader,
              nextId,
              IO.pure(CapturedAtMillis),
              event => events.update(_ :+ event),
            )
      /* Read the body to the end, so the tee gives its ResponseCaptured. */
      _   <- app.run(post(1)).flatMap(_.bodyText.compile.string)
      all <- events.get
    } yield {
      val requests = all.collect { case ProxyEvent.RequestCaptured(r) => r }
      assertEquals(requests.map(_.capturedAt), List(EpochMillis(CapturedAtMillis)))
    })
  }
}
