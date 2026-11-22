package claudeproxymate.proxy

import cats.effect.{IO, Ref}
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import io.circe.Json
import io.circe.parser.{parse => parseJson}
import org.http4s.*
import org.http4s.client.Client
import claudeproxymate.core.{CaptureId, EpochMillis, ProxyError, ProxyEvent, ProxyRequest, ProxyResponse, SseParser}
import claudeproxymate.core.filter.RequestFilter

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*

/** HTTP proxy routes: intercept requests, apply the request filter, forward to the upstream, tee responses. */
object ProxyServer {

  /* The time limit for the start of a response body (see BodyGuard). Ember
   * pulls the body right after it writes the head of the response (a few
   * hundred bytes on a local socket), so 30 s is far more than needed, and a
   * transfer that nobody reads is released after at most 30 s. */
  val DefaultBodyStartTimeout: FiniteDuration = 30.seconds

  /** The proxy routes.
    *
    * @param nextId gives the capture id of each request (see [[CaptureIds.make]]).
    *               One source is shared by all requests of the proxy process.
    * @param nowMillis gives the wall clock in milliseconds for the capturedAt of each request.
    *               It is CaptureIds.realTimeMillis in production.
    * @param emit   receives the `RequestCaptured` and `ResponseCaptured` events.
    *               It is `EventEmitter.emit` in production.
    */
  def routes(
    client: Client[IO],
    upstream: Upstream,
    bodyStartTimeout: FiniteDuration,
    loader: FilterConfigLoader,
    nextId: IO[CaptureId],
    nowMillis: IO[Long],
    emit: ProxyEvent => IO[Unit],
  ): HttpApp[IO] = HttpApp[IO] { (req: Request[IO]) =>
    for {
      /* 1. Read request body */
      bodyBytes <- req.body.compile.to(Array)
      bodyJson = if (bodyBytes.isEmpty) none[Json]
                 else parseJson(new String(bodyBytes, StandardCharsets.UTF_8)).toOption

      /* 2. Apply the request filter (config re-read per request; a no-op keeps the original bytes) */
      cfg <- loader.load
      outcome   = bodyJson.map(RequestFilter(cfg, _))
      sendJson  = outcome.map(_.body).orElse(bodyJson)
      sendBytes =
        if (outcome.exists(_.report.isDefined)) sendJson.fold(bodyBytes)(_.noSpaces.getBytes(StandardCharsets.UTF_8))
        else bodyBytes

      /* 3. Create request event (what is actually forwarded, plus the filter report) */
      reqId      <- nextId
      capturedAt <- nowMillis.map(EpochMillis(_))
      proxyReq =
        ProxyRequest(reqId, capturedAt, req.method.name, req.uri.renderString, sendJson, outcome.flatMap(_.report))
      _ <- emit(ProxyEvent.RequestCaptured(proxyReq))

      /* 4. Forward to the upstream, 5. tee the response body to the client and to the capture */
      resp <- AnthropicForwarder.forward(client, upstream, req, sendBytes).attempt.flatMap {
                case Right(forwarded) =>
                  tee(reqId, forwarded.response, captured => emit(ProxyEvent.ResponseCaptured(captured)))
                    .flatMap { teed =>
                      BodyGuard(
                        Forwarded(teed, forwarded.release),
                        bodyStartTimeout,
                        emitAbandoned(emit, reqId, forwarded.response.status),
                      )
                    }
                case Left(err) =>
                  val pe     = ProxyError.upstream(err)
                  val errMsg = pe.message
                  emit(
                    ProxyEvent.ResponseCaptured(
                      ProxyResponse(reqId, ProxyErrorHttp4s.status(pe).code, errMsg.asLeft[Json], errMsg.some)
                    )
                  )
                    .as(ProxyErrorHttp4s.asResponse(pe))
              }
    } yield resp
  }

  /** Give the response to the client as the upstream sends it, and keep a copy
    * of the body. When the body stream ends, fails or is cancelled, give the
    * captured response to `onCaptured`. For a failure or a cancel, it has an
    * error text, and the body is the part that was received.
    */
  private[proxy] def tee(
    reqId: CaptureId,
    resp: Response[IO],
    onCaptured: ProxyResponse => IO[Unit],
  ): IO[Response[IO]] =
    for {
      received       <- Ref.of[IO, Chunk[Byte]](Chunk.empty[Byte])
      upstreamFailed <- Ref.of[IO, Option[Throwable]](none[Throwable])
    } yield {
      /* handleErrorWith on the upstream body sees only the errors of the
       * upstream body. An error of the server that consumes the stream (for
       * example a write to a closed client socket) does not go through it. */
      val body = resp
        .body
        .handleErrorWith(e => Stream.exec(upstreamFailed.set(e.some)) ++ Stream.raiseError[IO](e))
        .chunks
        .evalTap(chunk => received.update(_ ++ chunk))
        .unchunks
        .onFinalizeCase { exitCase =>
          (received.get, upstreamFailed.get).tupled.flatMap {
            case (bytes, upstreamError) =>
              onCaptured(
                ProxyResponse(
                  reqId,
                  resp.status.code,
                  parseBody(bytes.toArray),
                  streamError(exitCase, upstreamError),
                )
              )
          }
        }
      resp.withBodyStream(body)
    }

  /** The `ResponseCaptured` event for a response whose body the client never read. */
  private def emitAbandoned(emit: ProxyEvent => IO[Unit], reqId: CaptureId, status: Status): IO[Unit] =
    emit(
      ProxyEvent.ResponseCaptured(
        ProxyResponse(
          reqId,
          status.code,
          "".asLeft[Json],
          "the client did not read the response, so the upstream transfer was stopped".some,
        )
      )
    )

  private[proxy] def streamError(exitCase: Resource.ExitCase, upstreamError: Option[Throwable]): Option[String] =
    (exitCase, upstreamError) match {
      case (Resource.ExitCase.Succeeded, _) =>
        none[String]
      case (Resource.ExitCase.Errored(_), Some(e)) =>
        s"the upstream response stopped before its end: ${ProxyError.upstream(e).message}".some
      case (Resource.ExitCase.Errored(e), None) =>
        s"the connection to the client failed before the end of the response: ${ProxyError.upstream(e).message}".some
      case (Resource.ExitCase.Canceled, _) =>
        "the client closed the connection before the end of the response".some
    }

  /** A JSON body, else the merged SSE events, else the first 4000 characters as text. */
  private def parseBody(bytes: Array[Byte]): Either[String, Json] = {
    val respStr = new String(bytes, StandardCharsets.UTF_8)
    parseJson(respStr)
      .toOption
      .map(_.asRight[String])
      .orElse(SseParser.parseSseStream(respStr).map(_.asRight[String]))
      .getOrElse(respStr.take(4000).asLeft[Json])
  }
}
