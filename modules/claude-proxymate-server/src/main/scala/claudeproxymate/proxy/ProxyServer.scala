package claudeproxymate.proxy

import cats.effect.{IO, Ref}
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.Chunk
import io.circe.Json
import io.circe.parser.{parse => parseJson}
import org.http4s.*
import org.http4s.client.Client
import claudeproxymate.core.{ProxyError, ProxyEvent, ProxyRequest, ProxyResponse, SseParser}
import claudeproxymate.core.filter.RequestFilter

import java.nio.charset.StandardCharsets
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.*

/** HTTP proxy routes: intercept requests, apply the request filter, forward to the upstream, tee responses. */
object ProxyServer {

  private val timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")

  /* The time limit for the start of a response body (see BodyGuard). Ember
   * pulls the body right after it writes the head of the response (a few
   * hundred bytes on a local socket), so 30 s is far more than needed, and a
   * transfer that nobody reads is released after at most 30 s. */
  val DefaultBodyStartTimeout: FiniteDuration = 30.seconds

  def routes(
    client: Client[IO],
    upstream: Upstream,
    bodyStartTimeout: FiniteDuration,
    loader: FilterConfigLoader,
  ): HttpApp[IO] = HttpApp[IO] { (req: Request[IO]) =>
    for {
      // 1. Read request body
      bodyBytes <- req.body.compile.to(Array)
      bodyJson = if (bodyBytes.isEmpty) none[Json]
                 else parseJson(new String(bodyBytes, StandardCharsets.UTF_8)).toOption

      // 2. Apply the request filter (config re-read per request; a no-op keeps the original bytes)
      cfg <- loader.load
      outcome   = bodyJson.map(RequestFilter(cfg, _))
      sendJson  = outcome.map(_.body).orElse(bodyJson)
      sendBytes =
        if (outcome.exists(_.report.isDefined)) sendJson.fold(bodyBytes)(_.noSpaces.getBytes(StandardCharsets.UTF_8))
        else bodyBytes

      // 3. Create request event (what is actually forwarded, plus the filter report)
      reqId    = System.currentTimeMillis()
      ts       = LocalTime.now().format(timeFormatter)
      proxyReq = ProxyRequest(reqId, ts, req.method.name, req.uri.renderString, sendJson, outcome.flatMap(_.report))
      _ <- EventEmitter.emit(ProxyEvent.RequestCaptured(proxyReq))

      // 4. Forward to the upstream, 5. tee the response body to the client and to the capture
      resp <- AnthropicForwarder.forward(client, upstream, req, sendBytes).attempt.flatMap {
                case Right(forwarded) =>
                  tee(reqId, forwarded.response).flatMap { teed =>
                    BodyGuard(
                      Forwarded(teed, forwarded.release),
                      bodyStartTimeout,
                      emitAbandoned(reqId, forwarded.response.status),
                    )
                  }
                case Left(err) =>
                  val pe     = ProxyError.upstream(err)
                  val errMsg = pe.message
                  EventEmitter
                    .emit(
                      ProxyEvent.ResponseCaptured(
                        ProxyResponse(reqId, ProxyErrorHttp4s.status(pe).code, errMsg.asLeft[Json], errMsg.some)
                      )
                    )
                    .as(ProxyErrorHttp4s.asResponse(pe))
              }
    } yield resp
  }

  /** Give the response to the client as the upstream sends it, and keep a copy
    * of the body. When the body stream ends, fails or is cancelled, emit
    * `ResponseCaptured` with the copy. For a failure or a cancel, the event has
    * an error text, and the body is the part that was received.
    */
  private def tee(reqId: Long, resp: Response[IO]): IO[Response[IO]] =
    Ref.of[IO, Chunk[Byte]](Chunk.empty[Byte]).map { received =>
      val body = resp
        .body
        .chunks
        .evalTap(chunk => received.update(_ ++ chunk))
        .unchunks
        .onFinalizeCase { exitCase =>
          received.get.flatMap { bytes =>
            EventEmitter.emit(
              ProxyEvent.ResponseCaptured(
                ProxyResponse(reqId, resp.status.code, parseBody(bytes.toArray), streamError(exitCase))
              )
            )
          }
        }
      resp.withBodyStream(body)
    }

  /** The `ResponseCaptured` event for a response whose body the client never read. */
  private def emitAbandoned(reqId: Long, status: Status): IO[Unit] =
    EventEmitter.emit(
      ProxyEvent.ResponseCaptured(
        ProxyResponse(
          reqId,
          status.code,
          "".asLeft[Json],
          "the client did not read the response, so the upstream transfer was stopped".some,
        )
      )
    )

  private def streamError(exitCase: Resource.ExitCase): Option[String] =
    exitCase match {
      case Resource.ExitCase.Succeeded =>
        none[String]
      case Resource.ExitCase.Errored(e) =>
        s"the upstream response stopped before its end: ${ProxyError.upstream(e).message}".some
      case Resource.ExitCase.Canceled =>
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
