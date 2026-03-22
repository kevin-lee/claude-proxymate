package claudeproxymate.proxy

import cats.effect.IO
import io.circe.parser.{parse => parseJson}
import org.http4s.*
import org.http4s.client.Client
import claudeproxymate.core.{ProxyEvent, ProxyRequest, ProxyResponse, SseParser}

import java.time.LocalTime
import java.time.format.DateTimeFormatter

/** HTTP proxy routes: intercept requests, forward to Anthropic, tee responses. */
object ProxyServer {

  private val timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")

  def routes(client: Client[IO]): HttpApp[IO] = HttpApp[IO] { (req: Request[IO]) =>
    for {
      // 1. Read request body
      bodyBytes <- req.body.compile.to(Array)
      bodyJson   = if (bodyBytes.isEmpty) None
                   else parseJson(new String(bodyBytes, "UTF-8")).toOption

      // 2. Create request event
      reqId = System.currentTimeMillis()
      ts    = LocalTime.now().format(timeFormatter)
      proxyReq = ProxyRequest(reqId, ts, req.method.name, req.uri.renderString, bodyJson)
      _ <- EventEmitter.emit(ProxyEvent.RequestCaptured(proxyReq))

      // 3. Forward to Anthropic
      resp <- AnthropicForwarder.forward(client, req, bodyBytes).handleErrorWith { err =>
        // On upstream error, return 502 and emit error event
        val errMsg = err.getMessage
        EventEmitter.emit(
          ProxyEvent.ResponseCaptured(ProxyResponse(reqId, 502, Left(errMsg), Some(errMsg)))
        ).as(
          Response[IO](Status.BadGateway)
            .withEntity(s"""{"error":"$errMsg"}""")
        )
      }

      // 4. Tee response: stream to client AND buffer for parsing
      respBytes <- resp.body.compile.to(Array)

      // 5. Parse response
      parsedBody = {
        val respStr = new String(respBytes, "UTF-8")
        parseJson(respStr).toOption
          .map(Right(_))
          .orElse(SseParser.parseSseStream(respStr).map(Right(_)))
          .getOrElse(Left(respStr.take(4000)))
      }

      _ <- EventEmitter.emit(
        ProxyEvent.ResponseCaptured(ProxyResponse(reqId, resp.status.code, parsedBody, None))
      )
    } yield {
      // Return the response with the buffered body
      resp.withBodyStream(fs2.Stream.emits(respBytes))
    }
  }
}
