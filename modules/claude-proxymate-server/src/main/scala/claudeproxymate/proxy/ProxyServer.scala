package claudeproxymate.proxy

import cats.effect.IO
import cats.syntax.all.*
import io.circe.parser.{parse => parseJson}
import org.http4s.*
import org.http4s.client.Client
import claudeproxymate.core.{ProxyError, ProxyEvent, ProxyRequest, ProxyResponse, SseParser}

import java.time.LocalTime
import java.time.format.DateTimeFormatter

/** HTTP proxy routes: intercept requests, forward to Anthropic, tee responses. */
object ProxyServer {

  private val timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")

  def routes(client: Client[IO]): HttpApp[IO] = HttpApp[IO] { (req: Request[IO]) =>
    for {
      // 1. Read request body
      bodyBytes <- req.body.compile.to(Array)
      bodyJson = if (bodyBytes.isEmpty) none[io.circe.Json]
                 else parseJson(new String(bodyBytes, "UTF-8")).toOption

      // 2. Create request event
      reqId    = System.currentTimeMillis()
      ts       = LocalTime.now().format(timeFormatter)
      proxyReq = ProxyRequest(reqId, ts, req.method.name, req.uri.renderString, bodyJson)
      _ <- EventEmitter.emit(ProxyEvent.RequestCaptured(proxyReq))

      // 3. Forward to Anthropic
      resp <- AnthropicForwarder.forward(client, req, bodyBytes).handleErrorWith { err =>
                val pe     = ProxyError.upstream(err)
                val errMsg = pe.message
                EventEmitter
                  .emit(
                    ProxyEvent.ResponseCaptured(
                      ProxyResponse(reqId, ProxyErrorHttp4s.status(pe).code, errMsg.asLeft[io.circe.Json], errMsg.some)
                    )
                  )
                  .as(ProxyErrorHttp4s.asResponse(pe))
              }

      // 4. Tee response: stream to client AND buffer for parsing
      respBytes <- resp.body.compile.to(Array)

      // 5. Parse response
      parsedBody = {
        val respStr = new String(respBytes, "UTF-8")
        parseJson(respStr)
          .toOption
          .map(_.asRight[String])
          .orElse(SseParser.parseSseStream(respStr).map(_.asRight[String]))
          .getOrElse(respStr.take(4000).asLeft[io.circe.Json])
      }

      _ <- EventEmitter.emit(
             ProxyEvent.ResponseCaptured(ProxyResponse(reqId, resp.status.code, parsedBody, none[String]))
           )
    } yield {
      // Return the response with the buffered body
      resp.withBodyStream(fs2.Stream.emits(respBytes))
    }
  }
}
