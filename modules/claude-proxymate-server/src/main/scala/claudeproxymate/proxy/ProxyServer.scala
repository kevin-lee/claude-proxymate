package claudeproxymate.proxy

import cats.effect.IO
import cats.syntax.all.*
import io.circe.parser.{parse => parseJson}
import org.http4s.*
import org.http4s.client.Client
import claudeproxymate.core.{ProxyError, ProxyEvent, ProxyRequest, ProxyResponse, SseParser}
import claudeproxymate.core.filter.RequestFilter

import java.time.LocalTime
import java.time.format.DateTimeFormatter

/** HTTP proxy routes: intercept requests, apply the request filter, forward to Anthropic, tee responses. */
object ProxyServer {

  private val timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")

  def routes(client: Client[IO], loader: FilterConfigLoader): HttpApp[IO] = HttpApp[IO] { (req: Request[IO]) =>
    for {
      // 1. Read request body
      bodyBytes <- req.body.compile.to(Array)
      bodyJson = if (bodyBytes.isEmpty) none[io.circe.Json]
                 else parseJson(new String(bodyBytes, "UTF-8")).toOption

      // 2. Apply the request filter (config re-read per request; a no-op keeps the original bytes)
      cfg <- loader.load
      outcome   = bodyJson.map(RequestFilter(cfg, _))
      sendJson  = outcome.map(_.body).orElse(bodyJson)
      sendBytes =
        if (outcome.exists(_.report.isDefined)) sendJson.fold(bodyBytes)(_.noSpaces.getBytes("UTF-8"))
        else bodyBytes

      // 3. Create request event (what is actually forwarded, plus the filter report)
      reqId    = System.currentTimeMillis()
      ts       = LocalTime.now().format(timeFormatter)
      proxyReq = ProxyRequest(reqId, ts, req.method.name, req.uri.renderString, sendJson, outcome.flatMap(_.report))
      _ <- EventEmitter.emit(ProxyEvent.RequestCaptured(proxyReq))

      // 4. Forward to Anthropic
      resp <- AnthropicForwarder.forward(client, req, sendBytes).handleErrorWith { err =>
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

      // 5. Tee response: stream to client AND buffer for parsing
      respBytes <- resp.body.compile.to(Array)

      // 6. Parse response
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
