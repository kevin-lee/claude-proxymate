package claudeproxymate.proxy

import cats.effect.IO
import cats.syntax.all.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.{`Content-Length`, Host}
import org.typelevel.ci.*

/** Forwards requests to the Anthropic API (api.anthropic.com:443). */
object AnthropicForwarder {

  private val anthropicHost = "api.anthropic.com"
  private val anthropicPort = 443

  def forward(client: Client[IO], req: Request[IO], bodyBytes: Array[Byte]): IO[Response[IO]] = {
    /* Build upstream request with modified headers. The body may have been
     * rewritten by the request filter, so the client's Content-Length (and any
     * Transfer-Encoding) is dropped and the real length is set explicitly:
     * ember does not derive it for a raw byte stream. */
    val upstreamHeaders = Headers(
      req
        .headers
        .headers
        .filterNot(h =>
          h.name === ci"Accept-Encoding" || h.name === ci"Content-Length" || h.name === ci"Transfer-Encoding"
        )
    ).put(Host(anthropicHost, anthropicPort.some), `Content-Length`.unsafeFromLong(bodyBytes.length.toLong))

    val upstreamUri = Uri(
      scheme = Uri.Scheme.https.some,
      authority = Uri.Authority(host = Uri.RegName(anthropicHost), port = anthropicPort.some).some,
      path = req.uri.path,
      query = req.uri.query,
    )

    val upstreamReq = Request[IO](
      method = req.method,
      uri = upstreamUri,
      headers = upstreamHeaders,
      body = fs2.Stream.emits(bodyBytes),
    )

    client.run(upstreamReq).allocated.map { case (response, _) => response }
  }
}
