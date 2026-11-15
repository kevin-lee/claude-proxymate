package claudeproxymate.proxy

import cats.effect.IO
import cats.syntax.all.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.{`Content-Length`, Host}
import org.typelevel.ci.*

/** Where the proxy sends requests. A parameter, so tests can use a local server. */
final case class Upstream(scheme: Uri.Scheme, host: String, port: Int)

object Upstream {

  /** The Anthropic API. */
  val Anthropic: Upstream = Upstream(Uri.Scheme.https, "api.anthropic.com", 443)
}

/** Forwards requests to the [[Upstream]]. */
object AnthropicForwarder {

  /** Send the request and give the response with its body as a stream.
    *
    * The client `Resource` (for libcurl: the transfer, its pipe and its
    * threads) is released when the body stream ends, fails or is cancelled. The
    * http4s server runs the body stream when it writes the response, so the
    * release follows the client connection.
    */
  def forward(client: Client[IO], upstream: Upstream, req: Request[IO], bodyBytes: Array[Byte]): IO[Response[IO]] = {
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
    ).put(Host(upstream.host, upstream.port.some), `Content-Length`.unsafeFromLong(bodyBytes.length.toLong))

    val upstreamUri = Uri(
      scheme = upstream.scheme.some,
      authority = Uri.Authority(host = Uri.RegName(upstream.host), port = upstream.port.some).some,
      path = req.uri.path,
      query = req.uri.query,
    )

    val upstreamReq = Request[IO](
      method = req.method,
      uri = upstreamUri,
      headers = upstreamHeaders,
      body = fs2.Stream.emits(bodyBytes),
    )

    client.run(upstreamReq).allocated.map {
      case (response, release) =>
        response.withBodyStream(response.body.onFinalize(release))
    }
  }
}
