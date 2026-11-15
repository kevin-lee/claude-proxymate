package claudeproxymate.proxy

import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.{host, port, Host as IpHost, Port}
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.{`Content-Length`, Host}
import org.typelevel.ci.*

/** Where the proxy sends requests. A parameter, so tests can use a local server. */
final case class Upstream(scheme: Uri.Scheme, host: IpHost, port: Port)

object Upstream {

  /** The Anthropic API. */
  val Anthropic: Upstream = Upstream(Uri.Scheme.https, host"api.anthropic.com", port"443")
}

/** Forwards requests to the [[Upstream]]. */
object AnthropicForwarder {

  /** Send the request and give the response, with its body as a stream, and
    * the release of the client `Resource` (for libcurl: the transfer, its pipe
    * and its threads).
    *
    * The caller must run the body or the release. A body that the http4s
    * server never runs would keep the transfer, so the caller wraps the
    * response with [[BodyGuard]].
    */
  def forward(client: Client[IO], upstream: Upstream, req: Request[IO], bodyBytes: Array[Byte]): IO[Forwarded] = {
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
    ).put(
      Host(upstream.host.toString, upstream.port.value.some),
      `Content-Length`.unsafeFromLong(bodyBytes.length.toLong)
    )

    val upstreamUri = Uri(
      scheme = upstream.scheme.some,
      authority = Uri.Authority(host = Uri.Host.fromIp4sHost(upstream.host), port = upstream.port.value.some).some,
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
        Forwarded(response, release)
    }
  }
}
