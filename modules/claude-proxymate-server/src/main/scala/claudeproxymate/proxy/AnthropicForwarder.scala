package claudeproxymate.proxy

import cats.effect.IO
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.Host
import org.typelevel.ci.*

/** Forwards requests to the Anthropic API (api.anthropic.com:443). */
object AnthropicForwarder {

  private val anthropicHost = "api.anthropic.com"
  private val anthropicPort = 443

  def forward(client: Client[IO], req: Request[IO], bodyBytes: Array[Byte]): IO[Response[IO]] = {
    // Build upstream request with modified headers
    val upstreamHeaders = Headers(
      req.headers.headers.filterNot(_.name == ci"Accept-Encoding")
    ).put(Host(anthropicHost, Some(anthropicPort)))

    val upstreamUri = Uri(
      scheme = Some(Uri.Scheme.https),
      authority = Some(Uri.Authority(host = Uri.RegName(anthropicHost), port = Some(anthropicPort))),
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
