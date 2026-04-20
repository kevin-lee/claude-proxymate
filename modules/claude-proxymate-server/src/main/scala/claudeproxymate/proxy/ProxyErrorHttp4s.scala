package claudeproxymate.proxy

import cats.effect.IO
import claudeproxymate.core.ProxyError
import org.http4s.{EntityEncoder, Response, Status}
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder

/** http4s-specific glue for rendering [[ProxyError]] as an HTTP response.
  *
  * Lives here (and not in `core`) to keep the ADT itself free of http4s
  * dependencies, which are only available on the Scala Native server side.
  */
object ProxyErrorHttp4s {

  def status(pe: ProxyError): Status = pe match {
    case ProxyError.Upstream(_)                => Status.BadGateway
    case ProxyError.CurlInitFailed             => Status.BadGateway
    case ProxyError.CurlPerformFailed(_)       => Status.BadGateway
    case ProxyError.TmpFileFailed              => Status.BadGateway
    case ProxyError.MalformedUpstreamResponse  => Status.BadGateway
    case ProxyError.CurlException(_)           => Status.BadGateway
  }

  given EntityEncoder[IO, ProxyError] = circeEntityEncoder[IO, ProxyError]

  def asResponse(pe: ProxyError): Response[IO] =
    Response[IO](status(pe)).withEntity(pe)
}
