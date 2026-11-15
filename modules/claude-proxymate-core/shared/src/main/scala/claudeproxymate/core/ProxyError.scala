package claudeproxymate.core

import io.circe.{Encoder, Json}

/** Typed errors the proxy can produce when forwarding or parsing.
  *
  * Two catch-all cases — [[ProxyError.Upstream]] on the http4s client side and
  * [[ProxyError.CurlException]] on the native libcurl side — absorb any
  * unanticipated throwable in their respective failure domains.
  *
  * HTTP-specific serialization (entity encoder, status mapping) lives in the
  * server module's `ProxyErrorHttp4s` to keep this file platform-neutral.
  */
enum ProxyError {
  case Upstream(msg: String)
  case CurlInitFailed
  case CurlPerformFailed(code: Int)
  case CurlOptionFailed(code: Int)
  case PipeFailed
  case MalformedUpstreamResponse
  case CurlException(msg: String)

  def message: String = this match {
    case ProxyError.Upstream(m) => m
    case ProxyError.CurlInitFailed => "curl_easy_init() returned null"
    case ProxyError.CurlPerformFailed(code) => s"curl_easy_perform failed with code $code"
    case ProxyError.CurlOptionFailed(code) => s"curl_easy_setopt failed with code $code"
    case ProxyError.PipeFailed => "pipe() or fdopen() failed"
    case ProxyError.MalformedUpstreamResponse => "malformed response from upstream"
    case ProxyError.CurlException(m) => m
  }
}

object ProxyError {

  /** Build an [[ProxyError.Upstream]] error from a throwable, falling back to
    * the exception's simple class name when [[Throwable#getMessage]] is null.
    */
  def upstream(t: Throwable): ProxyError =
    Upstream(Option(t.getMessage).getOrElse(t.getClass.getSimpleName))

  /** Build a [[ProxyError.CurlException]] error from a throwable, falling back
    * to the exception's simple class name when [[Throwable#getMessage]] is null.
    */
  def curlException(t: Throwable): ProxyError =
    CurlException(Option(t.getMessage).getOrElse(t.getClass.getSimpleName))

  given Encoder[ProxyError] =
    Encoder.instance(pe => Json.obj("error" -> Json.fromString(pe.message)))
}
