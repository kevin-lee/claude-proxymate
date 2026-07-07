package claudeproxymate.core

import cats.syntax.all.*

/** Reasons a URL might be refused by [[UrlScheme.validate]]. */
enum UrlSchemeError {
  case Malformed(input: String)
  case Disallowed(scheme: String, allowed: List[String])

  def message: String = this match {
    case UrlSchemeError.Malformed(input) =>
      s"malformed URL: \"$input\""
    case UrlSchemeError.Disallowed(scheme, allowed) =>
      s"scheme \"$scheme\" not allowed (allowed: ${allowed.mkString(", ")})"
  }
}

/** Validate a URL before handing it to `shell.openExternal`.
  *
  * Returns the input URL unchanged on success — proxy-pass-through
  * semantics, we never rewrite the URL the caller provided.
  *
  * Scheme matching is case-insensitive per RFC 3986.
  */
object UrlScheme {

  val AllowedSchemes: List[String] = List("https")

  def validate(url: String): Either[UrlSchemeError, String] = {
    val colon = url.indexOf(':')
    if (colon <= 0) {
      UrlSchemeError.Malformed(url).asLeft[String]
    } else {
      val scheme = url.substring(0, colon).toLowerCase
      Either.cond(AllowedSchemes.contains(scheme), url, UrlSchemeError.Disallowed(scheme, AllowedSchemes))
    }
  }
}
