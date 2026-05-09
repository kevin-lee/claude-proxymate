package claudeproxymate.core

/** URL query-string masking for the request `path` shown in the
  * capture list.
  *
  * Display-only — the raw `path` field on the captured request is
  * never modified. Replaces sensitive query-parameter values with
  * `***`. The path before `?` (and any `#fragment`) are preserved
  * verbatim.
  *
  * The sensitive-param predicate composes
  * [[SensitiveKeys.isSensitive]] (covers `api_key`, `*_token`,
  * `password`, `secret`, `cookie`, identifiers like `user_id` /
  * `device_id`, etc.) with a small list of query-only extras
  * (`code`, `state`, `signature`, `sig`, `nonce`) — these are
  * OAuth-style names that would false-positive if added to
  * `SensitiveKeys` (e.g. a JSON field `"code": 200` is an HTTP
  * status, not an OAuth code).
  */
object QueryParamMask {

  /** Sentinel that replaces sensitive query-parameter values. */
  val Sentinel: String = "***"

  /** Query-only param-name extras. Lowercase.
    *
    * Bare `token` is included here (but excluded from
    * `SensitiveKeys` because it would false-positive on
    * `input_tokens` / `output_tokens` count fields). In a query
    * string the name `?token=…` is unambiguously a credential.
    */
  val Extras: List[String] = List(
    "token",
    "code",
    "state",
    "signature",
    "sig",
    "nonce",
  )

  /** True if a query-parameter name (case-insensitive) is sensitive. */
  def isSensitive(paramName: String): Boolean = {
    if (paramName == null || paramName.isEmpty) return false
    val lower = paramName.toLowerCase
    SensitiveKeys.isSensitive(lower) || Extras.contains(lower)
  }

  /** Mask sensitive query-parameter values in `path`. The portion
    * before `?` is preserved verbatim; the `#fragment` (if any) is
    * appended after the masked query string. Repeated keys are each
    * processed independently. Parameters without a value (no `=`)
    * are emitted unchanged.
    *
    * Returns `path` unchanged when there is no `?`.
    */
  def maskPath(path: String): String = {
    if (path == null || path.isEmpty) return path
    val q = path.indexOf('?')
    if (q < 0) return path

    val prefix    = path.substring(0, q + 1) // includes the `?`
    val rest      = path.substring(q + 1)
    val (queryStr, fragment) = {
      val h = rest.indexOf('#')
      if (h < 0) (rest, "") else (rest.substring(0, h), rest.substring(h))
    }

    if (queryStr.isEmpty) return prefix + fragment

    val segments = queryStr.split('&').toList
    val masked   = segments.map(maskSegment)
    prefix + masked.mkString("&") + fragment
  }

  private def maskSegment(segment: String): String = {
    val eq = segment.indexOf('=')
    if (eq < 0) return segment
    val name = segment.substring(0, eq)
    if (isSensitive(decodeNameSafely(name))) s"$name=$Sentinel"
    else segment
  }

  private def decodeNameSafely(name: String): String =
    try java.net.URLDecoder.decode(name, "UTF-8")
    catch { case _: Throwable => name }
}
