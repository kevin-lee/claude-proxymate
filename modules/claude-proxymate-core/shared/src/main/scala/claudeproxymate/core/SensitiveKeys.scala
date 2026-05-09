package claudeproxymate.core

/** Field-name patterns that mark a JSON object value as sensitive
  * for display purposes. Used by the renderer's mask layer to hide
  * the value behind a click-to-reveal placeholder.
  *
  * Display-only — the raw wire data in `AppState.proxyCaptures` is
  * never modified. Match is case-insensitive substring on the
  * lowercased key.
  *
  * Two pattern categories:
  *
  *   1. **Credentials** — `api_key` / `authorization` / `bearer` /
  *      `password` / `*_token` / `secret` / `cookie` / etc. Today
  *      these don't appear in `body` because the proxy server only
  *      forwards `body` to the renderer (not headers); the patterns
  *      stay listed as future-proofing for the planned header-capture
  *      track.
  *   2. **Identifiers** sent in `body.metadata` by Claude Code:
  *      `device_id` / `account_uuid` / `account_id` / `user_id` /
  *      `client_id` / `session_id` / `request_id` / `trace_id`. These
  *      identify a device / account / session and leak identity even
  *      though they're not credential material.
  *
  * Note on `token`: the bare word is intentionally not in the list
  * because Anthropic's API uses non-sensitive token *count* fields
  * heavily (`input_tokens`, `output_tokens`, `cache_read_input_tokens`,
  * etc.). Specific suffixes that carry actual credential material
  * (`auth_token`, `id_token`, `access_token`, `refresh_token`,
  * `session_token`) are listed individually.
  *
  * Note on `id`: the bare word is intentionally not in the list — it
  * would match every Claude API node with an `id` field
  * (`messages[].id`, `tool_use[].id`, `content_block[].id`, …). The
  * correlation-ID hash-tag rendering planned for PR 3 is the right
  * tool for those cases.
  */
object SensitiveKeys {

  val Patterns: List[String] = List(
    // Credentials (header-track; not visible in `body` today)
    "api_key",
    "apikey",
    "x-api-key",
    "authorization",
    "auth_token",
    "access_token",
    "refresh_token",
    "id_token",
    "bearer",
    "password",
    "passwd",
    "secret",
    "client_secret",
    "private_key",
    "session_token",
    "csrf",
    "cookie",
    "set-cookie",
    // Identifiers (body-track; visible in body.metadata)
    "device_id",
    "account_uuid",
    "account_id",
    "user_id",
    "client_id",
    "session_id",
    "request_id",
    "trace_id",
  )

  /** Case-insensitive substring match. Returns true if any pattern
    * appears anywhere in the lowercased key.
    */
  def isSensitive(key: String): Boolean = {
    val lower = key.toLowerCase
    Patterns.exists(lower.contains)
  }
}
