package claudeproxymate.core

/* munit port of the shared hedgehog SensitiveKeysSpec for Scala
 * Native (the hedgehog runner hangs on Native). Properties are
 * ported as deterministic representative examples asserting the
 * same invariants. */
class SensitiveKeysSpec extends munit.FunSuite {

  test("api_key matches") {
    assert(SensitiveKeys.isSensitive("api_key"))
  }

  test("apikey matches") {
    assert(SensitiveKeys.isSensitive("apikey"))
  }

  test("x-api-key matches") {
    assert(SensitiveKeys.isSensitive("x-api-key"))
  }

  test("Authorization (mixed case) matches") {
    assert(SensitiveKeys.isSensitive("Authorization"))
  }

  test("proxy-authorization matches via substring") {
    assert(SensitiveKeys.isSensitive("proxy-authorization"))
  }

  test("password matches") {
    assert(SensitiveKeys.isSensitive("password"))
  }

  test("passwd matches") {
    assert(SensitiveKeys.isSensitive("passwd"))
  }

  test("set-cookie matches") {
    assert(SensitiveKeys.isSensitive("Set-Cookie"))
  }

  test("cookie matches") {
    assert(SensitiveKeys.isSensitive("cookie"))
  }

  test("bearer matches") {
    assert(SensitiveKeys.isSensitive("Bearer"))
  }

  test("secret matches") {
    assert(SensitiveKeys.isSensitive("secret"))
  }

  test("client_secret matches") {
    assert(SensitiveKeys.isSensitive("client_secret"))
  }

  test("private_key matches") {
    assert(SensitiveKeys.isSensitive("private_key"))
  }

  test("auth_token matches") {
    assert(SensitiveKeys.isSensitive("auth_token"))
  }

  test("access_token matches") {
    assert(SensitiveKeys.isSensitive("access_token"))
  }

  test("refresh_token matches") {
    assert(SensitiveKeys.isSensitive("refresh_token"))
  }

  test("id_token matches") {
    assert(SensitiveKeys.isSensitive("id_token"))
  }

  test("session_id matches") {
    assert(SensitiveKeys.isSensitive("session_id"))
  }

  test("session_token matches") {
    assert(SensitiveKeys.isSensitive("session_token"))
  }

  test("csrf matches") {
    assert(SensitiveKeys.isSensitive("csrf"))
  }

  /* ── Identifier patterns (body-track) ── */

  test("device_id matches") {
    assert(SensitiveKeys.isSensitive("device_id"))
  }

  test("account_uuid matches") {
    assert(SensitiveKeys.isSensitive("account_uuid"))
  }

  test("account_id matches") {
    assert(SensitiveKeys.isSensitive("account_id"))
  }

  test("user_id matches") {
    assert(SensitiveKeys.isSensitive("user_id"))
  }

  test("client_id matches") {
    assert(SensitiveKeys.isSensitive("client_id"))
  }

  test("request_id matches") {
    assert(SensitiveKeys.isSensitive("request_id"))
  }

  test("trace_id matches") {
    assert(SensitiveKeys.isSensitive("trace_id"))
  }

  test("metadata.user_id (with prefix) matches via substring") {
    assert(SensitiveKeys.isSensitive("metadata.user_id"))
  }

  test("token alone does NOT match (input_tokens guard)") {
    assert(!SensitiveKeys.isSensitive("token"))
  }

  test("id alone does NOT match (covers messages[].id, tool_use[].id, etc.)") {
    assert(!SensitiveKeys.isSensitive("id"))
  }

  test("message_id does NOT match (correlation IDs deferred to PR 3)") {
    assert(!SensitiveKeys.isSensitive("message_id"))
  }

  test("input_tokens does NOT match") {
    assert(!SensitiveKeys.isSensitive("input_tokens"))
  }

  test("output_tokens does NOT match") {
    assert(!SensitiveKeys.isSensitive("output_tokens"))
  }

  test("cache_read_input_tokens does NOT match") {
    assert(!SensitiveKeys.isSensitive("cache_read_input_tokens"))
  }

  test("cache_creation_input_tokens does NOT match") {
    assert(!SensitiveKeys.isSensitive("cache_creation_input_tokens"))
  }

  test("model does NOT match") {
    assert(!SensitiveKeys.isSensitive("model"))
  }

  test("role does NOT match") {
    assert(!SensitiveKeys.isSensitive("role"))
  }

  test("content does NOT match") {
    assert(!SensitiveKeys.isSensitive("content"))
  }

  test("empty string does NOT match") {
    assert(!SensitiveKeys.isSensitive(""))
  }

  /* ── properties ported as deterministic examples ── */

  test("any string containing a pattern matches (example: xxapi_keyyy, prefix+suffix around api_key)") {
    assert(SensitiveKeys.isSensitive("xxapi_keyyy"))
  }

  test("any string containing a pattern matches (example: prebearer, prefix only)") {
    assert(SensitiveKeys.isSensitive("prebearer"))
  }

  test("any string containing a pattern matches (example: secret, empty prefix and suffix)") {
    assert(SensitiveKeys.isSensitive("secret"))
  }

  test("any pattern in any case matches (example: API_KEY)") {
    assert(SensitiveKeys.isSensitive("API_KEY"))
  }

  test("any pattern in any case matches (example: SET-COOKIE)") {
    assert(SensitiveKeys.isSensitive("SET-COOKIE"))
  }

  /* Patterns include letters and `-` `_`, so strings restricted to
   * digits and spaces can never contain any pattern. */

  test("string of random ASCII without any pattern does not match (example: empty string)") {
    assert(!SensitiveKeys.isSensitive(""))
  }

  test("string of random ASCII without any pattern does not match (example: digits only)") {
    assert(!SensitiveKeys.isSensitive("0123456789"))
  }

  test("string of random ASCII without any pattern does not match (example: digits and spaces)") {
    assert(!SensitiveKeys.isSensitive("42 007 13"))
  }
}
