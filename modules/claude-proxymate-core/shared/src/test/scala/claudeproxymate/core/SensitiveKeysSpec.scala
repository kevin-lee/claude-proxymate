package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object SensitiveKeysSpec extends Properties {

  override def tests: List[Test] = List(
    example("api_key matches", testApiKey),
    example("apikey matches", testApikey),
    example("x-api-key matches", testXApiKey),
    example("Authorization (mixed case) matches", testAuthorizationMixed),
    example("proxy-authorization matches via substring", testProxyAuthorization),
    example("password matches", testPassword),
    example("passwd matches", testPasswd),
    example("set-cookie matches", testSetCookie),
    example("cookie matches", testCookie),
    example("bearer matches", testBearer),
    example("secret matches", testSecret),
    example("client_secret matches", testClientSecret),
    example("private_key matches", testPrivateKey),
    example("auth_token matches", testAuthToken),
    example("access_token matches", testAccessToken),
    example("refresh_token matches", testRefreshToken),
    example("id_token matches", testIdToken),
    example("session_id matches", testSessionId),
    example("session_token matches", testSessionToken),
    example("csrf matches", testCsrf),
    // Identifier patterns (body-track)
    example("device_id matches", testDeviceId),
    example("account_uuid matches", testAccountUuid),
    example("account_id matches", testAccountId),
    example("user_id matches", testUserId),
    example("client_id matches", testClientId),
    example("request_id matches", testRequestId),
    example("trace_id matches", testTraceId),
    example("metadata.user_id (with prefix) matches via substring", testMetadataUserId),
    example("token alone does NOT match (input_tokens guard)", testTokenAloneNoMatch),
    example("id alone does NOT match (covers messages[].id, tool_use[].id, etc.)", testIdAloneNoMatch),
    example("message_id does NOT match (correlation IDs deferred to PR 3)", testMessageIdNoMatch),
    example("input_tokens does NOT match", testInputTokensNoMatch),
    example("output_tokens does NOT match", testOutputTokensNoMatch),
    example("cache_read_input_tokens does NOT match", testCacheReadInputTokensNoMatch),
    example("cache_creation_input_tokens does NOT match", testCacheCreationInputTokensNoMatch),
    example("model does NOT match", testModelNoMatch),
    example("role does NOT match", testRoleNoMatch),
    example("content does NOT match", testContentNoMatch),
    example("empty string does NOT match", testEmptyNoMatch),
    property("any string containing a pattern matches", testStringContainingPattern),
    property("any pattern in any case matches", testPatternAnyCase),
    property("string of random ASCII without any pattern does not match", testRandomNoMatch),
  )

  private def must(s: String, expected: Boolean): Result =
    Result
      .assert(SensitiveKeys.isSensitive(s) == expected)
      .log(s"isSensitive(\"$s\") expected $expected")

  def testApiKey: Result             = must("api_key", true)
  def testApikey: Result             = must("apikey", true)
  def testXApiKey: Result            = must("x-api-key", true)
  def testAuthorizationMixed: Result = must("Authorization", true)
  def testProxyAuthorization: Result = must("proxy-authorization", true)
  def testPassword: Result           = must("password", true)
  def testPasswd: Result             = must("passwd", true)
  def testSetCookie: Result          = must("Set-Cookie", true)
  def testCookie: Result             = must("cookie", true)
  def testBearer: Result             = must("Bearer", true)
  def testSecret: Result             = must("secret", true)
  def testClientSecret: Result       = must("client_secret", true)
  def testPrivateKey: Result         = must("private_key", true)
  def testAuthToken: Result          = must("auth_token", true)
  def testAccessToken: Result        = must("access_token", true)
  def testRefreshToken: Result       = must("refresh_token", true)
  def testIdToken: Result            = must("id_token", true)
  def testSessionId: Result          = must("session_id", true)
  def testSessionToken: Result       = must("session_token", true)
  def testCsrf: Result               = must("csrf", true)

  def testDeviceId: Result       = must("device_id", true)
  def testAccountUuid: Result    = must("account_uuid", true)
  def testAccountId: Result      = must("account_id", true)
  def testUserId: Result         = must("user_id", true)
  def testClientId: Result       = must("client_id", true)
  def testRequestId: Result      = must("request_id", true)
  def testTraceId: Result        = must("trace_id", true)
  def testMetadataUserId: Result = must("metadata.user_id", true)

  def testTokenAloneNoMatch: Result               = must("token", false)
  def testIdAloneNoMatch: Result                  = must("id", false)
  def testMessageIdNoMatch: Result                = must("message_id", false)
  def testInputTokensNoMatch: Result              = must("input_tokens", false)
  def testOutputTokensNoMatch: Result             = must("output_tokens", false)
  def testCacheReadInputTokensNoMatch: Result     = must("cache_read_input_tokens", false)
  def testCacheCreationInputTokensNoMatch: Result = must("cache_creation_input_tokens", false)
  def testModelNoMatch: Result                    = must("model", false)
  def testRoleNoMatch: Result                     = must("role", false)
  def testContentNoMatch: Result                  = must("content", false)
  def testEmptyNoMatch: Result                    = must("", false)

  def testStringContainingPattern: Property =
    for {
      pattern <- Gen.element1(SensitiveKeys.Patterns.head, SensitiveKeys.Patterns.tail*).log("pattern")
      prefix  <- Gen.string(Gen.alpha, Range.linear(0, 6)).log("prefix")
      suffix  <- Gen.string(Gen.alpha, Range.linear(0, 6)).log("suffix")
    } yield {
      val s = s"$prefix$pattern$suffix"
      Result
        .assert(SensitiveKeys.isSensitive(s))
        .log(s"isSensitive(\"$s\") expected true (built from pattern \"$pattern\")")
    }

  def testPatternAnyCase: Property =
    for {
      pattern <- Gen.element1(SensitiveKeys.Patterns.head, SensitiveKeys.Patterns.tail*).log("pattern")
    } yield {
      // Flip every char's case, then assert match still holds.
      val flipped = pattern.map(c =>
        if (c.isLetter) {
          if (c.isUpper) c.toLower else c.toUpper
        } else c
      )
      Result
        .assert(SensitiveKeys.isSensitive(flipped))
        .log(s"isSensitive(\"$flipped\") expected true (case-flipped pattern \"$pattern\")")
    }

  def testRandomNoMatch: Property =
    for {
      // Use a small alphabet that can't accidentally form any pattern.
      // Patterns include letters and `-` `_`; restrict to digits + ` ` only.
      s <- Gen.string(Gen.frequency1(9 -> Gen.digit, 1 -> Gen.constant(' ')), Range.linear(0, 30)).log("s")
    } yield {
      Result
        .assert(!SensitiveKeys.isSensitive(s))
        .log(s"isSensitive(\"$s\") expected false (no letters / underscores / dashes)")
    }
}
