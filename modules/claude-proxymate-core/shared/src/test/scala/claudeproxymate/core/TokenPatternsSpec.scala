package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object TokenPatternsSpec extends Properties {

  override def tests: List[Test] = List(
    // Per-pattern positive
    example("Anthropic api key matches", testAnthropic),
    example("JWT 3-segment shape matches", testJwt),
    example("GitHub PAT (classic ghp_) matches", testGhPat),
    example("GitHub fine-grained PAT matches", testGhFineGrained),
    example("GitHub OAuth (gho_) matches", testGhOauth),
    example("AWS access key (AKIA…) matches", testAwsAkia),
    example("AWS temp key (ASIA…) matches", testAwsAsia),
    example("Google API key (AIza…) matches", testGoogle),
    example("Google API key longer than canonical 39 chars is covered whole", testGoogleOverlong),
    example("Stripe sk_live_ matches", testStripe),
    example("Bearer header form matches", testBearer),
    example("OpenAI sk- matches when not preceded by sk-ant-", testOpenAi),
    // Negative
    example("plain model name does NOT match", testPlainModel),
    example("64-char hex commit hash does NOT match", testCommitHash),
    example("'Bearer' alone does NOT match", testBearerAlone),
    example("sk-ant- truncated below threshold does NOT match", testAnthropicShort),
    example("'eyJ' alone does NOT match (need 3 segments)", testJwtSingleSegment),
    // Ordering / overlap
    example("sk-ant- prefers Anthropic, not OpenAI", testAnthropicOverOpenAi),
    example("multiple matches returned in start order", testMultipleInOrder),
    example("non-overlapping matches both returned", testNonOverlapping),
    // Fingerprint
    example("fingerprint long: first-4 ellipsis last-4", testFingerprintLong),
    example("fingerprint exactly 8 chars: ***", testFingerprintEight),
    example("fingerprint short: ***", testFingerprintShort),
    example("fingerprint empty: ***", testFingerprintEmpty),
    // Property
    property("random alphanumeric of length 50 with no recognized prefix produces zero matches", testRandomNoMatch),
    property("fixed-length tokens extended with charset chars are covered whole (no tail leak)", testOverlongTokenFullyCovered),
  )

  // ---- helpers -----------------------------------------------------------

  /* Full-coverage assertion: the scan must yield a match of the given
   * name spanning the entire input. A prefix-only match would leave
   * the token's tail outside the masked span (exposed on screen and
   * in copies). */
  private def coversWhole(s: String, name: String): Result = {
    val ms = TokenPatterns.scan(s)
    Result.assert(ms.exists(m => m.name == name && m.start == 0 && m.end == s.length))
      .log(s"expected $name covering [0, ${s.length}) in: $s, got: $ms")
  }

  /* The substring actually matched for the given pattern name, if any
   * — lets embedded-token tests pin the exact matched slice. */
  private def matchedSlice(s: String, name: String): Option[String] =
    TokenPatterns.scan(s).find(_.name == name).map(m => s.substring(m.start, m.end))

  // ---- positive cases ----------------------------------------------------

  def testAnthropic: Result = {
    val token = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    matchedSlice(s"use $token to authenticate", "anthropic-api-key") ==== Some(token)
  }

  def testJwt: Result = {
    val token = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjMifQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
    matchedSlice(s"token: $token rest", "jwt") ==== Some(token)
  }

  def testGhPat: Result =
    coversWhole("ghp_abcdefghijklmnopqrstuvwxyz0123456789", "github-pat")

  def testGhFineGrained: Result =
    coversWhole("github_pat_" + "A" * 70, "github-fine-grained-pat")

  def testGhOauth: Result =
    coversWhole("gho_abcdefghijklmnopqrstuvwxyz0123456789", "github-oauth")

  def testAwsAkia: Result =
    coversWhole("AKIA0123456789ABCDEF", "aws-access-key")

  def testAwsAsia: Result =
    coversWhole("ASIA0123456789ABCDEF", "aws-temp-key")

  def testGoogle: Result =
    coversWhole("AIzaSyAbCdEfGhIjKlMnOpQrStUvWxYz0123456", "google-api-key")

  /* Regression: a token one char longer than the canonical 39-char
   * Google key must be masked whole, not split into a 39-char match
   * plus an exposed trailing char (was rendered as `AIza…3456` + `7`
   * and copied as `***7`). */
  def testGoogleOverlong: Result =
    coversWhole("AIzaSyAbCdEfGhIjKlMnOpQrStUvWxYz01234567", "google-api-key")

  def testStripe: Result =
    coversWhole("sk_live_abcdefghijklmnopqrstuvwxyz", "stripe-live-secret")

  def testBearer: Result = {
    val token = "Bearer eyJabcdefghijklmno"
    matchedSlice(s"Authorization: $token", "bearer") ==== Some(token)
  }

  def testOpenAi: Result = {
    val token = "sk-abcdefghijklmnopqrstuvwxyz0123456789"
    matchedSlice(s"OPENAI_API_KEY=$token", "openai-api-key") ==== Some(token)
  }

  // ---- negative cases ----------------------------------------------------

  def testPlainModel: Result =
    Result.assert(TokenPatterns.scan("model: claude-3-5-sonnet-20241022").isEmpty)
      .log("plain model name should not match")

  def testCommitHash: Result =
    Result.assert(TokenPatterns.scan("commit a3f9c7b1e2d4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0").isEmpty)
      .log("64-char hex should not match")

  def testBearerAlone: Result =
    Result.assert(TokenPatterns.scan("Bearer").isEmpty)
      .log("'Bearer' alone should not match")

  def testAnthropicShort: Result =
    Result.assert(TokenPatterns.scan("sk-ant-short").isEmpty)
      .log("sk-ant- with < 20 chars should not match")

  def testJwtSingleSegment: Result =
    Result.assert(TokenPatterns.scan("eyJabc").isEmpty)
      .log("single-segment eyJ... should not match")

  // ---- ordering / overlap ------------------------------------------------

  def testAnthropicOverOpenAi: Result = {
    // sk-ant-... could also be matched by sk- (openai) — Anthropic should win.
    val s  = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    val ms = TokenPatterns.scan(s)
    Result.all(
      List(
        Result.assert(ms.size == 1).log(s"expected exactly 1 match, got: $ms"),
        Result.assert(ms.headOption.exists(_.name == "anthropic-api-key"))
          .log(s"expected anthropic-api-key, got: $ms"),
      )
    )
  }

  def testMultipleInOrder: Result = {
    val s  = "AKIA0123456789ABCDEF and AIzaSyAbCdEfGhIjKlMnOpQrStUvWxYz01234567"
    val ms = TokenPatterns.scan(s)
    Result.all(
      List(
        Result.assert(ms.size == 2).log(s"expected 2 matches, got: $ms"),
        Result.assert(ms.head.start < ms(1).start).log(s"matches not in start order: $ms"),
        Result.assert(ms.head.name == "aws-access-key").log(s"first match should be aws-access-key, got: $ms"),
        Result.assert(ms(1).name == "google-api-key").log(s"second match should be google-api-key, got: $ms"),
      )
    )
  }

  def testNonOverlapping: Result = {
    val s  = "ghp_abcdefghijklmnopqrstuvwxyz0123456789 separator gho_zyxwvutsrqponmlkjihgfedcba9876543210"
    val ms = TokenPatterns.scan(s)
    Result.assert(ms.size == 2).log(s"expected 2 matches, got: $ms")
  }

  // ---- fingerprint -------------------------------------------------------

  def testFingerprintLong: Result =
    Result.assert(TokenPatterns.fingerprint("sk-ant-abcdefghijklmnopXYZ4") == "sk-a…XYZ4")
      .log(s"fingerprint long failed: ${TokenPatterns.fingerprint("sk-ant-abcdefghijklmnopXYZ4")}")

  def testFingerprintEight: Result =
    Result.assert(TokenPatterns.fingerprint("12345678") == "***")
      .log(s"8-char fingerprint should be ***: got ${TokenPatterns.fingerprint("12345678")}")

  def testFingerprintShort: Result =
    Result.assert(TokenPatterns.fingerprint("abc") == "***")
      .log("short fingerprint should be ***")

  def testFingerprintEmpty: Result =
    Result.assert(TokenPatterns.fingerprint("") == "***")
      .log("empty fingerprint should be ***")

  // ---- property ----------------------------------------------------------

  def testRandomNoMatch: Property =
    for {
      // Plain digits + space — can't form any recognized prefix.
      s <- Gen.string(Gen.frequency1(9 -> Gen.digit, 1 -> Gen.constant(' ')), Range.linear(0, 50)).log("s")
    } yield {
      Result.assert(TokenPatterns.scan(s).isEmpty)
        .log(s"random digit string should not match: $s")
    }

  def testOverlongTokenFullyCovered: Property = {
    /* Canonical-length tokens for every fixed-length format. */
    val canonical = List(
      ("ghp_abcdefghijklmnopqrstuvwxyz0123456789", "github-pat"),
      ("gho_abcdefghijklmnopqrstuvwxyz0123456789", "github-oauth"),
      ("AKIA0123456789ABCDEF", "aws-access-key"),
      ("ASIA0123456789ABCDEF", "aws-temp-key"),
      ("AIzaSyAbCdEfGhIjKlMnOpQrStUvWxYz0123456", "google-api-key"),
    )
    for {
      /* Upper / digit chars are valid in all of these formats'
       * charsets ([a-zA-Z0-9], [A-Z0-9], [a-zA-Z0-9_\-]), so the
       * suffix always extends the token rather than terminating it. */
      suffix <- Gen.string(Gen.choice1(Gen.upper, Gen.digit), Range.linear(1, 12)).log("suffix")
    } yield Result.all(
      canonical.map { case (token, name) => coversWhole(token + suffix, name) }
    )
  }
}
