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
  )

  // ---- helpers -----------------------------------------------------------

  private def hasMatch(s: String, name: String): Boolean =
    TokenPatterns.scan(s).exists(_.name == name)

  // ---- positive cases ----------------------------------------------------

  def testAnthropic: Result = {
    val s = "use sk-ant-abcdefghijklmnopqrstuvwxyz12345 to authenticate"
    Result.assert(hasMatch(s, "anthropic-api-key"))
      .log(s"expected anthropic-api-key match in: $s")
  }

  def testJwt: Result = {
    val s = "token: eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjMifQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c rest"
    Result.assert(hasMatch(s, "jwt"))
      .log(s"expected jwt match in: $s")
  }

  def testGhPat: Result =
    Result.assert(hasMatch("ghp_abcdefghijklmnopqrstuvwxyz0123456789", "github-pat"))
      .log("expected github-pat")

  def testGhFineGrained: Result = {
    val s = "github_pat_" + "A" * 70
    Result.assert(hasMatch(s, "github-fine-grained-pat"))
      .log(s"expected github-fine-grained-pat in: $s")
  }

  def testGhOauth: Result =
    Result.assert(hasMatch("gho_abcdefghijklmnopqrstuvwxyz0123456789", "github-oauth"))
      .log("expected github-oauth")

  def testAwsAkia: Result =
    Result.assert(hasMatch("AKIA0123456789ABCDEF", "aws-access-key"))
      .log("expected aws-access-key")

  def testAwsAsia: Result =
    Result.assert(hasMatch("ASIA0123456789ABCDEF", "aws-temp-key"))
      .log("expected aws-temp-key")

  def testGoogle: Result =
    Result.assert(hasMatch("AIzaSyAbCdEfGhIjKlMnOpQrStUvWxYz01234567", "google-api-key"))
      .log("expected google-api-key")

  def testStripe: Result =
    Result.assert(hasMatch("sk_live_abcdefghijklmnopqrstuvwxyz", "stripe-live-secret"))
      .log("expected stripe-live-secret")

  def testBearer: Result =
    Result.assert(hasMatch("Authorization: Bearer eyJabcdefghijklmno", "bearer"))
      .log("expected bearer")

  def testOpenAi: Result = {
    val s = "OPENAI_API_KEY=sk-abcdefghijklmnopqrstuvwxyz0123456789"
    Result.assert(hasMatch(s, "openai-api-key"))
      .log(s"expected openai-api-key in: $s")
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
}
