package claudeproxymate.core

/* munit port of the shared hedgehog TokenPatternsSpec for Scala Native
 * (the hedgehog runner hangs on SN 0.5, so coreNative runs munit-only
 * tests). Examples port 1:1; each hedgehog property becomes a small set
 * of deterministic tests over representative inputs asserting the same
 * invariant. */
class TokenPatternsSpec extends munit.FunSuite {

  /* Full-coverage assertion: the scan must yield a match of the given
   * name spanning the entire input. A prefix-only match would leave
   * the token's tail outside the masked span (exposed on screen and
   * in copies). */
  private def coversWhole(s: String, name: String): Unit = {
    val ms = TokenPatterns.scan(s)
    assert(
      ms.exists(m => m.name == name && m.start == 0 && m.end == s.length),
      s"expected $name covering [0, ${s.length}) in: $s, got: $ms",
    )
  }

  /* The substring actually matched for the given pattern name, if any
   * — lets embedded-token tests pin the exact matched slice. */
  private def matchedSlice(s: String, name: String): Option[String] =
    TokenPatterns.scan(s).find(_.name == name).map(m => s.substring(m.start, m.end))

  /* ---- positive cases ---- */

  test("Anthropic api key matches") {
    val token = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
    assertEquals(matchedSlice(s"use $token to authenticate", "anthropic-api-key"), Some(token))
  }

  test("JWT 3-segment shape matches") {
    val token = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjMifQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
    assertEquals(matchedSlice(s"token: $token rest", "jwt"), Some(token))
  }

  test("GitHub PAT (classic ghp_) matches") {
    coversWhole("ghp_abcdefghijklmnopqrstuvwxyz0123456789", "github-pat")
  }

  test("GitHub fine-grained PAT matches") {
    coversWhole("github_pat_" + "A" * 70, "github-fine-grained-pat")
  }

  test("GitHub OAuth (gho_) matches") {
    coversWhole("gho_abcdefghijklmnopqrstuvwxyz0123456789", "github-oauth")
  }

  test("AWS access key (AKIA…) matches") {
    coversWhole("AKIA0123456789ABCDEF", "aws-access-key")
  }

  test("AWS temp key (ASIA…) matches") {
    coversWhole("ASIA0123456789ABCDEF", "aws-temp-key")
  }

  test("Google API key (AIza…) matches") {
    coversWhole("AIzaSyAbCdEfGhIjKlMnOpQrStUvWxYz0123456", "google-api-key")
  }

  /* Regression: a token one char longer than the canonical 39-char
   * Google key must be masked whole, not split into a 39-char match
   * plus an exposed trailing char (was rendered as `AIza…3456` + `7`
   * and copied as `***7`). */
  test("Google API key longer than canonical 39 chars is covered whole") {
    coversWhole("AIzaSyAbCdEfGhIjKlMnOpQrStUvWxYz01234567", "google-api-key")
  }

  test("Stripe sk_live_ matches") {
    coversWhole("sk_live_abcdefghijklmnopqrstuvwxyz", "stripe-live-secret")
  }

  test("Bearer header form matches") {
    val token = "Bearer eyJabcdefghijklmno"
    assertEquals(matchedSlice(s"Authorization: $token", "bearer"), Some(token))
  }

  test("OpenAI sk- matches when not preceded by sk-ant-") {
    val token = "sk-abcdefghijklmnopqrstuvwxyz0123456789"
    assertEquals(matchedSlice(s"OPENAI_API_KEY=$token", "openai-api-key"), Some(token))
  }

  /* ---- negative cases ---- */

  test("plain model name does NOT match") {
    assert(TokenPatterns.scan("model: claude-3-5-sonnet-20241022").isEmpty, "plain model name should not match")
  }

  test("64-char hex commit hash does NOT match") {
    assert(
      TokenPatterns.scan("commit a3f9c7b1e2d4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0").isEmpty,
      "64-char hex should not match",
    )
  }

  test("'Bearer' alone does NOT match") {
    assert(TokenPatterns.scan("Bearer").isEmpty, "'Bearer' alone should not match")
  }

  test("sk-ant- truncated below threshold does NOT match") {
    assert(TokenPatterns.scan("sk-ant-short").isEmpty, "sk-ant- with < 20 chars should not match")
  }

  test("'eyJ' alone does NOT match (need 3 segments)") {
    assert(TokenPatterns.scan("eyJabc").isEmpty, "single-segment eyJ... should not match")
  }

  /* ---- ordering / overlap ---- */

  test("sk-ant- prefers Anthropic, not OpenAI") {
    /* sk-ant-... could also be matched by sk- (openai) — Anthropic should win. */
    val ms = TokenPatterns.scan("sk-ant-abcdefghijklmnopqrstuvwxyz12345")
    assertEquals(ms.map(_.name), List("anthropic-api-key"))
  }

  test("multiple matches returned in start order") {
    val s = "AKIA0123456789ABCDEF and AIzaSyAbCdEfGhIjKlMnOpQrStUvWxYz01234567"
    TokenPatterns.scan(s) match {
      case List(first, second) =>
        assert(first.start < second.start, s"matches not in start order: $first, $second")
        assertEquals(first.name, "aws-access-key")
        assertEquals(second.name, "google-api-key")
      case other => fail(s"expected 2 matches, got: $other")
    }
  }

  test("non-overlapping matches both returned") {
    val s  = "ghp_abcdefghijklmnopqrstuvwxyz0123456789 separator gho_zyxwvutsrqponmlkjihgfedcba9876543210"
    val ms = TokenPatterns.scan(s)
    assertEquals(ms.size, 2, s"expected 2 matches, got: $ms")
  }

  /* ---- fingerprint ---- */

  test("fingerprint long: first-4 ellipsis last-4") {
    assertEquals(TokenPatterns.fingerprint("sk-ant-abcdefghijklmnopXYZ4"), "sk-a…XYZ4")
  }

  test("fingerprint exactly 8 chars: ***") {
    assertEquals(TokenPatterns.fingerprint("12345678"), "***")
  }

  test("fingerprint short: ***") {
    assertEquals(TokenPatterns.fingerprint("abc"), "***")
  }

  test("fingerprint empty: ***") {
    assertEquals(TokenPatterns.fingerprint(""), "***")
  }

  /* ---- deterministic ports of the hedgehog properties ---- */

  /* Property: digit/space strings with no recognized prefix produce
   * zero matches. Representatives cover the generator's
   * Range.linear(0, 50) boundaries and the digit/space mix. */

  test("random alphanumeric of length 50 with no recognized prefix produces zero matches (example: empty string)") {
    assert(TokenPatterns.scan("").isEmpty, "empty string should not match")
  }

  test("random alphanumeric of length 50 with no recognized prefix produces zero matches (example: 50 digits)") {
    val s = "0123456789" * 5
    assert(TokenPatterns.scan(s).isEmpty, s"digit string should not match: $s")
  }

  test("random alphanumeric of length 50 with no recognized prefix produces zero matches (example: digits with spaces)") {
    val s = "1234 5678 9012 3456 7890"
    assert(TokenPatterns.scan(s).isEmpty, s"digit/space string should not match: $s")
  }

  /* Canonical-length tokens for every fixed-length format. Upper /
   * digit suffix chars are valid in all of these formats' charsets
   * ([a-zA-Z0-9], [A-Z0-9], [a-zA-Z0-9_\-]), so a suffix always
   * extends the token rather than terminating it — the whole run must
   * be covered (no tail leak). Suffix lengths cover the generator's
   * Range.linear(1, 12) boundaries. */
  private val canonicalFixedLengthTokens: List[(String, String)] = List(
    ("ghp_abcdefghijklmnopqrstuvwxyz0123456789", "github-pat"),
    ("gho_abcdefghijklmnopqrstuvwxyz0123456789", "github-oauth"),
    ("AKIA0123456789ABCDEF", "aws-access-key"),
    ("ASIA0123456789ABCDEF", "aws-temp-key"),
    ("AIzaSyAbCdEfGhIjKlMnOpQrStUvWxYz0123456", "google-api-key"),
  )

  test("fixed-length tokens extended with charset chars are covered whole (no tail leak) (example: 1-char suffix)") {
    canonicalFixedLengthTokens.foreach { case (token, name) => coversWhole(token + "7", name) }
  }

  test("fixed-length tokens extended with charset chars are covered whole (no tail leak) (example: 12-char suffix)") {
    canonicalFixedLengthTokens.foreach { case (token, name) => coversWhole(token + "AB12CD34EF56", name) }
  }
}
