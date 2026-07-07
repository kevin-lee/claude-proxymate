package claudeproxymate.core

/* munit port of the shared hedgehog UrlSchemeSpec for Scala Native
 * (the hedgehog runner hangs on Native). Properties are ported as
 * deterministic representative examples. */
class UrlSchemeSpec extends munit.FunSuite {

  test("https URL is accepted") {
    val url = "https://docs.anthropic.com/en/docs/about-claude/models"
    assertEquals(UrlScheme.validate(url), Right(url))
  }

  test("HTTPS URL (uppercase) is accepted (scheme case-insensitive)") {
    val url = "HTTPS://EXAMPLE.COM/path"
    assertEquals(UrlScheme.validate(url), Right(url))
  }

  test("http URL is rejected as Disallowed") {
    assertEquals(
      UrlScheme.validate("http://example.com"),
      Left(UrlSchemeError.Disallowed("http", UrlScheme.AllowedSchemes)),
    )
  }

  test("javascript: URL is rejected as Disallowed") {
    assertEquals(
      UrlScheme.validate("javascript:alert(1)"),
      Left(UrlSchemeError.Disallowed("javascript", UrlScheme.AllowedSchemes)),
    )
  }

  test("file: URL is rejected as Disallowed") {
    assertEquals(
      UrlScheme.validate("file:///etc/passwd"),
      Left(UrlSchemeError.Disallowed("file", UrlScheme.AllowedSchemes)),
    )
  }

  test("data: URL is rejected as Disallowed") {
    assertEquals(
      UrlScheme.validate("data:text/html,<script>alert(1)</script>"),
      Left(UrlSchemeError.Disallowed("data", UrlScheme.AllowedSchemes)),
    )
  }

  test("empty string is rejected as Malformed") {
    assertEquals(UrlScheme.validate(""), Left(UrlSchemeError.Malformed("")))
  }

  test("string without colon is rejected as Malformed") {
    assertEquals(UrlScheme.validate("no-colon-here"), Left(UrlSchemeError.Malformed("no-colon-here")))
  }

  test("string starting with colon is rejected as Malformed") {
    assertEquals(UrlScheme.validate(":whatever"), Left(UrlSchemeError.Malformed(":whatever")))
  }

  test("any alphaNum string prefixed with 'javascript:' is rejected as Disallowed (example: 1-char tail)") {
    assertEquals(
      UrlScheme.validate("javascript:a"),
      Left(UrlSchemeError.Disallowed("javascript", UrlScheme.AllowedSchemes)),
    )
  }

  test("any alphaNum string prefixed with 'javascript:' is rejected as Disallowed (example: 30-char tail)") {
    assertEquals(
      UrlScheme.validate("javascript:abcdefghijklmnopqrstuvwxyz1234"),
      Left(UrlSchemeError.Disallowed("javascript", UrlScheme.AllowedSchemes)),
    )
  }

  test("any alphaNum string prefixed with 'https://' is accepted (example: 1-char tail)") {
    val input = "https://a"
    assertEquals(UrlScheme.validate(input), Right(input))
  }

  test("any alphaNum string prefixed with 'https://' is accepted (example: 30-char tail)") {
    val input = "https://abcdefghijklmnopqrstuvwxyz1234"
    assertEquals(UrlScheme.validate(input), Right(input))
  }

  test("Malformed message includes the input") {
    val msg = (UrlSchemeError.Malformed("bad"): UrlSchemeError).message
    assert(msg.contains("bad"), s"msg `$msg` should contain the input \"bad\"")
  }

  test("Disallowed message includes the scheme and allowed list") {
    val msg = (UrlSchemeError.Disallowed("file", List("https")): UrlSchemeError).message
    assert(msg.contains("file"), s"msg `$msg` should contain the scheme \"file\"")
    assert(msg.contains("https"), s"msg `$msg` should contain the allowed scheme \"https\"")
  }
}
