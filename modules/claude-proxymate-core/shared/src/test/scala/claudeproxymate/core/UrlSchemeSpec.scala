package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object UrlSchemeSpec extends Properties {

  override def tests: List[Test] = List(
    example("https URL is accepted", testHttpsAccepted),
    example("HTTPS URL (uppercase) is accepted (scheme case-insensitive)", testHttpsUppercaseAccepted),
    example("http URL is rejected as Disallowed", testHttpRejected),
    example("javascript: URL is rejected as Disallowed", testJavascriptRejected),
    example("file: URL is rejected as Disallowed", testFileRejected),
    example("data: URL is rejected as Disallowed", testDataRejected),
    example("empty string is rejected as Malformed", testEmptyMalformed),
    example("string without colon is rejected as Malformed", testNoColonMalformed),
    example("string starting with colon is rejected as Malformed", testLeadingColonMalformed),
    property("any alphaNum string prefixed with 'javascript:' is rejected as Disallowed", testJavascriptPrefixRejected),
    property("any alphaNum string prefixed with 'https://' is accepted", testHttpsPrefixAccepted),
    example("Malformed message includes the input", testMalformedMessage),
    example("Disallowed message includes the scheme and allowed list", testDisallowedMessage),
  )

  def testHttpsAccepted: Result = {
    val url = "https://docs.anthropic.com/en/docs/about-claude/models"
    UrlScheme.validate(url) ==== Right(url)
  }

  def testHttpsUppercaseAccepted: Result = {
    val url = "HTTPS://EXAMPLE.COM/path"
    UrlScheme.validate(url) ==== Right(url)
  }

  def testHttpRejected: Result =
    UrlScheme.validate("http://example.com") ==== Left(UrlSchemeError.Disallowed("http", UrlScheme.AllowedSchemes))

  def testJavascriptRejected: Result =
    UrlScheme.validate("javascript:alert(1)") ==== Left(
      UrlSchemeError.Disallowed("javascript", UrlScheme.AllowedSchemes)
    )

  def testFileRejected: Result =
    UrlScheme.validate("file:///etc/passwd") ==== Left(UrlSchemeError.Disallowed("file", UrlScheme.AllowedSchemes))

  def testDataRejected: Result =
    UrlScheme.validate("data:text/html,<script>alert(1)</script>") ==== Left(
      UrlSchemeError.Disallowed("data", UrlScheme.AllowedSchemes)
    )

  def testEmptyMalformed: Result =
    UrlScheme.validate("") ==== Left(UrlSchemeError.Malformed(""))

  def testNoColonMalformed: Result =
    UrlScheme.validate("no-colon-here") ==== Left(UrlSchemeError.Malformed("no-colon-here"))

  def testLeadingColonMalformed: Result =
    UrlScheme.validate(":whatever") ==== Left(UrlSchemeError.Malformed(":whatever"))

  def testJavascriptPrefixRejected: Property =
    for {
      tail <- Gen.string(Gen.alphaNum, Range.linear(1, 30)).log("tail")
    } yield {
      val input  = s"javascript:$tail"
      val result = UrlScheme.validate(input)
      (result ==== Left(UrlSchemeError.Disallowed("javascript", UrlScheme.AllowedSchemes)))
        .log(s"input=$input")
    }

  def testHttpsPrefixAccepted: Property =
    for {
      tail <- Gen.string(Gen.alphaNum, Range.linear(1, 30)).log("tail")
    } yield {
      val input  = s"https://$tail"
      val result = UrlScheme.validate(input)
      (result ==== Right(input)).log(s"input=$input")
    }

  def testMalformedMessage: Result = {
    val msg = (UrlSchemeError.Malformed("bad"): UrlSchemeError).message
    Result.diffNamed("msg contains input", msg, "bad")(_.contains(_))
  }

  def testDisallowedMessage: Result = {
    val msg = (UrlSchemeError.Disallowed("file", List("https")): UrlSchemeError).message
    Result.all(
      List(
        Result.diffNamed("msg contains scheme", msg, "file")(_.contains(_)),
        Result.diffNamed("msg contains allowed", msg, "https")(_.contains(_)),
      )
    )
  }
}
