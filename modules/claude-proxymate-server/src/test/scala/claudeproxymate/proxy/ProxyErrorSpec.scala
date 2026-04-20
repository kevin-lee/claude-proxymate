package claudeproxymate.proxy

import claudeproxymate.core.ProxyError
import io.circe.parser.{parse => parseJson}
import io.circe.syntax.*
import org.http4s.Status

class ProxyErrorSpec extends munit.FunSuite {

  private val allCases: List[ProxyError] = List(
    ProxyError.Upstream("u"),
    ProxyError.CurlInitFailed,
    ProxyError.CurlPerformFailed(7),
    ProxyError.TmpFileFailed,
    ProxyError.MalformedUpstreamResponse,
    ProxyError.CurlException("c"),
  )

  test("Upstream encodes plain message as {\"error\":\"<msg>\"}") {
    val out = (ProxyError.Upstream("boom"): ProxyError).asJson.noSpaces
    assertEquals(out, """{"error":"boom"}""")
  }

  test("Upstream with quote in message escapes the quote") {
    val msg    = "he said \"hi\""
    val out    = (ProxyError.Upstream(msg): ProxyError).asJson.noSpaces
    val parsed = parseJson(out).flatMap(_.hcursor.get[String]("error"))
    assertEquals(parsed, Right(msg))
  }

  test("Upstream with newline in message escapes the newline") {
    val msg    = "line1\nline2"
    val out    = (ProxyError.Upstream(msg): ProxyError).asJson.noSpaces
    val parsed = parseJson(out).flatMap(_.hcursor.get[String]("error"))
    assertEquals(parsed, Right(msg))
  }

  test("Upstream with backslash in message escapes the backslash") {
    val msg    = "path\\to\\file"
    val out    = (ProxyError.Upstream(msg): ProxyError).asJson.noSpaces
    val parsed = parseJson(out).flatMap(_.hcursor.get[String]("error"))
    assertEquals(parsed, Right(msg))
  }

  test("CurlInitFailed has fixed message") {
    assertEquals((ProxyError.CurlInitFailed: ProxyError).message, "curl_easy_init() returned null")
  }

  test("CurlPerformFailed message includes the code") {
    assertEquals((ProxyError.CurlPerformFailed(42): ProxyError).message, "curl_easy_perform failed with code 42")
  }

  test("TmpFileFailed has fixed message") {
    assertEquals((ProxyError.TmpFileFailed: ProxyError).message, "tmpfile() returned null")
  }

  test("MalformedUpstreamResponse has fixed message") {
    assertEquals((ProxyError.MalformedUpstreamResponse: ProxyError).message, "malformed response from upstream")
  }

  test("CurlException carries the message verbatim") {
    assertEquals((ProxyError.CurlException("boom"): ProxyError).message, "boom")
  }

  test("upstream(throwable) uses getMessage when non-null") {
    val t  = new RuntimeException("msg")
    val pe = ProxyError.upstream(t)
    assertEquals(pe.message, "msg")
  }

  test("upstream(throwable) falls back to class name when getMessage is null") {
    val t  = new RuntimeException(null: String)
    val pe = ProxyError.upstream(t)
    assertEquals(pe.message, "RuntimeException")
  }

  test("curlException(throwable) falls back to class name when getMessage is null") {
    val t  = new RuntimeException(null: String)
    val pe = ProxyError.curlException(t)
    assertEquals(pe.message, "RuntimeException")
  }

  test("all cases map to status BadGateway via ProxyErrorHttp4s") {
    val mismatches = allCases.filter(pe => ProxyErrorHttp4s.status(pe) != Status.BadGateway)
    assertEquals(mismatches, List.empty[ProxyError])
  }

  test("all cases produce encodable JSON round-tripping the message") {
    val failures = allCases.flatMap { pe =>
      val out    = (pe: ProxyError).asJson.noSpaces
      val parsed = parseJson(out).flatMap(_.hcursor.get[String]("error"))
      if (parsed.contains(pe.message)) None
      else Some(s"case=$pe encoded=$out parsed=$parsed")
    }
    assertEquals(failures, List.empty[String])
  }
}
