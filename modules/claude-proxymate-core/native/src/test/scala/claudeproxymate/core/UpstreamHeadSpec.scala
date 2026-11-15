package claudeproxymate.core

import java.nio.charset.StandardCharsets

/* munit port of the shared hedgehog UpstreamHeadSpec for Scala Native
 * (the hedgehog runner hangs on Native). Properties are ported as
 * deterministic representative examples. */
class UpstreamHeadSpec extends munit.FunSuite {

  private def bytes(s: String): Array[Byte] = s.getBytes(StandardCharsets.ISO_8859_1)

  private val headers: List[UpstreamHeader] =
    List(UpstreamHeader("content-type", "text/event-stream"), UpstreamHeader("request-id", "req_1"))

  private def render(statusLine: String, hs: List[UpstreamHeader]): String =
    statusLine + "\r\n" + hs.map(h => s"${h.name}: ${h.value}\r\n").mkString + "\r\n"

  test("a complete head with a body parses to the status, the headers and the body start") {
    List("HTTP/1.1 200 OK", "HTTP/1.0 200 Reason Phrase", "HTTP/2 200", "HTTP/3 200").foreach { statusLine =>
      val head = render(statusLine, headers)
      assertEquals(
        UpstreamHead.parse(bytes(head + "event: ping\r\n\r\ndata: x\n\n"), 64 * 1024),
        UpstreamHeadParse.Parsed(UpstreamHead(200, headers), head.length),
      )
    }
  }

  test("1xx blocks before the final block are skipped") {
    val head =
      render("HTTP/1.1 100 Continue", Nil) + render("HTTP/1.1 103 Early Hints", List(UpstreamHeader("Link", "</a>"))) +
        render("HTTP/1.1 529 Overloaded", headers)
    assertEquals(
      UpstreamHead.parse(bytes(head + "{}"), 64 * 1024),
      UpstreamHeadParse.Parsed(UpstreamHead(529, headers), head.length),
    )
  }

  test("any proper prefix of the head gives NeedMore") {
    val head = render("HTTP/1.1 100 Continue", Nil) + render("HTTP/2 200", headers)
    (0 until head.length).foreach { cut =>
      assertEquals(UpstreamHead.parse(bytes(head.substring(0, cut)), 64 * 1024), UpstreamHeadParse.NeedMore)
    }
  }

  test("a head longer than maxBytes fails with TooLarge") {
    val head = render("HTTP/1.1 200 OK", headers)
    List(0, 1, head.length / 2, head.length - 1).foreach { max =>
      assertEquals(
        UpstreamHead.parse(bytes(head + "body"), max),
        UpstreamHeadParse.Failed(UpstreamHeadError.TooLarge(max)),
      )
    }
  }

  test("bytes without a block end that are longer than maxBytes fail with TooLarge") {
    assertEquals(
      UpstreamHead.parse(bytes("HTTP/1.1 200 OK\r\nX: " + ("a" * 100)), 50),
      UpstreamHeadParse.Failed(UpstreamHeadError.TooLarge(50)),
    )
  }

  test("a status line that is not HTTP fails with BadStatusLine") {
    assertEquals(
      UpstreamHead.parse(bytes("FOO 200 OK\r\nA: b\r\n\r\n"), 1024),
      UpstreamHeadParse.Failed(UpstreamHeadError.BadStatusLine("FOO 200 OK")),
    )
  }

  test("a two-digit status fails with BadStatusLine") {
    assertEquals(
      UpstreamHead.parse(bytes("HTTP/1.1 20\r\n\r\n"), 1024),
      UpstreamHeadParse.Failed(UpstreamHeadError.BadStatusLine("HTTP/1.1 20")),
    )
  }

  test("a header line without a colon is skipped, names and values are trimmed") {
    val head = "HTTP/1.1 200 OK\r\nno colon here\r\n  Name  :  value  \r\n: no name\r\n\r\n"
    assertEquals(
      UpstreamHead.parse(bytes(head), 1024),
      UpstreamHeadParse.Parsed(UpstreamHead(200, List(UpstreamHeader("Name", "value"))), head.length),
    )
  }

  test("an empty input gives NeedMore") {
    assertEquals(UpstreamHead.parse(Array.emptyByteArray, 1024), UpstreamHeadParse.NeedMore)
  }

  test("a header value can contain a colon") {
    assertEquals(
      UpstreamHead.header("Location: https://example.com:8443/a"),
      Some(UpstreamHeader("Location", "https://example.com:8443/a")),
    )
  }
}
