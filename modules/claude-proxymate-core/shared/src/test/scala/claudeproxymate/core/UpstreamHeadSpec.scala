package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

import java.nio.charset.StandardCharsets

object UpstreamHeadSpec extends Properties {

  override def tests: List[Test] = List(
    property("a complete head with a body parses to the status, the headers and the body start", testParsesHead),
    property("1xx blocks before the final block are skipped", testSkipsInformational),
    property("any proper prefix of the head gives NeedMore", testPrefixNeedsMore),
    property("a head longer than maxBytes fails with TooLarge", testTooLarge),
    property("bytes without a block end that are longer than maxBytes fail with TooLarge", testNoEndTooLarge),
    example("HTTP/2 status line with no reason phrase", testHttp2),
    example("HTTP/1.1 status line with a reason phrase", testHttp11),
    example("a status line that is not HTTP fails with BadStatusLine", testBadStatusLine),
    example("a two-digit status fails with BadStatusLine", testShortStatus),
    example("a header line without a colon is skipped, names and values are trimmed", testHeaderLines),
    example("an empty input gives NeedMore", testEmpty),
    example("a header value can contain a colon", testValueWithColon),
  )

  private def bytes(s: String): Array[Byte] = s.getBytes(StandardCharsets.ISO_8859_1)

  private def genHeader: Gen[UpstreamHeader] =
    for {
      name  <- Gen.string(Gen.alphaNum, Range.linear(1, 20))
      value <- Gen.string(Gen.alphaNum, Range.linear(0, 40))
    } yield UpstreamHeader(name, value)

  private def genHeaders: Gen[List[UpstreamHeader]] =
    genHeader.list(Range.linear(0, 10))

  private def genStatusLine(code: Int): Gen[String] =
    Gen.element1(s"HTTP/1.1 $code OK", s"HTTP/1.0 $code Reason Phrase", s"HTTP/2 $code", s"HTTP/3 $code")

  private def genBody: Gen[String] =
    Gen.string(Gen.element1('a', 'b', '\r', '\n', ':', ' '), Range.linear(0, 200))

  private def render(statusLine: String, headers: List[UpstreamHeader]): String =
    statusLine + "\r\n" + headers.map(h => s"${h.name}: ${h.value}\r\n").mkString + "\r\n"

  def testParsesHead: Property =
    for {
      code       <- Gen.int(Range.linear(200, 599)).log("code")
      statusLine <- genStatusLine(code).log("statusLine")
      headers    <- genHeaders.log("headers")
      body       <- genBody.log("body")
    } yield {
      val head = render(statusLine, headers)
      UpstreamHead.parse(bytes(head + body), 64 * 1024) ==== UpstreamHeadParse.Parsed(
        UpstreamHead(code, headers),
        head.length,
      )
    }

  def testSkipsInformational: Property =
    for {
      infoCodes <- Gen.int(Range.linear(100, 199)).list(Range.linear(1, 3)).log("infoCodes")
      code      <- Gen.int(Range.linear(200, 599)).log("code")
      headers   <- genHeaders.log("headers")
      body      <- genBody.log("body")
    } yield {
      val info = infoCodes.map(c => render(s"HTTP/1.1 $c Info", List(UpstreamHeader("X-Info", c.toString)))).mkString
      val head = info + render(s"HTTP/1.1 $code OK", headers)
      UpstreamHead.parse(bytes(head + body), 64 * 1024) ==== UpstreamHeadParse.Parsed(
        UpstreamHead(code, headers),
        head.length,
      )
    }

  def testPrefixNeedsMore: Property =
    for {
      withInfo <- Gen.boolean.log("withInfo")
      code     <- Gen.int(Range.linear(200, 599)).log("code")
      headers  <- genHeaders.log("headers")
      cutSeed  <- Gen.int(Range.linear(0, 100000)).log("cutSeed")
    } yield {
      val head = (if (withInfo) render("HTTP/1.1 100 Continue", Nil) else "") + render(s"HTTP/2 $code", headers)
      val cut  = cutSeed % head.length
      UpstreamHead.parse(bytes(head.substring(0, cut)), 64 * 1024) ==== UpstreamHeadParse.NeedMore
    }

  def testTooLarge: Property =
    for {
      headers <- genHeaders.log("headers")
      maxSeed <- Gen.int(Range.linear(0, 100000)).log("maxSeed")
    } yield {
      val head = render("HTTP/1.1 200 OK", headers)
      val max  = maxSeed % head.length
      UpstreamHead.parse(bytes(head + "body"), max) ==== UpstreamHeadParse.Failed(UpstreamHeadError.TooLarge(max))
    }

  def testNoEndTooLarge: Property =
    for {
      max   <- Gen.int(Range.linear(0, 100)).log("max")
      extra <- Gen.int(Range.linear(1, 50)).log("extra")
    } yield {
      val text = "HTTP/1.1 200 OK\r\nX: " + ("a" * (max + extra))
      UpstreamHead.parse(bytes(text), max) ==== UpstreamHeadParse.Failed(UpstreamHeadError.TooLarge(max))
    }

  def testHttp2: Result = {
    val head = "HTTP/2 200\r\ncontent-type: text/event-stream\r\n\r\n"
    UpstreamHead.parse(bytes(head + "event: ping\n\n"), 1024) ==== UpstreamHeadParse.Parsed(
      UpstreamHead(200, List(UpstreamHeader("content-type", "text/event-stream"))),
      head.length,
    )
  }

  def testHttp11: Result = {
    val head = "HTTP/1.1 404 Not Found\r\nContent-Length: 2\r\n\r\n"
    UpstreamHead.parse(bytes(head + "{}"), 1024) ==== UpstreamHeadParse.Parsed(
      UpstreamHead(404, List(UpstreamHeader("Content-Length", "2"))),
      head.length,
    )
  }

  def testBadStatusLine: Result =
    UpstreamHead.parse(bytes("FOO 200 OK\r\nA: b\r\n\r\n"), 1024) ==== UpstreamHeadParse.Failed(
      UpstreamHeadError.BadStatusLine("FOO 200 OK")
    )

  def testShortStatus: Result =
    UpstreamHead.parse(bytes("HTTP/1.1 20\r\n\r\n"), 1024) ==== UpstreamHeadParse.Failed(
      UpstreamHeadError.BadStatusLine("HTTP/1.1 20")
    )

  def testHeaderLines: Result = {
    val head = "HTTP/1.1 200 OK\r\nno colon here\r\n  Name  :  value  \r\n: no name\r\n\r\n"
    UpstreamHead.parse(bytes(head), 1024) ====
      UpstreamHeadParse.Parsed(UpstreamHead(200, List(UpstreamHeader("Name", "value"))), head.length)
  }

  def testEmpty: Result =
    UpstreamHead.parse(Array.emptyByteArray, 1024) ==== UpstreamHeadParse.NeedMore

  def testValueWithColon: Result =
    UpstreamHead.header("Location: https://example.com:8443/a") ==== Some(
      UpstreamHeader("Location", "https://example.com:8443/a")
    )
}
