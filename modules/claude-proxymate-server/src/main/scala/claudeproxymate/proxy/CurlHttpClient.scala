package claudeproxymate.proxy

import cats.effect.IO
import cats.syntax.all.*
import claudeproxymate.core.ProxyError
import org.http4s.{Headers, Method, Request, Response, Status, Uri}
import org.http4s.client.Client
import org.typelevel.ci.CIString
import fs2.Stream
import scala.scalanative.unsafe._
import scala.scalanative.unsigned.UnsignedRichInt
import scala.scalanative.libc.stdio

/** An http4s Client[IO] backed by libcurl's easy (synchronous) API.
  *
  * Each request performs a blocking curl_easy_perform wrapped in IO.blocking.
  * Works under the standard IOApp runtime (no CurlApp/CurlRuntime needed).
  *
  * Note: On single-threaded Scala Native, IO.blocking blocks the event loop
  * during each request. This is acceptable for a dev proxy tool handling
  * sequential requests from Claude Code.
  */
object CurlHttpClient {

  def client: Client[IO] = Client[IO] { (req: Request[IO]) =>
    cats.effect.Resource.eval(performRequest(req))
  }

  private def performRequest(req: Request[IO]): IO[Response[IO]] = {
    for {
      bodyBytes <- req.body.compile.to(Array)
      resp      <- IO.blocking(curlPerform(req.method, req.uri, req.headers, bodyBytes))
    } yield resp
  }

  private def curlPerform(
    method: Method,
    uri: Uri,
    headers: Headers,
    body: Array[Byte],
  ): Response[IO] = {
    Zone {
      val curl = LibCurl.curl_easy_init()
      if (curl == null) {
        ProxyErrorHttp4s.asResponse(ProxyError.CurlInitFailed)
      } else {
        val emptyList: LibCurl.SList = null.asInstanceOf[LibCurl.SList]
        val forwardedHeaders         = headers.headers.filterNot { h =>
          val name = h.name.toString.toLowerCase
          name === "host" || name === "accept-encoding"
        }
        val builtList                = forwardedHeaders.foldLeft(emptyList) { (acc, h) =>
          LibCurl.curl_slist_append(acc, toCString(s"${h.name}: ${h.value}"))
        }
        val withHost                 = LibCurl.curl_slist_append(builtList, c"Host: api.anthropic.com")
        val finalList                = LibCurl.curl_slist_append(withHost, c"Accept-Encoding:")

        try {
          LibCurl.curl_easy_setopt(curl, CurlOpt.Url, toCString(uri.renderString)): Unit
          LibCurl.curl_easy_setopt(curl, CurlOpt.CustomRequest, toCString(method.name)): Unit
          LibCurl.curl_easy_setopt(curl, CurlOpt.Timeout, 300L): Unit
          LibCurl.curl_easy_setopt(curl, CurlOpt.Header, 1L): Unit // include response headers in output
          LibCurl.curl_easy_setopt(curl, CurlOpt.HttpHeader, finalList): Unit

          // Request body
          if (body.nonEmpty) {
            val bodyPtr = alloc[Byte](body.length.toUSize)
            (0 until body.length).foreach { i =>
              !(bodyPtr + i) = body(i)
            }
            LibCurl.curl_easy_setopt(curl, CurlOpt.PostFields, bodyPtr): Unit
            LibCurl.curl_easy_setopt(curl, CurlOpt.PostFieldSize, body.length.toLong): Unit
          } else ()

          performWithTmpFile(curl)
        } catch {
          case e: Throwable =>
            ProxyErrorHttp4s.asResponse(ProxyError.curlException(e))
        } finally {
          if (finalList != null) LibCurl.curl_slist_free_all(finalList) else ()
          LibCurl.curl_easy_cleanup(curl)
        }
      }
    }
  }

  private def performWithTmpFile(curl: LibCurl.CURL)(implicit z: Zone): Response[IO] = {
    val tmpFile = stdio.tmpfile()
    if (tmpFile == null) {
      ProxyErrorHttp4s.asResponse(ProxyError.TmpFileFailed)
    } else {
      try {
        LibCurl.curl_easy_setopt(curl, CurlOpt.WriteData, tmpFile): Unit

        // Perform the request (blocks)
        val code = LibCurl.curl_easy_perform(curl)
        if (code =!= 0) {
          ProxyErrorHttp4s.asResponse(ProxyError.CurlPerformFailed(code))
        } else {
          // Read response from temp file
          val fileSize = stdio.ftell(tmpFile).toInt
          stdio.rewind(tmpFile)

          val buf = alloc[Byte](fileSize.toUSize)
          stdio.fread(buf, 1.toUSize, fileSize.toUSize, tmpFile): Unit

          val respData = new Array[Byte](fileSize)
          (0 until fileSize).foreach { j =>
            respData(j) = !(buf + j)
          }

          /* Ask libcurl for the parsed status code rather than
           * scraping it from the raw status line. libcurl parses the
           * status line internally and is robust across the
           * HTTP/1.0/1.1/2/3 shape variation (`HTTP/2 200` with no
           * reason phrase vs `HTTP/1.1 200 OK`) that made a manual
           * split brittle. Read while `curl` is still live (before
           * `curl_easy_cleanup` in the caller's finally). A non-zero
           * getinfo result or a 0 code falls through to 500 in
           * `parseResponse` via `Status.fromInt(...).getOrElse(...)`. */
          val codePtr: Ptr[CLong] = alloc[CLong]()
          val statusCode: Int     =
            if (LibCurl.curl_easy_getinfo(curl, CurlInfo.ResponseCode, codePtr) === 0) (!codePtr).toInt
            else 0

          parseResponse(respData, statusCode)
        }
      } finally {
        stdio.fclose(tmpFile): Unit
      }
    }
  }

  /** Parse a curl `-i`-style response buffer (status line + headers +
    * body) into an http4s `Response`.
    *
    * `statusCode` is supplied by the caller from
    * `CURLINFO_RESPONSE_CODE` (libcurl's own parse), so the raw
    * status line (`lines.head`) is skipped here — only the header
    * lines are parsed. An unknown / 0 code maps to 500 via
    * `Status.fromInt(...).getOrElse(...)`.
    */
  private def parseResponse(data: Array[Byte], statusCode: Int): Response[IO] = {
    val str    = new String(data, "ISO-8859-1")
    val sepIdx = str.indexOf("\r\n\r\n")
    if (sepIdx < 0) {
      ProxyErrorHttp4s.asResponse(ProxyError.MalformedUpstreamResponse)
    } else {
      val headerPart = str.substring(0, sepIdx)
      val bodyStart  = sepIdx + 4
      val bodyBytes  = java.util.Arrays.copyOfRange(data, bodyStart, data.length)

      val status = Status.fromInt(statusCode).getOrElse(Status.InternalServerError)

      // `lines.tail` skips the status line; parse only header lines.
      val lines       = headerPart.split("\r\n")
      val respHeaders = lines.tail.flatMap { line =>
        val colonIdx = line.indexOf(':')
        Option.when(colonIdx > 0) {
          val name  = line.substring(0, colonIdx).trim
          val value = line.substring(colonIdx + 1).trim
          org.http4s.Header.Raw(CIString(name), value)
        }
      }

      Response[IO](status)
        .withHeaders(Headers(respHeaders.toList))
        .withBodyStream(Stream.emits(bodyBytes))
    }
  }
}
