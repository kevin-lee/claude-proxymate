package claudeproxymate.proxy

import cats.effect.IO
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
    Zone { implicit z =>
      val curl = LibCurl.curl_easy_init()
      if (curl == null) {
        Response[IO](Status.BadGateway)
          .withEntity("""{"error":"curl_easy_init() returned null"}""")
      } else {
        val emptyList: LibCurl.SList = null.asInstanceOf[LibCurl.SList]
        val forwardedHeaders         = headers.headers.filterNot { h =>
          val name = h.name.toString.toLowerCase
          name == "host" || name == "accept-encoding"
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
            val bodyPtr = alloc[Byte](body.length.toULong)
            (0 until body.length).foreach { i =>
              !(bodyPtr + i) = body(i)
            }
            LibCurl.curl_easy_setopt(curl, CurlOpt.PostFields, bodyPtr): Unit
            LibCurl.curl_easy_setopt(curl, CurlOpt.PostFieldSize, body.length.toLong): Unit
          } else ()

          performWithTmpFile(curl)
        } catch {
          case e: Throwable =>
            Response[IO](Status.BadGateway)
              .withEntity(s"""{"error":"${e.getMessage}"}""")
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
      Response[IO](Status.BadGateway)
        .withEntity("""{"error":"tmpfile() returned null"}""")
    } else {
      try {
        LibCurl.curl_easy_setopt(curl, CurlOpt.WriteData, tmpFile): Unit

        // Perform the request (blocks)
        val code = LibCurl.curl_easy_perform(curl)
        if (code != 0) {
          Response[IO](Status.BadGateway)
            .withEntity(s"""{"error":"curl_easy_perform failed with code $code"}""")
        } else {
          // Read response from temp file
          val fileSize = stdio.ftell(tmpFile).toInt
          stdio.rewind(tmpFile)

          val buf = alloc[Byte](fileSize.toULong)
          stdio.fread(buf, 1.toULong, fileSize.toULong, tmpFile): Unit

          val respData = new Array[Byte](fileSize)
          (0 until fileSize).foreach { j =>
            respData(j) = !(buf + j)
          }

          parseResponse(respData)
        }
      } finally {
        stdio.fclose(tmpFile): Unit
      }
    }
  }

  /** Parse raw HTTP response (headers + body) from curl -i style output. */
  private def parseResponse(data: Array[Byte]): Response[IO] = {
    val str    = new String(data, "ISO-8859-1")
    val sepIdx = str.indexOf("\r\n\r\n")
    if (sepIdx < 0) {
      Response[IO](Status.BadGateway)
        .withEntity("""{"error":"malformed response from upstream"}""")
    } else {
      val headerPart = str.substring(0, sepIdx)
      val bodyStart  = sepIdx + 4
      val bodyBytes  = java.util.Arrays.copyOfRange(data, bodyStart, data.length)

      val lines = headerPart.split("\r\n")

      // Status line: "HTTP/2 200" or "HTTP/1.1 200 OK"
      val statusCode = lines.head.split("\\s+", 3)(1).toInt
      val status     = Status.fromInt(statusCode).getOrElse(Status.InternalServerError)

      // Response headers
      val respHeaders = lines.tail.flatMap { line =>
        val colonIdx = line.indexOf(':')
        if (colonIdx > 0) {
          val name  = line.substring(0, colonIdx).trim
          val value = line.substring(colonIdx + 1).trim
          Some(org.http4s.Header.Raw(CIString(name), value))
        } else {
          None
        }
      }

      Response[IO](status)
        .withHeaders(Headers(respHeaders.toList))
        .withBodyStream(Stream.emits(bodyBytes))
    }
  }
}
