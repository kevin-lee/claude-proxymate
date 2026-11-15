package claudeproxymate.core

import cats.*
import cats.derived.strict.*
import cats.syntax.all.*

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.util.matching.Regex

/** One header line of an upstream response. */
final case class UpstreamHeader(name: String, value: String) derives CanEqual, Eq, Show

/** The status code and the headers of the final header block of an upstream
  * response.
  */
final case class UpstreamHead(status: Int, headers: List[UpstreamHeader]) derives CanEqual, Eq, Show

/** Why the start of an upstream response cannot be used. */
enum UpstreamHeadError derives CanEqual, Eq, Show {

  /** The first line of a header block is not an HTTP status line. */
  case BadStatusLine(line: String)

  /** The header blocks are longer than `maxBytes` bytes. */
  case TooLarge(maxBytes: Int)
}

object UpstreamHeadError {
  def badStatusLine(line: String): UpstreamHeadError = BadStatusLine(line)
  def tooLarge(maxBytes: Int): UpstreamHeadError     = TooLarge(maxBytes)
}

/** The result of [[UpstreamHead.parse]]. */
enum UpstreamHeadParse derives CanEqual, Eq, Show {

  /** The final header block is not complete yet. Read more bytes, then parse
    * again from the start.
    */
  case NeedMore

  /** The final header block. The body starts at byte `bodyStart`. */
  case Parsed(head: UpstreamHead, bodyStart: Int)

  case Failed(error: UpstreamHeadError)
}

object UpstreamHeadParse {
  def needMore: UpstreamHeadParse                                   = NeedMore
  def parsed(head: UpstreamHead, bodyStart: Int): UpstreamHeadParse = Parsed(head, bodyStart)
  def failed(error: UpstreamHeadError): UpstreamHeadParse           = Failed(error)
}

/** Parses the start of a raw HTTP response, as libcurl writes it with
  * `CURLOPT_HEADER` set to 1: one or more header blocks, then the body.
  *
  * libcurl writes each header block as received, each line ending with
  * `\r\n` and the block ending with an empty line. Over HTTP/2 the status line
  * is `HTTP/2 200`, with no reason phrase. A `1xx` block (for example
  * `100 Continue` or `103 Early Hints`) comes before the final block, and it is
  * skipped.
  */
object UpstreamHead {

  private val BlockEnd: String = "\r\n\r\n"

  /* HTTP/1.0, HTTP/1.1, HTTP/2 and HTTP/3, a three-digit code, then an
   * optional reason phrase. */
  private val StatusLine: Regex = """HTTP/\d(?:\.\d)? (\d{3})(?: .*)?""".r

  /** Parse the header blocks at the start of `bytes`.
    *
    * `maxBytes` limits the length of all header blocks together, so a peer
    * that never ends its headers cannot make the caller keep reading into
    * memory.
    */
  def parse(bytes: Array[Byte], maxBytes: Int): UpstreamHeadParse = {
    /* ISO-8859-1 maps each byte to one char, so char offsets are byte offsets. */
    val text = new String(bytes, StandardCharsets.ISO_8859_1)

    @tailrec
    def loop(blockStart: Int): UpstreamHeadParse = {
      val blockEnd = text.indexOf(BlockEnd, blockStart)
      if (blockEnd < 0) {
        if (text.length > maxBytes) UpstreamHeadParse.failed(UpstreamHeadError.tooLarge(maxBytes))
        else UpstreamHeadParse.needMore
      } else {
        val bodyStart = blockEnd + BlockEnd.length
        if (bodyStart > maxBytes) {
          UpstreamHeadParse.failed(UpstreamHeadError.tooLarge(maxBytes))
        } else {
          text.substring(blockStart, blockEnd).split("\r\n", -1).toList match {
            case statusLine :: headerLines =>
              statusCode(statusLine) match {
                case Some(code) if code >= 100 && code < 200 =>
                  loop(bodyStart)
                case Some(code) =>
                  UpstreamHeadParse.parsed(UpstreamHead(code, headerLines.flatMap(header)), bodyStart)
                case None =>
                  UpstreamHeadParse.failed(UpstreamHeadError.badStatusLine(statusLine))
              }
            case Nil =>
              UpstreamHeadParse.failed(UpstreamHeadError.badStatusLine(""))
          }
        }
      }
    }

    loop(0)
  }

  /** The status code of an HTTP status line, if it is one. */
  def statusCode(line: String): Option[Int] =
    line match {
      case StatusLine(code) => code.toIntOption
      case _ => none[Int]
    }

  /** A header line `name: value`. A line without a name is skipped. */
  def header(line: String): Option[UpstreamHeader] = {
    val colon = line.indexOf(':')
    Option.when(colon > 0)(UpstreamHeader(line.substring(0, colon).trim, line.substring(colon + 1).trim))
  }
}
