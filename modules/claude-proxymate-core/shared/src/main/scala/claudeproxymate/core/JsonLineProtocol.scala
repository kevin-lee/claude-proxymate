package claudeproxymate.core

import io.circe.Printer
import io.circe.syntax.*
import io.circe.parser.{decode => circeDecode}

/** Encode/decode ProxyEvent as newline-delimited JSON (JSON Lines protocol).
  *
  * Used between the Scala Native proxy (stdout emitter) and the Electron main process (reader).
  */
object JsonLineProtocol {
  import codecs.given

  private val compactPrinter: Printer = Printer.noSpaces

  /** Serialize a ProxyEvent to a single-line JSON string (no trailing newline). */
  def encode(event: ProxyEvent): String =
    event.asJson.printWith(compactPrinter)

  /** Deserialize a JSON line back to a ProxyEvent. */
  def decode(line: String): Either[String, ProxyEvent] =
    circeDecode[ProxyEvent](line).left.map(_.getMessage)
}
