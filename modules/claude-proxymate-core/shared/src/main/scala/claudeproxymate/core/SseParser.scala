package claudeproxymate.core

import cats.syntax.all.*
import io.circe.{Json, JsonObject}
import io.circe.parser.{parse => parseJson}

/** Reconstructs an Anthropic message object from an SSE (Server-Sent Events) stream.
  *
  * Ports `parseSseStream` from `main.js:34-60`.
  */
object SseParser {

  private val linePattern = "^(event|data):\\s?(.*)".r

  /** Stream parsing state: pending data line + accumulated message object. */
  final private case class State(currentData: Option[String], msg: Option[JsonObject])

  private object State {
    val empty: State = State(none[String], none[JsonObject])
  }

  def parseSseStream(text: String): Option[Json] = {
    try {
      val lines = text.split('\n').toList.map(_.replaceAll("\\r$", ""))

      val processed = lines.foldLeft(State.empty) { (state, line) =>
        line match {
          case linePattern(key, value) =>
            val trimmed = value.replaceAll("\\s+$", "")
            key match {
              case "data" => state.copy(currentData = trimmed.some)
              case _ => state
            }
          case "" =>
            state.currentData match {
              case Some(data) => State(currentData = none[String], msg = processEvent(data, state.msg))
              case None => state
            }
          case _ => state
        }
      }

      // Process any trailing data without a final blank line
      val finalState = processed.currentData match {
        case Some(data) => processed.copy(msg = processEvent(data, processed.msg))
        case None => processed
      }

      finalState.msg.map(Json.fromJsonObject)
    } catch {
      case _: Throwable => none[Json]
    }
  }

  private def processEvent(data: String, msg: Option[JsonObject]): Option[JsonObject] = {
    try {
      parseJson(data).toOption.flatMap(_.asObject) match {
        case None => msg
        case Some(d) =>
          val eventType = d("type").flatMap(_.asString).getOrElse("")
          eventType match {
            case "message_start" => handleMessageStart(d)
            case "content_block_start" => handleContentBlockStart(d, msg)
            case "content_block_delta" => handleContentBlockDelta(d, msg)
            case "message_delta" => handleMessageDelta(d, msg)
            case _ => msg
          }
      }
    } catch {
      case _: Throwable => msg // silently ignore malformed events
    }
  }

  private def handleMessageStart(d: JsonObject): Option[JsonObject] =
    d("message")
      .flatMap(_.asObject)
      .map(_.add("_streaming", Json.fromBoolean(true)))

  private def handleContentBlockStart(d: JsonObject, msg: Option[JsonObject]): Option[JsonObject] =
    for {
      m     <- msg
      index <- d("index").flatMap(_.asNumber).flatMap(_.toInt)
      cb    <- d("content_block")
    } yield {
      val content = m("content").flatMap(_.asArray).getOrElse(Vector.empty)
      val padded  = content.padTo(index + 1, Json.Null)
      val updated = padded.updated(index, cb)
      m.add("content", Json.fromValues(updated))
    }

  private def handleContentBlockDelta(d: JsonObject, msg: Option[JsonObject]): Option[JsonObject] =
    (for {
      m     <- msg
      index <- d("index").flatMap(_.asNumber).flatMap(_.toInt)
      delta <- d("delta").flatMap(_.asObject)
    } yield {
      val content   = m("content").flatMap(_.asArray).getOrElse(Vector.empty)
      val deltaType = delta("type").flatMap(_.asString).getOrElse("")
      if (index < content.size) {
        val block        = content(index).asObject.getOrElse(JsonObject.empty)
        val updatedBlock = applyDelta(block, delta, deltaType)
        val updated      = content.updated(index, Json.fromJsonObject(updatedBlock))
        m.add("content", Json.fromValues(updated))
      } else m
    }).orElse(msg)

  private def applyDelta(block: JsonObject, delta: JsonObject, deltaType: String): JsonObject =
    deltaType match {
      case "text_delta" =>
        val existing = block("text").flatMap(_.asString).getOrElse("")
        val addition = delta("text").flatMap(_.asString).getOrElse("")
        block.add("text", Json.fromString(existing + addition))
      case "thinking_delta" =>
        val existing = block("thinking").flatMap(_.asString).getOrElse("")
        val addition = delta("thinking").flatMap(_.asString).getOrElse("")
        block.add("thinking", Json.fromString(existing + addition))
      case _ => block
    }

  private def handleMessageDelta(d: JsonObject, msg: Option[JsonObject]): Option[JsonObject] =
    msg.map { m =>
      val withDelta = d("delta").flatMap(_.asObject) match {
        case Some(delta) => delta.toIterable.foldLeft(m) { case (acc, (k, v)) => acc.add(k, v) }
        case None => m
      }
      d("usage").flatMap(_.asObject) match {
        case Some(usage) =>
          val existingUsage = withDelta("usage").flatMap(_.asObject).getOrElse(JsonObject.empty)
          val merged        = usage.toIterable.foldLeft(existingUsage) { case (acc, (k, v)) => acc.add(k, v) }
          withDelta.add("usage", Json.fromJsonObject(merged))
        case None => withDelta
      }
    }
}
