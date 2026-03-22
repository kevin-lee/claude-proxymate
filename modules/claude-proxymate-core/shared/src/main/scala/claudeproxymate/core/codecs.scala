package claudeproxymate.core

import io.circe.*
import io.circe.syntax.*
import io.circe.generic.semiauto.*

object codecs {

  // ----- ProxyRequest -----
  given Encoder[ProxyRequest] = deriveEncoder[ProxyRequest]
  given Decoder[ProxyRequest] = deriveDecoder[ProxyRequest]

  // ----- ProxyResponse -----
  // body is Either[String, Json]: Left = raw truncated string, Right = parsed JSON
  given Encoder[ProxyResponse] = Encoder.instance { r =>
    val bodyJson = r.body match {
      case Right(json)  => json
      case Left(rawStr) => Json.fromString(rawStr)
    }
    Json.obj(
      "id"     -> r.id.asJson,
      "status" -> r.status.asJson,
      "body"   -> bodyJson,
      "error"  -> r.error.asJson,
    )
  }

  given Decoder[ProxyResponse] = Decoder.instance { c =>
    for {
      id       <- c.get[Long]("id")
      status   <- c.get[Int]("status")
      bodyJson <- c.get[Json]("body")
      error    <- c.get[Option[String]]("error")
    } yield {
      val body =
        if (bodyJson.isString) Left(bodyJson.asString.getOrElse(""))
        else Right(bodyJson)
      ProxyResponse(id, status, body, error)
    }
  }

  // ----- ProxyEvent -----
  // Discriminated with "type" field for the stdout JSON line protocol
  given Encoder[ProxyEvent] = Encoder.instance {
    case ProxyEvent.RequestCaptured(req) =>
      Json.obj("type" -> "request_captured".asJson, "request" -> req.asJson)
    case ProxyEvent.ResponseCaptured(resp) =>
      Json.obj("type" -> "response_captured".asJson, "response" -> resp.asJson)
    case ProxyEvent.ProxyStarted(port) =>
      Json.obj("type" -> "proxy_started".asJson, "port" -> port.asJson)
    case ProxyEvent.ProxyStopped =>
      Json.obj("type" -> "proxy_stopped".asJson)
    case ProxyEvent.ProxyError(message) =>
      Json.obj("type" -> "proxy_error".asJson, "message" -> message.asJson)
  }

  given Decoder[ProxyEvent] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "request_captured"  => c.get[ProxyRequest]("request").map(ProxyEvent.RequestCaptured(_))
      case "response_captured" => c.get[ProxyResponse]("response").map(ProxyEvent.ResponseCaptured(_))
      case "proxy_started"     => c.get[Int]("port").map(ProxyEvent.ProxyStarted(_))
      case "proxy_stopped"     => Right(ProxyEvent.ProxyStopped)
      case "proxy_error"       => c.get[String]("message").map(ProxyEvent.ProxyError(_))
      case other               => Left(DecodingFailure(s"Unknown ProxyEvent type: $other", c.history))
    }
  }

  // ----- Analysis models -----
  given Encoder[Section] = deriveEncoder[Section]
  given Decoder[Section] = deriveDecoder[Section]

  given Encoder[SlashCommand] = deriveEncoder[SlashCommand]
  given Decoder[SlashCommand] = deriveDecoder[SlashCommand]

  given Encoder[SkillUse] = deriveEncoder[SkillUse]
  given Decoder[SkillUse] = deriveDecoder[SkillUse]

  given Encoder[SubAgent] = deriveEncoder[SubAgent]
  given Decoder[SubAgent] = deriveDecoder[SubAgent]

  given Encoder[McpTool] = deriveEncoder[McpTool]
  given Decoder[McpTool] = deriveDecoder[McpTool]

  given Encoder[Mechanisms] = deriveEncoder[Mechanisms]
  given Decoder[Mechanisms] = deriveDecoder[Mechanisms]
}
