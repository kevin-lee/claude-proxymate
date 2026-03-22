package claudeproxymate.core

import io.circe.Json

/** Data sent to renderer on each intercepted request. */
final case class ProxyRequest(
  id: Long,
  ts: String,
  method: String,
  path: String,
  body: Option[Json],
)

/** Data sent to renderer on each intercepted response. */
final case class ProxyResponse(
  id: Long,
  status: Int,
  body: Either[String, Json],
  error: Option[String],
)

/** Events emitted by the proxy server (JSON lines on stdout). */
enum ProxyEvent {
  case RequestCaptured(request: ProxyRequest)
  case ResponseCaptured(response: ProxyResponse)
  case ProxyStarted(port: Int)
  case ProxyStopped
  case ProxyError(message: String)
}

// ----- Analysis models (used by core parsers) -----

/** A parsed CLAUDE.md / rule / memory section from system-reminder blocks. */
final case class Section(
  label: String,
  path: String,
  content: String,
  cls: String,
  scope: String,
)

final case class SlashCommand(
  name: String,
  tag: String,
  full: String,
)

final case class SkillUse(
  id: String,
  input: Json,
  result: Option[String],
)

final case class SubAgent(
  id: String,
  name: String,
  input: Json,
)

final case class McpTool(
  id: String,
  name: String,
  input: Json,
  result: Option[String],
)

/** All detected prompt augmentation mechanisms in an API request body. */
final case class Mechanisms(
  claudeMd: Option[String],
  outputStyle: Option[List[String]],
  slashCommands: List[SlashCommand],
  skills: List[SkillUse],
  subAgents: List[SubAgent],
  mcpTools: List[McpTool],
)

object Mechanisms {
  val empty: Mechanisms = Mechanisms(
    claudeMd = None,
    outputStyle = None,
    slashCommands = Nil,
    skills = Nil,
    subAgents = Nil,
    mcpTools = Nil,
  )
}
