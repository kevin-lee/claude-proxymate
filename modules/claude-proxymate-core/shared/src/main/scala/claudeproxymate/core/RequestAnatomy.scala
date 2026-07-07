package claudeproxymate.core

import cats.syntax.all.*
import io.circe.Json

/** Per-input-segment size estimate (bytes + estimated tokens). */
final case class SegmentSize(label: String, bytes: Int, estTokens: Int)

/** Countable structural facts about a request body + its response. */
final case class StructureFacts(
  messageCount: Int,
  userTurns: Int,
  assistantTurns: Int,
  systemBlocks: Int,
  cachedSystemBlocks: Int,
  toolsDefined: Int,
  toolUseCount: Int,
  toolResultCount: Int,
  imageCount: Int,
  thinkingBlocks: Int,
  stream: Option[Boolean],
)

/** A quantified mechanism-inventory row. */
final case class InventoryRow(key: String, label: String, count: Int, estTokens: Int)

enum AnomalyKind {
  case Info, Warn, Security
}

/** A diagnostic line. `key` is an i18n key resolved by the renderer; `params`
  * carries interpolation values (e.g. `n`).
  */
final case class Anomaly(kind: AnomalyKind, key: String, params: Map[String, String] = Map.empty)

/** Full Request Anatomy result. `segments` is sorted desc by estTokens. */
final case class RequestAnatomy(
  model: Option[String],
  totalEstBytes: Int,
  totalEstTokens: Int,
  segments: List[SegmentSize],
  structure: StructureFacts,
  inventory: List[InventoryRow],
  anomalies: List[Anomaly],
)

/** Builds a [[RequestAnatomy]] from an Anthropic Messages API request body
  * plus minimal response signals. Pure: operates on `io.circe.Json` only.
  *
  * All token counts are estimates via [[estTokens]] (`ceil(bytes / 3.5)`),
  * matching the estimate path in the renderer's token pill. Labels for
  * segments / inventory / anomalies are i18n KEY strings; the renderer
  * resolves them.
  */
object RequestAnatomy {

  /* ── i18n key constants (resolved renderer-side) ── */
  private val SegSystem     = "anatomy.segSystem"
  private val SegTools      = "anatomy.segTools"
  private val SegThinking   = "anatomy.segThinking"
  private val SegClaudeMd   = "anatomy.segClaudeMd"
  private val SegMcpInstr   = "anatomy.segMcpInstr"
  private val SegSkills     = "anatomy.segSkills"
  private val SegHistory    = "anatomy.segHistory"
  private val SegLatestUser = "anatomy.segLatestUser"

  private val InvClaudeMd = "anatomy.invClaudeMd"
  private val InvMcp      = "anatomy.invMcp"
  private val InvSkills   = "anatomy.invSkills"
  private val InvSlash    = "anatomy.invSlash"
  private val InvSubAgent = "anatomy.invSubAgent"

  private val AnomToolsUnused = "anatomy.anomToolsUnused"
  private val AnomBasePrompt  = "anatomy.anomBasePrompt"
  private val AnomTruncated   = "anatomy.anomTruncated"
  private val AnomCredential  = "anatomy.anomCredential"
  private val AnomNoResponse  = "anatomy.noResponseNote"

  /* ── credential / injection heuristics ── */
  private val credentialPrefixes: List[String] =
    List("sk-ant-", "ghp_", "github_pat_", "gho_", "AKIA", "ASIA", "AIza", "sk_live_", "sk-", "Bearer ", "eyJ")

  private val echoInstructions: List[String] =
    List("echo back", "verbatim", "every token", "print the", "repeat the")

  private val McpInstrMarker   = "MCP Server Instructions"
  private val SkillsListMarker = "skills are available for use with the Skill tool"

  def estTokens(bytes: Int): Int =
    if (bytes <= 0) 0 else math.ceil(bytes / 3.5).toInt

  private def byteLen(s: String): Int = s.getBytes("UTF-8").length
  private def jsonBytes(j: Json): Int = byteLen(j.noSpaces)

  def analyze(body: Json, responseCaptured: Boolean, stopReason: Option[String]): RequestAnatomy = {
    val obj      = body.asObject
    val messages = obj.flatMap(_.apply("messages")).flatMap(_.asArray).getOrElse(Vector.empty)
    val systemJs = obj.flatMap(_.apply("system"))
    val toolsArr = obj.flatMap(_.apply("tools")).flatMap(_.asArray).getOrElse(Vector.empty)

    val model = obj.flatMap(_.apply("model")).flatMap(_.asString)

    val structure = buildStructure(body, messages, systemJs, toolsArr)
    val mech      = MechanismDetector.detectMechanisms(body)

    val msgTexts = collectUserTexts(messages)

    /* segment byte sizes */
    val systemBytes     = systemBytesOf(systemJs)
    val toolsBytes      = if (toolsArr.isEmpty) 0 else jsonBytes(Json.fromValues(toolsArr))
    val thinkingBytes   = thinkingSignatureBytes(messages)
    val claudeMdBytes   = mech.claudeMd.map(byteLen).getOrElse(0)
    val mcpInstrBytes   = matchingReminderBytes(msgTexts, McpInstrMarker)
    val skillsBytes     = matchingReminderBytes(msgTexts, SkillsListMarker)
    val latestUserBytes = latestUserTurnBytes(messages)
    val messagesBytes   = messages.map(jsonBytes).sum
    val historyBytes    = math.max(
      0,
      messagesBytes - claudeMdBytes - mcpInstrBytes - skillsBytes - thinkingBytes - latestUserBytes,
    )

    val rawSegments = List(
      SegmentSize(SegSystem, systemBytes, estTokens(systemBytes)),
      SegmentSize(SegTools, toolsBytes, estTokens(toolsBytes)),
      SegmentSize(SegThinking, thinkingBytes, estTokens(thinkingBytes)),
      SegmentSize(SegClaudeMd, claudeMdBytes, estTokens(claudeMdBytes)),
      SegmentSize(SegMcpInstr, mcpInstrBytes, estTokens(mcpInstrBytes)),
      SegmentSize(SegSkills, skillsBytes, estTokens(skillsBytes)),
      SegmentSize(SegHistory, historyBytes, estTokens(historyBytes)),
      SegmentSize(SegLatestUser, latestUserBytes, estTokens(latestUserBytes)),
    )
    val segments    = rawSegments.filter(_.bytes > 0).sortBy(s => -s.estTokens)

    val totalBytes = segments.map(_.bytes).sum

    val inventory = buildInventory(mech, claudeMdBytes, mcpInstrBytes, skillsBytes, msgTexts)
    val anomalies = buildAnomalies(structure, mech, msgTexts, responseCaptured, stopReason)

    RequestAnatomy(
      model = model,
      totalEstBytes = totalBytes,
      totalEstTokens = estTokens(totalBytes),
      segments = segments,
      structure = structure,
      inventory = inventory,
      anomalies = anomalies,
    )
  }

  private def buildStructure(
    body: Json,
    messages: Vector[Json],
    systemJs: Option[Json],
    toolsArr: Vector[Json],
  ): StructureFacts = {
    val roles          = messages.flatMap(_.asObject).flatMap(_.apply("role")).flatMap(_.asString)
    val userTurns      = roles.count(_ === "user")
    val assistantTurns = roles.count(_ === "assistant")

    val (systemBlocks, cachedSystemBlocks) = systemJs match {
      case Some(j) if j.isArray =>
        val arr    = j.asArray.getOrElse(Vector.empty)
        val cached = arr.flatMap(_.asObject).count(_.contains("cache_control"))
        (arr.size, cached)
      case Some(j) if j.isString => (1, 0)
      case Some(_) | None => (0, 0)
    }

    val blocks                    = messages.flatMap(contentBlocks)
    def countType(t: String): Int = blocks.flatMap(_.asObject).count(_.apply("type").flatMap(_.asString).contains(t))

    val stream = body.asObject.flatMap(_.apply("stream")).flatMap(_.asBoolean)

    StructureFacts(
      messageCount = messages.size,
      userTurns = userTurns,
      assistantTurns = assistantTurns,
      systemBlocks = systemBlocks,
      cachedSystemBlocks = cachedSystemBlocks,
      toolsDefined = toolsArr.size,
      toolUseCount = countType("tool_use"),
      toolResultCount = countType("tool_result"),
      imageCount = countType("image"),
      thinkingBlocks = countType("thinking"),
      stream = stream,
    )
  }

  /** Content blocks for a message; empty when content is a plain string. */
  private def contentBlocks(msg: Json): Vector[Json] =
    msg.asObject.flatMap(_.apply("content")).flatMap(_.asArray).getOrElse(Vector.empty)

  /** Text strings from user-message text blocks (and string content). */
  private def collectUserTexts(messages: Vector[Json]): List[String] =
    messages.flatMap { msg =>
      val o    = msg.asObject
      val role = o.flatMap(_.apply("role")).flatMap(_.asString)
      if (!role.contains("user")) Vector.empty
      else
        o.flatMap(_.apply("content")) match {
          case Some(c) if c.isString => Vector(c.asString.getOrElse(""))
          case Some(c) if c.isArray =>
            c.asArray.getOrElse(Vector.empty).flatMap { b =>
              b.asObject
                .filter(_.apply("type").flatMap(_.asString).contains("text"))
                .flatMap(_.apply("text"))
                .flatMap(_.asString)
            }
          case Some(_) | None => Vector.empty
        }
    }.toList

  private def systemBytesOf(systemJs: Option[Json]): Int =
    systemJs match {
      case Some(j) if j.isArray =>
        j.asArray
          .getOrElse(Vector.empty)
          .flatMap { b =>
            b.asObject.flatMap(_.apply("text")).flatMap(_.asString)
          }
          .map(byteLen)
          .sum
      case Some(j) if j.isString => byteLen(j.asString.getOrElse(""))
      case Some(_) | None => 0
    }

  private def thinkingSignatureBytes(messages: Vector[Json]): Int =
    messages
      .flatMap(contentBlocks)
      .flatMap { b =>
        b.asObject
          .filter(_.apply("type").flatMap(_.asString).contains("thinking"))
          .flatMap(_.apply("signature"))
          .flatMap(_.asString)
      }
      .map(byteLen)
      .sum

  private def matchingReminderBytes(texts: List[String], marker: String): Int =
    texts.filter(_.contains(marker)).map(byteLen).sum

  private def latestUserTurnBytes(messages: Vector[Json]): Int =
    messages.lastOption match {
      case Some(msg) if msg.asObject.flatMap(_.apply("role")).flatMap(_.asString).contains("user") =>
        msg.asObject.flatMap(_.apply("content")) match {
          case Some(c) if c.isString => byteLen(c.asString.getOrElse(""))
          case Some(c) if c.isArray => jsonBytes(c)
          case Some(_) | None => 0
        }
      case Some(_) | None => 0
    }

  private def buildInventory(
    mech: Mechanisms,
    claudeMdBytes: Int,
    mcpInstrBytes: Int,
    skillsBytes: Int,
    msgTexts: List[String],
  ): List[InventoryRow] = {
    val buf = scala.collection.mutable.ListBuffer.empty[InventoryRow]

    mech.claudeMd.foreach { raw =>
      val sections = ClaudeMdParser.parseClaudeMdSections(raw)
      val count    = if (sections.nonEmpty) sections.size else 1
      buf += InventoryRow("cm", InvClaudeMd, count, estTokens(claudeMdBytes))
    }

    val mcpInstrCount = msgTexts.count(_.contains(McpInstrMarker))
    if (mcpInstrCount > 0) buf += InventoryRow("mc", InvMcp, mcpInstrCount, estTokens(mcpInstrBytes))

    val skillsCount = msgTexts.filter(_.contains(SkillsListMarker)).map(countSkillLines).sum
    if (skillsCount > 0) buf += InventoryRow("sk", InvSkills, skillsCount, estTokens(skillsBytes))

    if (mech.slashCommands.nonEmpty) buf += InventoryRow("sc", InvSlash, mech.slashCommands.size, 0)
    if (mech.subAgents.nonEmpty) buf += InventoryRow("sa", InvSubAgent, mech.subAgents.size, 0)

    buf.toList
  }

  private def countSkillLines(text: String): Int =
    text.linesIterator.count(_.trim.startsWith("- "))

  private def buildAnomalies(
    structure: StructureFacts,
    mech: Mechanisms,
    msgTexts: List[String],
    responseCaptured: Boolean,
    stopReason: Option[String],
  ): List[Anomaly] = {
    val buf = scala.collection.mutable.ListBuffer.empty[Anomaly]

    /* Security */
    val hasCredential = msgTexts.exists(t => credentialPrefixes.exists(t.contains))
    val hasEcho       = msgTexts.exists(t => echoInstructions.exists(p => t.toLowerCase.contains(p)))
    if (hasCredential && hasEcho) buf += Anomaly(AnomalyKind.Security, AnomCredential)

    /* Warn */
    if (!responseCaptured) buf += Anomaly(AnomalyKind.Warn, AnomNoResponse)
    if (stopReason.contains("max_tokens")) buf += Anomaly(AnomalyKind.Warn, AnomTruncated)

    /* Info */
    if (structure.systemBlocks >= 2)
      buf += Anomaly(AnomalyKind.Info, AnomBasePrompt, Map("n" -> structure.systemBlocks.toString))

    val skillsAdvertised = msgTexts.exists(_.contains(SkillsListMarker))
    val skillInvoked     = mech.skills.nonEmpty
    if (structure.toolsDefined > 0 && structure.toolUseCount === 0 && skillsAdvertised && !skillInvoked)
      buf += Anomaly(AnomalyKind.Info, AnomToolsUnused)

    buf.toList
  }
}
