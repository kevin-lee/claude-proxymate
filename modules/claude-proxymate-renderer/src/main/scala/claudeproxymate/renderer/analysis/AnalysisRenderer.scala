package claudeproxymate.renderer.analysis

import cats.syntax.all.*
import claudeproxymate.core.{ClaudeMdParser, HtmlIds, RequestAnatomy, SegmentSize}
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.json.JsonTreeViewer
import claudeproxymate.renderer.util.JsJsonBridge
import claudeproxymate.renderer.view.ViewHelpers
import org.scalajs.dom

import scala.scalajs.js

/** Render the Analysis tab for a proxy capture entry.
  *
  * Thin orchestrator: builds typed [[AnalysisData]] from the captured
  * body, resolves [[AnalysisLabels]] from i18n, renders via
  * [[AnalysisView]], then runs the post-render JSON-tree hydration walk
  * that picks up `jt-json-block[data-json]` elements (J-e deferred).
  */
object AnalysisRenderer {

  def renderProxyAnalysis(entry: js.Dynamic, container: dom.html.Element): Unit = {
    val body   = entry.selectDynamic("body")
    val det    = MechChips.detectMechanismsFromDynamic(body)
    val hasAny = det.claudeMd.isDefined || det.outputStyle.isDefined ||
      det.slashCommands.nonEmpty || det.skills.nonEmpty ||
      det.subAgents.nonEmpty || det.mcpTools.nonEmpty

    val mechChipsHtml = MechChips.buildMechFilterChips(body)
    val aq            = claudeproxymate.renderer.state.AppState.proxyDetailSearch
    val labels        = buildLabels(det.slashCommands.map(_.name))

    val data: Option[AnalysisData] =
      Option.when(hasAny)(
        AnalysisData(
          modelName = readModelName(body),
          claudeMd = readClaudeMd(det.claudeMd),
          outputStyle = det.outputStyle.map(_.drop(1).mkString("\n\n---\n\n")),
          slashCommands = det.slashCommands.map(c => SlashCommandView(c.name, c.full)),
          skills = det.skills.map(s => SkillView(s.id, s.input.noSpaces, s.result)),
          slashSkillLinked = det.slashCommands.nonEmpty && det.skills.nonEmpty,
          subAgents = det.subAgents.map(buildSubAgent),
          mcpTools = det.mcpTools.map(buildMcpTool),
        )
      )

    ViewHelpers.setInnerHtml(
      container,
      AnalysisView.buildFrag(
        data = data,
        labels = labels,
        searchQuery = aq,
        searchInputId = HtmlIds.ProxyDetailSearchInput,
        mechChipsHtml = mechChipsHtml,
      ),
    )

    // Request Anatomy dashboard: prepend the four cards above the existing
    // mechanism sections inside `.analysis-view`.
    val analysisViewEl = container.querySelector(".analysis-view").asInstanceOf[dom.html.Element]
    if (analysisViewEl != null) {
      val anatomyHtml = buildAnatomyHtml(entry, body)
      if (anatomyHtml.nonEmpty) analysisViewEl.insertAdjacentHTML("afterbegin", anatomyHtml)
    }

    // Post-render JSON-tree hydration. The view emits `jt-json-block` divs
    // with `data-json="<input>"`; render the live JSON tree into each.
    val jsonBlocks = container.querySelectorAll(s".${AnalysisView.JsonBlockClass}")
    var idx        = 0
    while (idx < jsonBlocks.length) {
      val el      = jsonBlocks(idx).asInstanceOf[dom.html.Element]
      val jsonStr = el.dataset.get("json").getOrElse("")
      if (jsonStr.nonEmpty) {
        try JsonTreeViewer.renderJsonTree(el, js.JSON.parse(jsonStr))
        catch { case _: Throwable => el.textContent = jsonStr }
      }
      idx += 1
    }
  }

  private def buildLabels(slashCmdNames: List[String]): AnalysisLabels =
    AnalysisLabels(
      noMechanismsTitle = I18n.t("analysis.noMechanismsTitle"),
      noMechanismsHint = I18n.t("analysis.noMechanismsHint"),
      claudeMdTitle = I18n.t("analysis.claudeMdTitle"),
      claudeMdDesc = I18n.t("analysis.claudeMdDesc"),
      outputStyleTitle = I18n.t("analysis.outputStyleTitle"),
      outputStyleDesc = I18n.t("analysis.outputStyleDesc"),
      slashCmdTitle = I18n.t("analysis.slashCmdTitle"),
      slashCmdDescs = slashCmdNames.map(name => I18n.t("analysis.slashCmdDesc", Map("cmd" -> name))),
      skillTitle = I18n.t("analysis.skillTitle"),
      skillLinkedTitle = I18n.t("analysis.skillLinkedTitle"),
      skillDesc = I18n.t("analysis.skillDesc"),
      skillLinkedDesc = I18n.t("analysis.skillLinkedDesc"),
      noToolResult = I18n.t("analysis.noToolResult"),
      subAgentDesc = I18n.t("analysis.subAgentDesc"),
      mcpDesc = I18n.t("mechDesc.mc.what"),
      searchPlaceholder = I18n.t("analysis.searchPlaceholder"),
      searchClear = I18n.t("messages.searchClear"),
    )

  /** Build the Request Anatomy dashboard HTML for the given capture. Returns
    * empty string when there's no usable body.
    */
  private def buildAnatomyHtml(entry: js.Dynamic, body: js.Dynamic): String = {
    if (js.isUndefined(body) || body == null) return ""
    val bodyJson = JsJsonBridge.toCirceJsonUnsafe(body)

    val respBody         = readResponseBody(entry)
    val usage            = if (respBody != null) respBody.selectDynamic("usage") else null
    val responseCaptured = !js.isUndefined(usage) && usage != null
    val stopReason       = readStopReason(respBody)

    val anatomy = RequestAnatomy.analyze(bodyJson, responseCaptured, stopReason)

    val reqBytes = {
      val jsonStr = js.JSON.stringify(body)
      val encoder = js.Dynamic.newInstance(dom.window.asInstanceOf[js.Dynamic].TextEncoder)()
      encoder.encode(jsonStr).length.asInstanceOf[Int]
    }
    val reqKb    = f"${reqBytes / 1024.0}%.1f"
    val model    = anatomy.model.getOrElse("")

    val cost =
      if (responseCaptured)
        AnatomyCost.fromUsage(
          model = model,
          reqKb = reqKb,
          inputTokens = usageInt(usage, "input_tokens"),
          cacheRead = usageInt(usage, "cache_read_input_tokens"),
          cacheWrite = usageInt(usage, "cache_creation_input_tokens"),
          outputTokens = usageInt(usage, "output_tokens"),
        )
      else
        AnatomyCost.estimateOnly(model, reqKb, RequestAnatomy.estTokens(reqBytes))

    val labels    = buildAnatomyLabels()
    val segments  = resolveSegments(anatomy.segments)
    val inventory = anatomy.inventory.map(r => InventoryDisplay(I18n.t(r.label), r.count, r.estTokens))
    val anomalies = anatomy.anomalies.map(a => AnomalyDisplay(a.kind, I18n.t(a.key, a.params)))

    AnatomyView.buildFrag(cost, anatomy, segments, inventory, anomalies, labels).render
  }

  private def resolveSegments(segments: List[SegmentSize]): List[SegmentRow] = {
    val total = segments.map(_.estTokens).sum
    segments.map { s =>
      val pct = if (total > 0) math.round(s.estTokens.toDouble / total * 100).toInt else 0
      SegmentRow(I18n.t(s.label), s.estTokens, pct)
    }
  }

  private def readResponseBody(entry: js.Dynamic): js.Dynamic = {
    val resp = entry.selectDynamic("response")
    if (js.isUndefined(resp) || resp == null) null
    else {
      val rb = resp.selectDynamic("body")
      if (js.isUndefined(rb) || rb == null) null else rb
    }
  }

  private def readStopReason(respBody: js.Dynamic): Option[String] = {
    if (respBody == null) none[String]
    else {
      val sr = respBody.selectDynamic("stop_reason")
      Option.unless(js.isUndefined(sr) || sr == null)(sr.toString)
    }
  }

  private def usageInt(usage: js.Dynamic, name: String): Int = {
    val v = usage.selectDynamic(name)
    if (!js.isUndefined(v) && v != null)
      try v.asInstanceOf[Int]
      catch { case _: Throwable => 0 }
    else 0
  }

  private def buildAnatomyLabels(): AnatomyLabels =
    AnatomyLabels(
      costTitle = I18n.t("anatomy.costTitle"),
      attributionTitle = I18n.t("anatomy.attributionTitle"),
      structureTitle = I18n.t("anatomy.structureTitle"),
      inventoryTitle = I18n.t("anatomy.inventoryTitle"),
      anomaliesTitle = I18n.t("anatomy.anomaliesTitle"),
      estimatedTag = I18n.t("anatomy.estimatedTag"),
      noResponseNote = I18n.t("anatomy.noResponseNote"),
      lblMessages = I18n.t("anatomy.lblMessages"),
      lblTurns = I18n.t("anatomy.lblTurns"),
      lblSystemBlocks = I18n.t("anatomy.lblSystemBlocks"),
      lblTools = I18n.t("anatomy.lblTools"),
      lblToolUse = I18n.t("anatomy.lblToolUse"),
      lblToolResult = I18n.t("anatomy.lblToolResult"),
      lblImages = I18n.t("anatomy.lblImages"),
      lblThinking = I18n.t("anatomy.lblThinking"),
      lblStream = I18n.t("anatomy.lblStream"),
    )

  private def readModelName(body: js.Dynamic): Option[String] = {
    if (js.isUndefined(body) || body == null) none[String]
    else {
      val model = body.selectDynamic("model")
      Option.unless(js.isUndefined(model) || model == null)(model.toString)
    }
  }

  private def readClaudeMd(claudeMd: Option[String]): Option[Either[String, List[CmSectionView]]] =
    claudeMd.map { raw =>
      val parsed = ClaudeMdParser.parseClaudeMdSections(raw)
      Either.cond(parsed.nonEmpty, parsed.map(s => CmSectionView(s.label, s.path, s.content, s.scope)), raw)
    }

  private def buildSubAgent(sa: claudeproxymate.core.SubAgent): SubAgentView = {
    val inputJson    = sa.input.noSpaces
    val isJson       = sa.input.isObject || sa.input.isArray
    val subagentType = sa
      .input
      .hcursor
      .downField("subagent_type")
      .as[String]
      .orElse(sa.input.hcursor.downField("type").as[String])
      .getOrElse("?")
    val prompt       = sa.input.hcursor.downField("prompt").as[String].getOrElse("")
    SubAgentView(sa.name, subagentType, inputJson, prompt, isJson)
  }

  private def buildMcpTool(mc: claudeproxymate.core.McpTool): McpToolView = {
    val parts      = mc.name.split("__")
    val serverName = if (parts.length > 1) parts(1) else "?"
    val toolName   = if (parts.length > 2) parts.drop(2).mkString("__") else mc.name
    val inputJson  = mc.input.noSpaces
    val isJson     = mc.input.isObject || mc.input.isArray
    McpToolView(mc.id, serverName, toolName, inputJson, isJson, mc.result)
  }
}
