package claudeproxymate.renderer.analysis

import claudeproxymate.core.{ClaudeMdParser, HtmlIds}
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.json.JsonTreeViewer
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
    val body = entry.selectDynamic("body")
    val det  = MechChips.detectMechanismsFromDynamic(body)
    val hasAny = det.claudeMd.isDefined || det.outputStyle.isDefined ||
      det.slashCommands.nonEmpty || det.skills.nonEmpty ||
      det.subAgents.nonEmpty || det.mcpTools.nonEmpty

    val mechChipsHtml = MechChips.buildMechFilterChips(body)
    val aq            = claudeproxymate.renderer.state.AppState.proxyDetailSearch
    val labels        = buildLabels(det.slashCommands.map(_.name))

    val data: Option[AnalysisData] =
      if (!hasAny) None
      else
        Some(
          AnalysisData(
            modelName       = readModelName(body),
            claudeMd        = readClaudeMd(det.claudeMd),
            outputStyle     = det.outputStyle.map(_.drop(1).mkString("\n\n---\n\n")),
            slashCommands   = det.slashCommands.map(c => SlashCommandView(c.name, c.full)),
            skills          = det.skills.map(s => SkillView(s.id, s.input.noSpaces, s.result)),
            slashSkillLinked = det.slashCommands.nonEmpty && det.skills.nonEmpty,
            subAgents       = det.subAgents.map(buildSubAgent),
            mcpTools        = det.mcpTools.map(buildMcpTool),
          )
        )

    ViewHelpers.setInnerHtml(
      container,
      AnalysisView.buildFrag(
        data           = data,
        labels         = labels,
        searchQuery    = aq,
        searchInputId  = HtmlIds.ProxyDetailSearchInput,
        mechChipsHtml  = mechChipsHtml,
      ),
    )

    // Post-render JSON-tree hydration. The view emits `jt-json-block` divs
    // with `data-json="<input>"`; render the live JSON tree into each.
    val jsonBlocks = container.querySelectorAll(s".${AnalysisView.JsonBlockClass}")
    var idx = 0
    while (idx < jsonBlocks.length) {
      val el = jsonBlocks(idx).asInstanceOf[dom.html.Element]
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
      noMechanismsHint  = I18n.t("analysis.noMechanismsHint"),
      claudeMdTitle     = I18n.t("analysis.claudeMdTitle"),
      claudeMdDesc      = I18n.t("analysis.claudeMdDesc"),
      outputStyleTitle  = I18n.t("analysis.outputStyleTitle"),
      outputStyleDesc   = I18n.t("analysis.outputStyleDesc"),
      slashCmdTitle     = I18n.t("analysis.slashCmdTitle"),
      slashCmdDescs     = slashCmdNames.map(name => I18n.t("analysis.slashCmdDesc", Map("cmd" -> name))),
      skillTitle        = I18n.t("analysis.skillTitle"),
      skillLinkedTitle  = I18n.t("analysis.skillLinkedTitle"),
      skillDesc         = I18n.t("analysis.skillDesc"),
      skillLinkedDesc   = I18n.t("analysis.skillLinkedDesc"),
      noToolResult      = I18n.t("analysis.noToolResult"),
      subAgentDesc      = I18n.t("analysis.subAgentDesc"),
      mcpDesc           = I18n.t("mechDesc.mc.what"),
      searchPlaceholder = I18n.t("analysis.searchPlaceholder"),
      searchClear       = I18n.t("messages.searchClear"),
    )

  private def readModelName(body: js.Dynamic): Option[String] = {
    if (js.isUndefined(body) || body == null) None
    else {
      val model = body.selectDynamic("model")
      if (js.isUndefined(model) || model == null) None else Some(model.toString)
    }
  }

  private def readClaudeMd(claudeMd: Option[String]): Option[Either[String, List[CmSectionView]]] =
    claudeMd.map { raw =>
      val parsed = ClaudeMdParser.parseClaudeMdSections(raw)
      if (parsed.isEmpty) Left(raw)
      else Right(parsed.map(s => CmSectionView(s.label, s.path, s.content, s.scope)))
    }

  private def buildSubAgent(sa: claudeproxymate.core.SubAgent): SubAgentView = {
    val inputJson    = sa.input.noSpaces
    val isJson       = sa.input.isObject || sa.input.isArray
    val subagentType = sa.input.hcursor.downField("subagent_type").as[String]
      .orElse(sa.input.hcursor.downField("type").as[String])
      .getOrElse("?")
    val prompt = sa.input.hcursor.downField("prompt").as[String].getOrElse("")
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
