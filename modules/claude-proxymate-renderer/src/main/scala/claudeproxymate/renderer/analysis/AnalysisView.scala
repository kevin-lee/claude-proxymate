package claudeproxymate.renderer.analysis

import cats.syntax.all.*
import claudeproxymate.renderer.view.I18nTemplate
import scalatags.Text.all.*

/** Typed inputs for [[AnalysisView]]. The orchestrator
  * ([[AnalysisRenderer]]) computes derived strings (MCP server / tool
  * split, sub-agent type / prompt, slash-command desc with `{cmd}`
  * pre-substituted) so the view does no parsing.
  */
final case class CmSectionView(label: String, path: String, content: String, scope: String)
final case class SlashCommandView(name: String, full: String)
final case class SkillView(id: String, inputJson: String, result: Option[String])
final case class SubAgentView(name: String, subagentType: String, inputJson: String, prompt: String, isJson: Boolean)
final case class McpToolView(
  id: String,
  serverName: String,
  toolName: String,
  inputJson: String,
  isJson: Boolean,
  result: Option[String]
)

final case class AnalysisData(
  modelName: Option[String],
  // Right(sections) when ClaudeMdParser found section boundaries;
  // Left(raw)      when the parser found nothing (raw fallback).
  claudeMd: Option[Either[String, List[CmSectionView]]],
  outputStyle: Option[String],
  slashCommands: List[SlashCommandView],
  skills: List[SkillView],
  slashSkillLinked: Boolean,
  subAgents: List[SubAgentView],
  mcpTools: List[McpToolView],
)

/** Per-skill rendered description text, pre-resolved by the orchestrator
  * (because of the linked / unlinked selector).
  */
final case class SkillLabel(title: String, desc: String)

final case class AnalysisLabels(
  noMechanismsTitle: String,
  noMechanismsHint: String,
  claudeMdTitle: String,
  claudeMdDesc: String,
  outputStyleTitle: String,
  outputStyleDesc: String,
  slashCmdTitle: String,
  // Indexed list of pre-substituted slash-command desc strings (one per
  // command, matching `data.slashCommands` order).
  slashCmdDescs: List[String],
  skillTitle: String,
  skillLinkedTitle: String,
  skillDesc: String,
  skillLinkedDesc: String,
  noToolResult: String,
  subAgentDesc: String,
  mcpDesc: String,
  searchPlaceholder: String,
  searchClear: String,
)

/** Pure view for the Analysis tab. */
object AnalysisView {

  val SearchInputClass: String = "msg-search-input"
  val SearchClearClass: String = "msg-search-clear"

  val JsonBlockClass: String = "jt-json-block"
  val JsonDataAttr: String   = "data-json"

  /** Preserves the selector read by `DetailView.scala` to scroll the
    * active mechanism into view.
    */
  val MechKeyAttr: String = "data-mech-key"

  /** Build the entire Analysis tab fragment.
    *
    * `mechChipsHtml` is the pre-rendered output of
    * `MechChips.buildMechFilterChips`; wrapped in `raw(...)` because
    * the upstream is itself Scalatags-rendered.
    */
  def buildFrag(
    data: Option[AnalysisData],
    labels: AnalysisLabels,
    searchQuery: String,
    searchInputId: String,
    mechChipsHtml: String,
  ): Frag =
    frag(
      div(style := "flex-shrink:0")(
        buildSearchBarFrag(searchInputId, searchQuery, labels),
        raw(mechChipsHtml),
      ),
      div(style := "flex:1;overflow:auto")(
        div(cls := "analysis-view")(
          data match {
            case None => buildEmptyFrag(labels)
            case Some(d) => buildBody(d, labels)
          },
        ),
      ),
    )

  def buildSearchBarFrag(searchInputId: String, query: String, labels: AnalysisLabels): Frag =
    // No inline handlers — the input + clear button are wired by
    // ProxyDetailSearchListeners installed once from RendererMain.
    div(cls := "msg-search-bar", style := "flex-shrink:0")(
      input(
        tpe := "text",
        cls := SearchInputClass,
        id := searchInputId,
        placeholder := labels.searchPlaceholder,
        value := query,
      ),
      if (query.nonEmpty)
        button(
          cls := SearchClearClass,
          attr("title") := labels.searchClear,
        )("✕")
      else frag(),
    )

  def buildEmptyFrag(labels: AnalysisLabels): Frag =
    div(cls := "analysis-none")(
      labels.noMechanismsTitle,
      small(I18nTemplate.render(labels.noMechanismsHint)),
    )

  private def buildBody(d: AnalysisData, labels: AnalysisLabels): Frag = {
    val sections = scala.collection.mutable.ListBuffer.empty[Frag]

    d.modelName.foreach { name =>
      sections += div(cls := "analysis-kv")(
        span(cls := "ak")("model"),
        span(cls := "av")(name),
      )
    }

    d.claudeMd.foreach {
      case Left(raw) =>
        sections += div(cls := "analysis-section", attr(MechKeyAttr) := "cm")(
          div(cls := "analysis-section-title", style := "color:var(--green)")(labels.claudeMdTitle),
          div(cls := "analysis-desc")(labels.claudeMdDesc),
          div(cls := "analysis-block highlight-green")(raw),
        )
      case Right(parsed) =>
        sections += div(cls := "analysis-section", attr(MechKeyAttr) := "cm")(
          div(cls := "analysis-section-title", style := "color:var(--green)")(labels.claudeMdTitle),
          div(cls := "analysis-desc")(labels.claudeMdDesc),
          frag(parsed.zipWithIndex.map { case (s, i) => buildCmSection(s, i) }),
        )
    }

    d.outputStyle.foreach { extra =>
      sections += div(cls := "analysis-section", attr(MechKeyAttr) := "st")(
        div(cls := "analysis-section-title", style := "color:var(--blue)")(labels.outputStyleTitle),
        div(cls := "analysis-desc")(labels.outputStyleDesc),
        div(cls := "analysis-block highlight-blue")(extra),
      )
    }

    d.slashCommands.zipWithIndex.foreach {
      case (cmd, i) =>
        val desc = labels.slashCmdDescs.lift(i).getOrElse(labels.slashCmdTitle)
        sections += div(cls := "analysis-section", attr(MechKeyAttr) := s"sc_$i")(
          div(cls := "analysis-section-title", style := "color:var(--yellow)")(labels.slashCmdTitle),
          div(cls := "analysis-desc")(desc),
          div(cls := "analysis-block highlight-yellow")(cmd.full),
        )
    }

    d.skills.zipWithIndex.foreach {
      case (sk, i) =>
        val titleText = if (d.slashSkillLinked) labels.skillLinkedTitle else labels.skillTitle
        val descText  = if (d.slashSkillLinked) labels.skillLinkedDesc else labels.skillDesc
        sections += div(cls := "analysis-section", attr(MechKeyAttr) := s"sk_$i")(
          div(cls := "analysis-section-title", style := "color:var(--purple)")(titleText),
          div(cls := "analysis-desc")(descText),
          div(cls := "analysis-kv")(span(cls := "ak")("id"), span(cls := "av")(sk.id)),
          div(
            cls := s"analysis-block highlight-purple $JsonBlockClass",
            attr(JsonDataAttr) := sk.inputJson,
          ),
          sk.result match {
            case Some(r) =>
              frag(
                div(cls := "analysis-kv")(span(cls := "ak")("result (tool_result)")),
                div(cls := "analysis-block highlight-purple")(r),
              )
            case None =>
              div(cls := "analysis-kv")(
                span(cls := "ak", style := "color:var(--dim)")(labels.noToolResult),
              )
          },
        )
    }

    d.subAgents.foreach { sa =>
      sections += div(cls := "analysis-section", attr(MechKeyAttr) := "sa")(
        div(cls := "analysis-section-title", style := "color:var(--orange)")(s"${sa.name} — Sub-Agent"),
        div(cls := "analysis-desc")(labels.subAgentDesc),
        div(cls := "analysis-kv")(
          span(cls := "ak")("subagent_type"),
          span(cls := "av", style := "color:var(--orange)")(sa.subagentType),
        ),
        if (sa.isJson)
          div(
            cls := s"analysis-block highlight-orange $JsonBlockClass",
            attr(JsonDataAttr) := sa.inputJson,
          )
        else
          div(cls := "analysis-block highlight-orange")(sa.prompt),
      )
    }

    d.mcpTools.zipWithIndex.foreach {
      case (mc, i) =>
        val cyanStyle =
          "border-left:2px solid color-mix(in srgb, var(--cyan) 45%, transparent);" +
            "background:color-mix(in srgb, var(--cyan) 10%, transparent)"
        sections += div(cls := "analysis-section", attr(MechKeyAttr) := s"mc_$i")(
          div(cls := "analysis-section-title", style := "color:var(--cyan)")(
            s"🔌 ${mc.toolName} ",
            span(style := "font-weight:400;opacity:.7")(s"(${mc.serverName})"),
          ),
          div(cls := "analysis-desc")(labels.mcpDesc),
          div(cls := "analysis-kv")(span(cls := "ak")("id"), span(cls := "av")(mc.id)),
          if (mc.isJson)
            div(
              cls := s"analysis-block $JsonBlockClass",
              style := cyanStyle,
              attr(JsonDataAttr) := mc.inputJson,
            )
          else
            div(cls := "analysis-block", style := cyanStyle)(mc.inputJson),
          mc.result match {
            case Some(r) =>
              frag(
                div(cls := "analysis-kv")(span(cls := "ak")("result (tool_result)")),
                div(cls := "analysis-block", style := cyanStyle)(r),
              )
            case None => frag()
          },
        )
    }

    frag(sections.toList)
  }

  private def buildCmSection(s: CmSectionView, idx: Int): Frag = {
    val color   = if (s.scope === "global") "var(--green)" else "var(--blue)"
    val hlClass = if (s.scope === "global") "highlight-green" else "highlight-blue"
    div(style := "margin-top:8px", attr(MechKeyAttr) := s"cm_$idx")(
      div(style := s"font-size:10px;font-weight:700;color:$color;margin-bottom:4px")(s.label),
      div(style := "font-size:10px;color:var(--dim);margin-bottom:4px;word-break:break-all")(s.path),
      div(cls := s"analysis-block $hlClass")(s.content),
    )
  }
}
