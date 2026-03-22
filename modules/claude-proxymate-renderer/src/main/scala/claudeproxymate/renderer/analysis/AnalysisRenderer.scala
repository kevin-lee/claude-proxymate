package claudeproxymate.renderer.analysis

import claudeproxymate.core.{ClaudeMdParser, HtmlIds}
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.json.JsonTreeViewer
import claudeproxymate.renderer.util.HtmlUtil.{esc, escAttr}
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Render the Analysis tab for a proxy capture entry.
  *
  * Ports `renderProxyAnalysis` from renderer.js.
  */
object AnalysisRenderer {

  @JSExportTopLevel("renderProxyAnalysis")
  def renderProxyAnalysis(entry: js.Dynamic, container: dom.html.Element): Unit = {
    val body = entry.selectDynamic("body")
    val det  = MechChips.detectMechanismsFromDynamic(body)
    val hasAny = det.claudeMd.isDefined || det.outputStyle.isDefined ||
      det.slashCommands.nonEmpty || det.skills.nonEmpty ||
      det.subAgents.nonEmpty || det.mcpTools.nonEmpty

    val mechChipsHtml = MechChips.buildMechFilterChips(body)

    val aq = claudeproxymate.renderer.state.AppState.proxyDetailSearch
    val analysisSearchBar = buildAnalysisSearchBar(aq)

    val sb = new StringBuilder
    sb.append("""<div style="flex:1;overflow:auto"><div class="analysis-view">""")

    if (!hasAny) {
      sb.append(s"""<div class="analysis-none">${I18n.t("analysis.noMechanisms")}</div>""")
      sb.append("</div></div>")
      container.innerHTML = s"""<div style="flex-shrink:0">$analysisSearchBar$mechChipsHtml</div>""" + sb.toString()
      return
    }

    // Model info
    if (!js.isUndefined(body) && body != null) {
      val model = body.selectDynamic("model")
      if (!js.isUndefined(model) && model != null) {
        sb.append(s"""<div class="analysis-kv"><span class="ak">model</span><span class="av">${esc(model.toString)}</span></div>""")
      }
    }

    // CLAUDE.md
    det.claudeMd.foreach { claudeMd =>
      val mdSections = ClaudeMdParser.parseClaudeMdSections(claudeMd)
      if (mdSections.nonEmpty) {
        sb.append(s"""<div class="analysis-section" data-mech-key="cm">""")
        sb.append(s"""<div class="analysis-section-title" style="color:var(--green)">${I18n.t("analysis.claudeMdTitle")}</div>""")
        sb.append(s"""<div class="analysis-desc">${I18n.t("analysis.claudeMdDesc")}</div>""")
        for ((s, si) <- mdSections.zipWithIndex) {
          val color = if (s.scope == "global") "var(--green)" else "var(--blue)"
          val hlClass = if (s.scope == "global") "highlight-green" else "highlight-blue"
          sb.append(s"""<div style="margin-top:8px" data-mech-key="cm_$si">""")
          sb.append(s"""<div style="font-size:10px;font-weight:700;color:$color;margin-bottom:4px">${esc(s.label)}</div>""")
          sb.append(s"""<div style="font-size:10px;color:var(--dim);margin-bottom:4px;word-break:break-all">${esc(s.path)}</div>""")
          sb.append(s"""<div class="analysis-block $hlClass">${esc(s.content)}</div>""")
          sb.append("</div>")
        }
        sb.append("</div>")
      } else {
        sb.append(s"""<div class="analysis-section" data-mech-key="cm">""")
        sb.append(s"""<div class="analysis-section-title" style="color:var(--green)">${I18n.t("analysis.claudeMdTitle")}</div>""")
        sb.append(s"""<div class="analysis-desc">${I18n.t("analysis.claudeMdDesc")}</div>""")
        sb.append(s"""<div class="analysis-block highlight-green">${esc(claudeMd)}</div>""")
        sb.append("</div>")
      }
    }

    // Output Style
    det.outputStyle.foreach { styles =>
      val extra = styles.drop(1).mkString("\n\n---\n\n")
      sb.append(s"""<div class="analysis-section" data-mech-key="st">""")
      sb.append(s"""<div class="analysis-section-title" style="color:var(--blue)">${I18n.t("analysis.outputStyleTitle")}</div>""")
      sb.append(s"""<div class="analysis-desc">${I18n.t("analysis.outputStyleDesc")}</div>""")
      sb.append(s"""<div class="analysis-block highlight-blue">${esc(extra)}</div>""")
      sb.append("</div>")
    }

    // Slash Commands + Skills
    val slashSkillLinked = det.slashCommands.nonEmpty && det.skills.nonEmpty

    for ((cmd, i) <- det.slashCommands.zipWithIndex) {
      sb.append(s"""<div class="analysis-section" data-mech-key="sc_$i">""")
      sb.append(s"""<div class="analysis-section-title" style="color:var(--yellow)">${I18n.t("analysis.slashCmdTitle")}</div>""")
      sb.append(s"""<div class="analysis-desc">${I18n.t("analysis.slashCmdDesc", Map("cmd" -> esc(cmd.name)))}</div>""")
      sb.append(s"""<div class="analysis-block highlight-yellow">${esc(cmd.full)}</div>""")
      sb.append("</div>")
    }

    for ((sk, i) <- det.skills.zipWithIndex) {
      val titleKey = if (slashSkillLinked) "analysis.skillLinkedTitle" else "analysis.skillTitle"
      val descKey = if (slashSkillLinked) "analysis.skillLinkedDesc" else "analysis.skillDesc"
      val inputJson = sk.input.noSpaces
      sb.append(s"""<div class="analysis-section" data-mech-key="sk_$i">""")
      sb.append(s"""<div class="analysis-section-title" style="color:var(--purple)">${I18n.t(titleKey)}</div>""")
      sb.append(s"""<div class="analysis-desc">${I18n.t(descKey)}</div>""")
      sb.append(s"""<div class="analysis-kv"><span class="ak">id</span><span class="av">${esc(sk.id)}</span></div>""")
      sb.append(s"""<div class="analysis-block highlight-purple jt-json-block" data-json="${escAttr(inputJson)}"></div>""")
      sk.result match {
        case Some(result) =>
          sb.append("""<div class="analysis-kv"><span class="ak">result (tool_result)</span></div>""")
          sb.append(s"""<div class="analysis-block highlight-purple">${esc(result)}</div>""")
        case None =>
          sb.append(s"""<div class="analysis-kv"><span class="ak" style="color:var(--dim)">${I18n.t("analysis.noToolResult")}</span></div>""")
      }
      sb.append("</div>")
    }

    // Sub-Agents
    for (sa <- det.subAgents) {
      val inputJson = sa.input.noSpaces
      val isJson = sa.input.isObject || sa.input.isArray
      val subagentType = sa.input.hcursor.downField("subagent_type").as[String]
        .orElse(sa.input.hcursor.downField("type").as[String])
        .getOrElse("?")
      val prompt = sa.input.hcursor.downField("prompt").as[String].getOrElse("")
      sb.append(s"""<div class="analysis-section" data-mech-key="sa">""")
      sb.append(s"""<div class="analysis-section-title" style="color:var(--orange)">${esc(sa.name)} — Sub-Agent</div>""")
      sb.append(s"""<div class="analysis-desc">${I18n.t("analysis.subAgentDesc")}</div>""")
      sb.append(s"""<div class="analysis-kv"><span class="ak">subagent_type</span><span class="av" style="color:var(--orange)">${esc(subagentType)}</span></div>""")
      if (isJson) {
        sb.append(s"""<div class="analysis-block highlight-orange jt-json-block" data-json="${escAttr(inputJson)}"></div>""")
      } else {
        sb.append(s"""<div class="analysis-block highlight-orange">${esc(prompt)}</div>""")
      }
      sb.append("</div>")
    }

    // MCP Tools
    for ((mc, i) <- det.mcpTools.zipWithIndex) {
      val parts = mc.name.split("__")
      val serverName = if (parts.length > 1) parts(1) else "?"
      val toolName = if (parts.length > 2) parts.drop(2).mkString("__") else mc.name
      val inputJson = mc.input.noSpaces
      val isJson = mc.input.isObject || mc.input.isArray
      sb.append(s"""<div class="analysis-section" data-mech-key="mc_$i">""")
      sb.append(s"""<div class="analysis-section-title" style="color:var(--cyan)">\uD83D\uDD0C ${esc(toolName)} <span style="font-weight:400;opacity:.7">(${esc(serverName)})</span></div>""")
      sb.append(s"""<div class="analysis-desc">${I18n.t("mechDesc.mc.what")}</div>""")
      sb.append(s"""<div class="analysis-kv"><span class="ak">id</span><span class="av">${esc(mc.id)}</span></div>""")
      if (isJson) {
        sb.append(s"""<div class="analysis-block jt-json-block" style="border-left:2px solid color-mix(in srgb, var(--cyan) 45%, transparent);background:color-mix(in srgb, var(--cyan) 10%, transparent)" data-json="${escAttr(inputJson)}"></div>""")
      } else {
        sb.append(s"""<div class="analysis-block" style="border-left:2px solid color-mix(in srgb, var(--cyan) 45%, transparent);background:color-mix(in srgb, var(--cyan) 10%, transparent)">${esc(inputJson)}</div>""")
      }
      mc.result.foreach { result =>
        sb.append("""<div class="analysis-kv"><span class="ak">result (tool_result)</span></div>""")
        sb.append(s"""<div class="analysis-block" style="border-left:2px solid color-mix(in srgb, var(--cyan) 45%, transparent);background:color-mix(in srgb, var(--cyan) 10%, transparent)">${esc(result)}</div>""")
      }
      sb.append("</div>")
    }

    sb.append("</div></div>")
    container.innerHTML = s"""<div style="flex-shrink:0">$analysisSearchBar$mechChipsHtml</div>""" + sb.toString()

    // Render JSON trees in jt-json-block elements
    val jsonBlocks = container.querySelectorAll(".jt-json-block")
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

  private def buildAnalysisSearchBar(q: String): String = {
    val clearBtn =
      if (q.nonEmpty)
        s"""<button class="msg-search-clear" onclick="setProxyDetailSearch('');document.getElementById('${HtmlIds.ProxyDetailSearchInput}')?.focus()" title="${escAttr(I18n.t("messages.searchClear"))}">✕</button>"""
      else ""
    s"""<div class="msg-search-bar" style="flex-shrink:0">""" +
      s"""<input type="text" class="msg-search-input" id="${HtmlIds.ProxyDetailSearchInput}" """ +
      s"""placeholder="${escAttr(I18n.t("analysis.searchPlaceholder"))}" value="${escAttr(q)}" """ +
      s"""oninput="setProxyDetailSearch(this.value)" """ +
      s"""oncompositionstart="_imeComposing=true" """ +
      s"""oncompositionend="_imeComposing=false;setProxyDetailSearch(this.value)">""" +
      clearBtn +
      "</div>"
  }
}
