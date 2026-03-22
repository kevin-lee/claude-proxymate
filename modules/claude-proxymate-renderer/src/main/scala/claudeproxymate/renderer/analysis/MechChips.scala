package claudeproxymate.renderer.analysis

import claudeproxymate.core.{ClaudeMdParser, Mechanisms, MechanismDetector}
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.HtmlUtil.{esc, escAttr}
import claudeproxymate.renderer.util.JsJsonBridge

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Mechanism filter chips and metadata.
  *
  * Ports `buildMechFilterChips`, `getChipMeta`, `setProxyDetailMechFilter`,
  * and `detectMechanisms` (JS bridge to core) from renderer.js.
  */
object MechChips {

  /** Bridge from JS `detectMechanisms(body)` to core `MechanismDetector`.
    * Takes `js.Dynamic`, converts via JsJsonBridge, returns `Mechanisms`.
    */
  def detectMechanismsFromDynamic(body: js.Dynamic): Mechanisms = {
    val json = JsJsonBridge.toCirceJsonUnsafe(body)
    MechanismDetector.detectMechanisms(json)
  }

  /** JS-callable `detectMechanisms` returning `js.Dynamic` for remaining JS code. */
  @JSExportTopLevel("detectMechanisms")
  def detectMechanismsJs(body: js.Dynamic): js.Dynamic = {
    val det = detectMechanismsFromDynamic(body)
    mechanismsToJsDynamic(det)
  }

  private def mechanismsToJsDynamic(det: Mechanisms): js.Dynamic = {
    val result = js.Dynamic.literal(
      "claudeMd" -> det.claudeMd.orNull,
      "outputStyle" -> det.outputStyle.map(ls => js.Array(ls.map(_.asInstanceOf[js.Any])*)).orNull,
      "slashCommands" -> js.Array(det.slashCommands.map { sc =>
        js.Dynamic.literal("name" -> sc.name, "tag" -> sc.tag, "full" -> sc.full)
      }*),
      "skills" -> js.Array(det.skills.map { sk =>
        val obj = js.Dynamic.literal(
          "id" -> sk.id,
          "input" -> JsJsonBridge.toJsDynamic(sk.input),
        )
        sk.result.foreach(r => obj.result = r)
        obj
      }*),
      "subAgents" -> js.Array(det.subAgents.map { sa =>
        js.Dynamic.literal("id" -> sa.id, "name" -> sa.name, "input" -> JsJsonBridge.toJsDynamic(sa.input))
      }*),
      "mcpTools" -> js.Array(det.mcpTools.map { mc =>
        val obj = js.Dynamic.literal(
          "id" -> mc.id,
          "name" -> mc.name,
          "input" -> JsJsonBridge.toJsDynamic(mc.input),
        )
        mc.result.foreach(r => obj.result = r)
        obj
      }*),
    )
    result
  }

  final case class ChipMeta(color: String, who: String, what: String)

  def getChipMeta(): Map[String, ChipMeta] = Map(
    "cm" -> ChipMeta("var(--green)", I18n.t("mechDesc.cm.who"), I18n.t("mechDesc.cm.what")),
    "sc" -> ChipMeta("var(--yellow)", I18n.t("mechDesc.sc.who"), I18n.t("mechDesc.sc.what")),
    "sk" -> ChipMeta("var(--purple)", I18n.t("mechDesc.sk.who"), I18n.t("mechDesc.sk.what")),
    "sa" -> ChipMeta("var(--orange)", I18n.t("mechDesc.sa.who"), I18n.t("mechDesc.sa.what")),
    "mc" -> ChipMeta("var(--cyan)", I18n.t("mechDesc.mc.who"), I18n.t("mechDesc.mc.what")),
  )

  @JSExportTopLevel("getChipMeta")
  def getChipMetaJs(): js.Dynamic = {
    val meta = getChipMeta()
    val result = js.Dynamic.literal()
    meta.foreach { case (k, v) =>
      result.updateDynamic(k)(js.Dynamic.literal("color" -> v.color, "who" -> v.who, "what" -> v.what))
    }
    result
  }

  @JSExportTopLevel("setProxyDetailMechFilter")
  def setProxyDetailMechFilter(key: String): Unit = {
    AppState.proxyDetailMechFilter = AppState.proxyDetailMechFilter match {
      case Some(k) if k == key => None
      case _                    => Some(key)
    }
    claudeproxymate.renderer.detail.DetailView.renderProxyDetail()
  }

  final case class Chip(key: String, found: Boolean, label: String, pattern: Option[String], cls: String, metaKey: String)

  def buildMechFilterChips(body: js.Dynamic): String = {
    val det = detectMechanismsFromDynamic(body)

    val chips = scala.collection.mutable.ListBuffer.empty[Chip]

    // CLAUDE.md sections
    det.claudeMd.foreach { claudeMd =>
      val sections = ClaudeMdParser.parseClaudeMdSections(claudeMd)
      if (sections.nonEmpty) {
        for ((s, i) <- sections.zipWithIndex) {
          val firstLine = s.content.split('\n').find(_.trim.length > 8).getOrElse(s.content.take(40))
          val cls = if (s.cls == "cyan") "st" else "cm"
          chips += Chip(s"cm_$i", found = true, s.label, Some(firstLine.trim), cls, "cm")
        }
      } else {
        chips += Chip("cm", found = true, "\uD83D\uDCCB CLAUDE.md", Some("system-reminder"), "cm", "cm")
      }
    }

    for ((cmd, i) <- det.slashCommands.zipWithIndex) {
      chips += Chip(s"sc_$i", found = true, s"\u2328 /${cmd.name}", None, "sc", "sc")
    }
    for ((sk, i) <- det.skills.zipWithIndex) {
      val skName = {
        val input = sk.input
        input.hcursor.downField("skill").as[String]
          .orElse(input.hcursor.downField("command").as[String])
          .getOrElse(s"Skill ${i + 1}")
      }
      chips += Chip(s"sk_$i", found = true, s"\uD83D\uDD27 $skName", None, "sk", "sk")
    }
    if (det.subAgents.nonEmpty) {
      val name = det.subAgents.headOption.map(_.name).getOrElse("Agent")
      chips += Chip("sa", found = true, s"\uD83E\uDD16 Sub-Agent", Some(name), "sa", "sa")
    }
    for ((mc, i) <- det.mcpTools.zipWithIndex) {
      val parts = mc.name.split("__")
      val toolName = if (parts.length > 2) parts.drop(2).mkString("__") else mc.name
      chips += Chip(s"mc_$i", found = true, s"\uD83D\uDD0C $toolName", None, "mc", "mc")
    }

    if (chips.isEmpty) return ""

    val activeChip = chips.find(c => AppState.proxyDetailMechFilter.contains(c.key))
    val meta = activeChip.flatMap(c => getChipMeta().get(c.metaKey))
    val descBanner = meta match {
      case Some(m) =>
        s"""<div class="mech-filter-desc">""" +
          s"""<span class="mech-filter-desc-dot" style="background:${m.color}"></span>""" +
          s"""<div class="mech-filter-desc-body">""" +
          s"""<span class="mech-filter-desc-who">${esc(m.who)}</span>""" +
          s"""<span class="mech-filter-desc-what">${esc(m.what)}</span>""" +
          "</div></div>"
      case None => ""
    }

    """<div style="flex-shrink:0;border-bottom:1px solid var(--border)">""" +
      """<div style="display:flex;flex-wrap:wrap;gap:5px;padding:5px 10px 7px">""" +
      chips.map { c =>
        val active = if (AppState.proxyDetailMechFilter.contains(c.key)) " active" else ""
        s"""<span class="mech-chip ${c.cls} found btn$active" data-key="${escAttr(c.key)}" onclick="setProxyDetailMechFilter(this.dataset.key)">${esc(c.label)}</span>"""
      }.mkString +
      "</div>" +
      descBanner +
      "</div>"
  }

  @JSExportTopLevel("buildMechFilterChips")
  def buildMechFilterChipsJs(body: js.Dynamic): String = buildMechFilterChips(body)
}
