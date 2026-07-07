package claudeproxymate.renderer.analysis

import cats.syntax.all.*
import claudeproxymate.core.{ClaudeMdParser, MechanismDetector, Mechanisms}
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.JsJsonBridge
import org.scalajs.dom

import scala.scalajs.js

/** Mechanism filter chips and metadata.
  *
  * Owns the document-level click handler that toggles the active filter
  * via `setProxyDetailMechFilter`. The HTML building lives in
  * [[MechChipsView]].
  */
object MechChips {

  /** Install the document-level click handler for chip clicks. Called
    * from RendererMain.
    */
  def install(): Unit =
    dom.document.addEventListener("click", handleClick _)

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    val chipEl = target.closest(s".${MechChipsView.ChipClass}[${MechChipsView.ChipDataAttr}]")
    if (chipEl == null) return
    val key    = chipEl.asInstanceOf[dom.html.Element].getAttribute(MechChipsView.ChipDataAttr)
    if (key == null || key.isEmpty) return
    setProxyDetailMechFilter(key)
  }

  /** Bridge from JS `js.Dynamic` body to core `MechanismDetector`. */
  def detectMechanismsFromDynamic(body: js.Dynamic): Mechanisms = {
    val json = JsJsonBridge.toCirceJsonUnsafe(body)
    MechanismDetector.detectMechanisms(json)
  }

  /** Static i18n-sourced chip metadata (colour, who, what). */
  def getChipMeta(): Map[String, ChipMeta] = Map(
    "cm" -> ChipMeta("var(--green)", I18n.t("mechDesc.cm.who"), I18n.t("mechDesc.cm.what")),
    "sc" -> ChipMeta("var(--yellow)", I18n.t("mechDesc.sc.who"), I18n.t("mechDesc.sc.what")),
    "sk" -> ChipMeta("var(--purple)", I18n.t("mechDesc.sk.who"), I18n.t("mechDesc.sk.what")),
    "sa" -> ChipMeta("var(--orange)", I18n.t("mechDesc.sa.who"), I18n.t("mechDesc.sa.what")),
    "mc" -> ChipMeta("var(--cyan)", I18n.t("mechDesc.mc.who"), I18n.t("mechDesc.mc.what")),
  )

  /** Toggle the mechanism filter. Called by the click handler. */
  def setProxyDetailMechFilter(key: String): Unit = {
    AppState.proxyDetailMechFilter = AppState.proxyDetailMechFilter match {
      case Some(k) if k === key => none[String]
      case Some(_) | None => key.some
    }
    claudeproxymate.renderer.detail.DetailView.renderProxyDetail()
  }

  /** Build the mechanism filter chips HTML. Returns empty string when no
    * chips. Caller (AnalysisRenderer / DetailView) concatenates the
    * result into a String template — keep the return type stable until
    * those panels migrate (A3g/A3h).
    */
  def buildMechFilterChips(body: js.Dynamic): String = {
    val det       = detectMechanismsFromDynamic(body)
    val chips     = collectChips(det)
    if (chips.isEmpty) return ""
    val activeKey = AppState.proxyDetailMechFilter
    val descMeta  = activeKey
      .flatMap(k => chips.find(_.key === k))
      .flatMap(c => getChipMeta().get(c.metaKey))
    MechChipsView.buildChipsFrag(chips, activeKey, descMeta).render
  }

  private def collectChips(det: Mechanisms): List[Chip] = {
    val buf = scala.collection.mutable.ListBuffer.empty[Chip]

    det.claudeMd.foreach { claudeMd =>
      val sections = ClaudeMdParser.parseClaudeMdSections(claudeMd)
      if (sections.nonEmpty) {
        for ((s, i) <- sections.zipWithIndex) {
          val cls = if (s.cls === "cyan") "st" else "cm"
          buf += Chip(s"cm_$i", s.label, cls, "cm")
        }
      } else {
        buf += Chip("cm", "📋 CLAUDE.md", "cm", "cm")
      }
    }

    for ((cmd, i) <- det.slashCommands.zipWithIndex) {
      buf += Chip(s"sc_$i", s"⌨ /${cmd.name}", "sc", "sc")
    }

    for ((sk, i) <- det.skills.zipWithIndex) {
      val skName = sk
        .input
        .hcursor
        .downField("skill")
        .as[String]
        .orElse(sk.input.hcursor.downField("command").as[String])
        .getOrElse(s"Skill ${i + 1}")
      buf += Chip(s"sk_$i", s"🔧 $skName", "sk", "sk")
    }

    if (det.subAgents.nonEmpty) {
      buf += Chip("sa", "🤖 Sub-Agent", "sa", "sa")
    }

    for ((mc, i) <- det.mcpTools.zipWithIndex) {
      val parts    = mc.name.split("__")
      val toolName = if (parts.length > 2) parts.drop(2).mkString("__") else mc.name
      buf += Chip(s"mc_$i", s"🔌 $toolName", "mc", "mc")
    }

    buf.toList
  }
}
