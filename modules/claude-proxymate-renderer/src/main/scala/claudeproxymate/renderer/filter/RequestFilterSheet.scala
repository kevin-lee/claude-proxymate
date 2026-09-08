package claudeproxymate.renderer.filter

import cats.syntax.all.*
import claudeproxymate.core.{ClaudeMdParser, HtmlIds, RequestAnatomy}
import claudeproxymate.core.filter.*
import claudeproxymate.renderer.facades.ElectronApi
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.util.JsJsonBridge
import claudeproxymate.renderer.view.ViewHelpers
import io.circe.Json
import io.circe.syntax.*
import org.scalajs.dom

import scala.scalajs.js

/** DOM sibling of [[RequestFilterView]]: opens the sheet from the address-bar
  * button, applies [[FilterConfigEdits]] to a draft, previews the draft against
  * the last captured request, and saves through the Electron bridge.
  *
  * Uses document-level delegation (like every other feature object) so the
  * listeners survive re-rendering the card.
  */
object RequestFilterSheet {

  private var draft: FilterConfig                            = FilterConfig.default
  private var disk: List[InventoryItem]                      = Nil
  private var traffic: List[InventoryItem]                   = Nil
  private var previewBody: Option[Json]                      = none[Json]
  private var previewReport: Option[FilterReport]            = none[FilterReport]
  private var isOpen: Boolean                                = false
  private var trafficCache: Map[Double, List[InventoryItem]] = Map.empty

  private val PreviewCaptures: Int = 10

  def install(): Unit = {
    dom.document.addEventListener("click", handleClick(_))
    dom.document.addEventListener("input", handleInput(_))
    dom.document.addEventListener("keydown", handleKeydown(_))
  }

  /** Load the persisted config into `AppState.filterConfig` and paint the button. */
  def loadConfig(): Unit =
    ElectronApi.get.foreach { api =>
      api
        .filterConfigGet()
        .`then`[Unit] { (raw: js.Dynamic) =>
          decodeConfig(raw).foreach(cfg => AppState.filterConfig = cfg)
          renderButton()
        }
        .asInstanceOf[js.Dynamic]
        .`catch`({ (_: Any) => renderButton() }: js.Function1[Any, Unit])
    }

  private def decodeConfig(raw: js.Dynamic): Option[FilterConfig] =
    JsJsonBridge.toCirceJson(raw).toOption.flatMap(_.as[FilterConfig].toOption)

  def open(): Unit = {
    draft = AppState.filterConfig
    val previewEntry = currentPreviewEntry()
    previewBody = previewEntry.flatMap(entryBody)
    previewReport = previewEntry.flatMap(entryReport)
    traffic = trafficItems()
    isOpen = true
    modalEl.foreach(_.style.display = "flex")
    render()
    ElectronApi.get.foreach { api =>
      api
        .claudeInventoryScan()
        .`then`[Unit] { (result: js.Dynamic) =>
          disk = parseDisk(result)
          if (isOpen) render() else ()
        }
        .asInstanceOf[js.Dynamic]
        .`catch`({ (_: Any) => () }: js.Function1[Any, Unit])
    }
  }

  def close(): Unit = {
    isOpen = false
    modalEl.foreach(_.style.display = "none")
  }

  def rerenderIfOpen(): Unit = if (isOpen) render() else ()

  def render(): Unit =
    cardEl.foreach { card =>
      ViewHelpers.setInnerHtml(card, RequestFilterView.buildCardFrag(model()))
    }

  /** Update only the preview line (keeps the focused pattern input alive). */
  private def refreshPreview(): Unit = {
    val el = dom.document.getElementById(HtmlIds.RequestFilterPreview)
    if (el != null) el.textContent = RequestFilterView.buildPreviewText(labels(), previewOf(draft)._1) else ()
  }

  /** Paint the address-bar button: rule count badge and active state. */
  def renderButton(): Unit = {
    val (count, active) = RequestFilterView.buttonState(AppState.filterConfig)
    val btn             = dom.document.getElementById(HtmlIds.RequestFilterBtn)
    val countEl         = dom.document.getElementById(HtmlIds.RequestFilterCount)
    if (btn != null) {
      locally { val _ = btn.classList.toggle("active", active) }
    } else ()
    if (countEl != null) {
      countEl.textContent = count.toString
      locally { val _ = countEl.classList.toggle("u-hide", count === 0) }
    } else ()
  }

  private def save(): Unit =
    ElectronApi.get.foreach { api =>
      api
        .filterConfigSet(JsJsonBridge.toJsDynamic(draft.asJson))
        .`then`[Unit] { (result: js.Dynamic) =>
          val ok = result.selectDynamic("ok")
          if (!js.isUndefined(ok) && ok.asInstanceOf[Boolean]) {
            AppState.filterConfig = draft
            renderButton()
            close()
          } else {
            val reason = result.selectDynamic("reason")
            val text   = if (js.isUndefined(reason) || reason == null) "" else reason.toString
            dom.window.alert(I18n.t("filter.saveFail", Map("reason" -> text)))
          }
        }
        .asInstanceOf[js.Dynamic]
        .`catch`({ (e: Any) =>
          dom.window.alert(I18n.t("filter.saveFail", Map("reason" -> e.toString)))
        }: js.Function1[Any, Unit])
    }

  // ── model ──

  private def model(): SheetModel = {
    val merged                   = FilterInventory.merge(disk, traffic, draft)
    val (preview, hits, skipped) = previewOf(draft)
    SheetModel(
      draft = draft,
      rules = merged.filter(_.item.category === FilterCategory.Rules),
      docs = merged.filter(_.item.category === FilterCategory.Docs),
      skills = merged.filter(_.item.category === FilterCategory.Skills),
      preview = preview,
      hitsByPattern = hits,
      skipped = skipped,
      labels = labels(),
    )
  }

  /** `(preview tokens, text-rule hits per pattern, skipped reasons)` for `cfg`.
    *
    * A capture holds the body that was actually forwarded, so the "before" size
    * comes from the report the proxy attached to it (the request as Claude Code
    * sent it) and the "after" size from applying the draft on top of that body.
    * Rules the draft drops cannot restore content the saved config already cut,
    * so the preview is exact for additions and a floor for removals.
    */
  private def previewOf(cfg: FilterConfig): (Option[(Int, Int)], Map[String, Int], List[String]) =
    previewBody match {
      case None => (none[(Int, Int)], Map.empty, Nil)
      case Some(body) =>
        val bodyBytes    = TextRules.byteLen(body.noSpaces)
        val outcome      = RequestFilter(cfg, body)
        val from         = previewReport.map(_.originalBytes).getOrElse(bodyBytes)
        val to           = outcome.report.map(_.filteredBytes).getOrElse(bodyBytes)
        val hitsOf       = (items: List[RemovedItem]) =>
          items.filter(_.category === RemovedCategory.Text).groupBy(_.key).map { case (k, xs) => k -> xs.length }
        val hitsCaptured = previewReport.map(r => hitsOf(r.removed)).getOrElse(Map.empty)
        val hitsDraft    = outcome.report.map(r => hitsOf(r.removed)).getOrElse(Map.empty)
        val hits         = (hitsCaptured.keySet ++ hitsDraft.keySet).map { k =>
          k -> math.max(hitsCaptured.getOrElse(k, 0), hitsDraft.getOrElse(k, 0))
        }.toMap
        (
          (RequestAnatomy.estTokens(from), RequestAnatomy.estTokens(to)).some,
          hits,
          outcome.report.map(_.skipped).getOrElse(Nil),
        )
    }

  private def entryBody(entry: js.Dynamic): Option[Json] = {
    val body = entry.selectDynamic("body")
    if (js.isUndefined(body) || body == null) none[Json]
    else JsJsonBridge.toCirceJson(body).toOption
  }

  private def entryReport(entry: js.Dynamic): Option[FilterReport] = {
    val filter = entry.selectDynamic("filter")
    if (js.isUndefined(filter) || filter == null) none[FilterReport]
    else JsJsonBridge.toCirceJson(filter).toOption.flatMap(_.as[FilterReport].toOption)
  }

  private def currentPreviewEntry(): Option[js.Dynamic] = {
    val selected = AppState.selectedProxyId.flatMap { id =>
      AppState.proxyCaptures.find(e => e.selectDynamic("id").asInstanceOf[Double] === id)
    }
    selected.orElse(AppState.proxyCaptures.headOption)
  }

  private def trafficItems(): List[InventoryItem] = {
    val recent = AppState.proxyCaptures.take(PreviewCaptures)
    val items  = recent.flatMap { entry =>
      val id = entry.selectDynamic("id").asInstanceOf[Double]
      trafficCache.get(id) match {
        case Some(cached) => cached
        case None =>
          val found = entryBody(entry).map(FilterInventory.fromBody).getOrElse(Nil)
          trafficCache = trafficCache.updated(id, found)
          found
      }
    }
    items.distinctBy(i => (i.category, i.key))
  }

  /* Everything the disk scan finds lives under ~/.claude, i.e. it is global. */
  private val GlobalDesc: String = "user's private global instructions for all projects"

  /** Disk items get the labels the Messages badges (and traffic items) use, so a
    * row reads the same whether it was found on disk or seen in a request.
    */
  private def diskLabel(cat: FilterCategory, key: String, scanLabel: String): String =
    cat match {
      case FilterCategory.Rules => ClaudeMdParser.label(key, GlobalDesc)._1
      case FilterCategory.Docs =>
        if (key.endsWith("CLAUDE.md")) ClaudeMdParser.label(key, GlobalDesc)._1 else s"🧠 Memory: $scanLabel"
      case FilterCategory.Skills => s"🔧 $scanLabel"
    }

  private def parseDisk(result: js.Dynamic): List[InventoryItem] = {
    def items(field: String, cat: FilterCategory): List[InventoryItem] = {
      val arr = result.selectDynamic(field)
      if (!js.Array.isArray(arr)) Nil
      else
        arr.asInstanceOf[js.Array[js.Dynamic]].toList.flatMap { e =>
          val key   = e.selectDynamic("key")
          val label = e.selectDynamic("label")
          if (js.isUndefined(key) || key == null) Nil
          else {
            val scanLabel = if (js.isUndefined(label) || label == null) key.toString else label.toString
            List(InventoryItem(cat, key.toString, diskLabel(cat, key.toString, scanLabel), 0))
          }
        }
    }
    items("rules", FilterCategory.Rules) ++ items("docs", FilterCategory.Docs) ++ items("skills", FilterCategory.Skills)
  }

  private def labels(): SheetLabels =
    SheetLabels(
      title = I18n.t("filter.title"),
      sub = I18n.t("filter.sub"),
      enabled = I18n.t("filter.enabled"),
      sectionRules = I18n.t("filter.sectionRules"),
      sectionDocs = I18n.t("filter.sectionDocs"),
      sectionSkills = I18n.t("filter.sectionSkills"),
      sectionText = I18n.t("filter.sectionText"),
      found = I18n.t("filter.found"),
      noneFound = I18n.t("filter.noneFound"),
      modeKeepAll = I18n.t("filter.modeKeepAll"),
      modeRemoveSelected = I18n.t("filter.modeRemoveSelected"),
      modeRemoveAll = I18n.t("filter.modeRemoveAll"),
      addRule = I18n.t("filter.addRule"),
      kindText = I18n.t("filter.kindText"),
      kindRegex = I18n.t("filter.kindRegex"),
      kindTag = I18n.t("filter.kindTag"),
      scopeTyped = I18n.t("filter.scopeTyped"),
      scopeAssistant = I18n.t("filter.scopeAssistant"),
      scopeBoth = I18n.t("filter.scopeBoth"),
      patternPlaceholder = I18n.t("filter.patternPlaceholder"),
      regexPlaceholder = I18n.t("filter.regexPlaceholder"),
      tagPlaceholder = I18n.t("filter.tagPlaceholder"),
      hits = I18n.t("filter.hits"),
      sourceDisk = I18n.t("filter.sourceDisk"),
      sourceTraffic = I18n.t("filter.sourceTraffic"),
      sourceAbsent = I18n.t("filter.sourceAbsent"),
      absentNote = I18n.t("filter.absentNote"),
      hintRegex = I18n.t("filter.hintRegex"),
      hintCache = I18n.t("filter.hintCache"),
      skipped = I18n.t("filter.skipped"),
      preview = I18n.t("filter.preview"),
      previewNone = I18n.t("filter.previewNone"),
      cancel = I18n.t("filter.cancel"),
      save = I18n.t("filter.save"),
    )

  // ── DOM ──

  private def modalEl: Option[dom.html.Element] =
    Option(dom.document.getElementById(HtmlIds.RequestFilterModal)).map(_.asInstanceOf[dom.html.Element])

  private def cardEl: Option[dom.html.Element] =
    Option(dom.document.getElementById(HtmlIds.RequestFilterCard)).map(_.asInstanceOf[dom.html.Element])

  private def update(edit: FilterConfig => FilterConfig): Unit = {
    draft = edit(draft)
    render()
  }

  private def attrOf(el: dom.html.Element, name: String): String = {
    val v = el.getAttribute(name)
    if (v == null) "" else v
  }

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target   = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    if (target.closest(s"#${HtmlIds.RequestFilterBtn}") != null) {
      open()
      return
    }
    if (!isOpen) return
    if (modalEl.exists(_ eq target)) {
      close()
      return
    }
    val actionEl = target.closest(s"[${RequestFilterView.ActionAttr}]")
    if (actionEl == null) return
    val el       = actionEl.asInstanceOf[dom.html.Element]
    val action   = attrOf(el, RequestFilterView.ActionAttr)
    val cat      = FilterCategory.parse(attrOf(el, RequestFilterView.CatAttr))
    val key      = attrOf(el, RequestFilterView.KeyAttr)
    val idx      = attrOf(el, RequestFilterView.IdxAttr).toIntOption
    val value    = attrOf(el, RequestFilterView.ValueAttr)

    import RequestFilterView.Action
    action match {
      case Action.Enabled => update(FilterConfigEdits.setEnabled(!draft.enabled))
      case Action.Mode =>
        (cat, CategoryMode.parse(value)) match {
          case (Some(c), Some(m)) => update(FilterConfigEdits.setMode(c, m))
          case _ => ()
        }
      case Action.ToggleItem =>
        cat.foreach(c => if (key.nonEmpty) update(FilterConfigEdits.toggleKey(c, key)) else ())
      case Action.AddRule =>
        update(FilterConfigEdits.addRule(TextRuleKind.parse(value).getOrElse(TextRuleKind.Text)))
      case Action.RemoveRule => idx.foreach(i => update(FilterConfigEdits.removeRule(i)))
      case Action.RuleKind =>
        (idx, TextRuleKind.parse(value)) match {
          case (Some(i), Some(k)) => update(FilterConfigEdits.setRuleKind(i, k))
          case _ => ()
        }
      case Action.RuleScope =>
        (idx, TextRuleScope.parse(value)) match {
          case (Some(i), Some(s)) => update(FilterConfigEdits.setRuleScope(i, s))
          case _ => ()
        }
      case Action.RuleEnabled =>
        idx.foreach { i =>
          val current = draft.textRules.lift(i).exists(_.enabled)
          update(FilterConfigEdits.setRuleEnabled(i, !current))
        }
      case Action.Save => save()
      case Action.Cancel => close()
      case _ => ()
    }
  }

  private def handleInput(e: dom.Event): Unit = {
    if (!isOpen) return
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    val el     = target.asInstanceOf[dom.html.Element]
    if (attrOf(el, RequestFilterView.ActionAttr) =!= RequestFilterView.Action.RulePattern) return
    attrOf(el, RequestFilterView.IdxAttr).toIntOption.foreach { i =>
      draft = FilterConfigEdits.setRulePattern(i, el.asInstanceOf[dom.html.Input].value)(draft)
      refreshPreview()
    }
  }

  private def handleKeydown(e: dom.KeyboardEvent): Unit =
    if (isOpen && e.key === "Escape") {
      e.preventDefault()
      close()
    } else ()
}
