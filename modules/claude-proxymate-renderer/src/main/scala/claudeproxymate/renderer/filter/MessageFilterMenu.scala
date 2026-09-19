package claudeproxymate.renderer.filter

import cats.syntax.all.*
import claudeproxymate.core.HtmlIds
import claudeproxymate.core.filter.*
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.messages.{BadgeFilter, BadgeToggle, MessageView}
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.view.ViewHelpers
import org.scalajs.dom

/** DOM sibling of [[MessageFilterView]]: opens the badge popover, tracks a
  * text selection inside a typed part, and dispatches every inline filter
  * action to [[RequestFilterSheet.applyAndSave]].
  *
  * Document-level delegation like every other feature object. Badge clicks
  * are routed here by `MessageRenderer` through [[onBadgeClick]].
  */
object MessageFilterMenu {

  private var openMenuUid: Option[String]                 = none[String]
  private var selection: Option[(String, Option[String])] = none[(String, Option[String])]
  private var selKind: TextRuleKind                       = TextRuleKind.Text

  def install(): Unit = {
    dom.document.addEventListener("click", handleClick(_))
    dom.document.addEventListener("mouseup", handleMouseUp(_))
    dom.document.addEventListener("keydown", handleKeydown(_))
  }

  /** A click on `.msg-badge[data-msg-badge-uid]`: badges without a filter key
    * expand as before, keyed badges toggle their popover.
    */
  def onBadgeClick(uid: String, badgeEl: dom.html.Element): Unit =
    if (badgeEl.getAttribute(MessageView.BadgeFilterAttr) == null) BadgeToggle.toggleBadge(uid)
    else if (openMenuUid.contains(uid)) closeMenu()
    else openMenu(uid, badgeEl)

  private def badgeFilterOf(badgeEl: dom.html.Element): Option[BadgeFilter] = {
    val kind = attrOf(badgeEl, MessageView.BadgeFilterAttr)
    if (kind === MessageView.BadgeFilterSkills) BadgeFilter.SkillsReminder.some
    else if (kind === MessageView.BadgeFilterItem) {
      val key = attrOf(badgeEl, RequestFilterView.KeyAttr)
      FilterCategory
        .parse(attrOf(badgeEl, RequestFilterView.CatAttr))
        .filter(_ => key.nonEmpty)
        .map(c => BadgeFilter.Item(c, key))
    } else none[BadgeFilter]
  }

  private def openMenu(uid: String, badgeEl: dom.html.Element): Unit = {
    closeMenu()
    closeSelectionMenu()
    badgeFilterOf(badgeEl) match {
      case None => BadgeToggle.toggleBadge(uid)
      case Some(filter) =>
        val content  = dom.document.getElementById(s"bc_$uid")
        val expanded = content != null && content.asInstanceOf[dom.html.Element].style.display =!= "none"
        val model    =
          BadgeMenuModel(uid, filter, MessageView.badgeState(AppState.filterConfig, filter), expanded, menuLabels())
        val holder   = dom.document.createElement("div")
        ViewHelpers.setInnerHtml(holder, MessageFilterView.buildBadgeMenuFrag(model))
        val menu     = holder.firstElementChild
        if (menu != null && badgeEl.parentNode != null) {
          locally { val _ = badgeEl.parentNode.appendChild(menu) }
          openMenuUid = uid.some
        }
    }
  }

  private def closeMenu(): Unit = {
    val el = dom.document.getElementById(HtmlIds.BadgeMenu)
    if (el != null) locally { val _ = el.parentNode.removeChild(el) }
    openMenuUid = none[String]
  }

  private def closeSelectionMenu(): Unit = {
    val el = dom.document.getElementById(HtmlIds.SelectionMenu)
    if (el != null) locally { val _ = el.parentNode.removeChild(el) }
    selection = none[(String, Option[String])]
  }

  private def attrOf(el: dom.Element, name: String): String = {
    val v = el.getAttribute(name)
    if (v == null) "" else v
  }

  private def handleClick(e: dom.MouseEvent): Unit =
    Option(e.target.asInstanceOf[dom.Element]).foreach { target =>
      val inside =
        target.closest(s".${MessageFilterView.MenuClass}") != null ||
          target.closest(s".${MessageFilterView.SelMenuClass}") != null ||
          target.closest(s".${MessageFilterView.BarClass}") != null ||
          target.closest(s".${MessageView.SkillXClass}") != null
      if (inside) {
        Option(target.closest(s"[${MessageFilterView.ActionAttr}]"))
          .foreach(el => dispatch(el.asInstanceOf[dom.html.Element]))
      } else if (target.closest(s".${MessageView.BadgeClass}[${MessageView.BadgeDataAttr}]") != null) {
        () // MessageRenderer routes badge clicks to onBadgeClick
      } else {
        closeMenu()
        closeSelectionMenu()
      }
    }

  private def dispatch(el: dom.html.Element): Unit = {
    import MessageFilterView.Action
    val action = attrOf(el, MessageFilterView.ActionAttr)
    val cat    = FilterCategory.parse(attrOf(el, RequestFilterView.CatAttr))
    val key    = attrOf(el, RequestFilterView.KeyAttr)
    val value  = attrOf(el, RequestFilterView.ValueAttr)

    action match {
      case Action.BadgeRemove =>
        closeMenu()
        cat.foreach(c => if (key.nonEmpty) RequestFilterSheet.applyAndSave(FilterConfigEdits.removeKey(c, key)) else ())
      case Action.BadgeRestore =>
        closeMenu()
        cat.foreach(c => if (key.nonEmpty) RequestFilterSheet.applyAndSave(FilterConfigEdits.toggleKey(c, key)) else ())
      case Action.BadgeRemoveAll =>
        closeMenu()
        cat.foreach(c => RequestFilterSheet.applyAndSave(FilterConfigEdits.setMode(c, CategoryMode.RemoveAll)))
      case Action.BadgeExpand =>
        closeMenu()
        if (value.nonEmpty) BadgeToggle.toggleBadge(value) else ()
      case Action.ToggleSkill =>
        if (key.nonEmpty)
          RequestFilterSheet.applyAndSave { cfg =>
            if (cfg.skills.mode === CategoryMode.RemoveAll) cfg
            else if (cfg.skills.remove(key)) FilterConfigEdits.toggleKey(FilterCategory.Skills, key)(cfg)
            else FilterConfigEdits.removeKey(FilterCategory.Skills, key)(cfg)
          }
        else ()
      case Action.SelKind =>
        TextRuleKind.parse(value).foreach { k =>
          selKind = k
          rerenderSelectionMenu()
        }
      case Action.SelAdd =>
        selection
          .flatMap { case (text, tag) => MessageFilterView.ruleFor(selKind, text, tag) }
          .foreach {
            case (k, p) => RequestFilterSheet.applyAndSave(FilterConfigEdits.appendRule(k, p, TextRuleScope.Both))
          }
        closeSelectionMenu()
      case Action.BarEnabled =>
        RequestFilterSheet.applyAndSave(FilterConfigEdits.setEnabled(!AppState.filterConfig.enabled))
      case Action.BarAddRule => RequestFilterSheet.openWithNewRule(TextRuleKind.Text)
      case Action.BarManage => RequestFilterSheet.open()
      case _ => ()
    }
  }

  // ── selection ──

  private def typedAncestor(node: dom.Node): Option[dom.Element] = {
    val el: dom.Element = node match {
      case e: dom.Element => e
      case other => other.parentNode.asInstanceOf[dom.Element]
    }
    Option(el).flatMap(e => Option(e.closest(".msg-typed")))
  }

  private def handleMouseUp(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target != null && target.closest(s".${MessageFilterView.SelMenuClass}") != null) return
    val sel    = dom.window.getSelection()
    if (sel == null || sel.isCollapsed || sel.rangeCount === 0 || sel.toString.trim.isEmpty) {
      closeSelectionMenu()
      return
    }
    (typedAncestor(sel.anchorNode), typedAncestor(sel.focusNode)) match {
      case (Some(a), Some(f)) if a eq f =>
        val text = sel.toString
        val tag  = MessageFilterView.tagNameOf(text)
        selection = (text, tag).some
        selKind = if (tag.isDefined) TextRuleKind.Tag else TextRuleKind.Text
        closeMenu()
        showSelectionMenu(a.asInstanceOf[dom.html.Element], sel.getRangeAt(0))
      case (Some(_), Some(_)) | (Some(_), None) | (None, Some(_)) | (None, None) => closeSelectionMenu()
    }
  }

  private def showSelectionMenu(typedEl: dom.html.Element, range: dom.Range): Unit = {
    val existing  = dom.document.getElementById(HtmlIds.SelectionMenu)
    if (existing != null) locally { val _ = existing.parentNode.removeChild(existing) }
    val rect      = range.getBoundingClientRect()
    val typedRect = typedEl.getBoundingClientRect()
    val left      = math.max(0.0, rect.left - typedRect.left)
    val top       = math.max(0.0, rect.bottom - typedRect.top + 4)
    val holder    = dom.document.createElement("div")
    ViewHelpers.setInnerHtml(holder, MessageFilterView.buildSelectionMenuFrag(selectionModel()))
    val menu      = holder.firstElementChild.asInstanceOf[dom.html.Element]
    if (menu != null) {
      menu.style.left = s"${left}px"
      menu.style.top = s"${top}px"
      locally { val _ = typedEl.appendChild(menu) }
    }
  }

  private def rerenderSelectionMenu(): Unit = {
    val existing = dom.document.getElementById(HtmlIds.SelectionMenu)
    if (existing == null) return
    val el       = existing.asInstanceOf[dom.html.Element]
    val holder   = dom.document.createElement("div")
    ViewHelpers.setInnerHtml(holder, MessageFilterView.buildSelectionMenuFrag(selectionModel()))
    val menu     = holder.firstElementChild.asInstanceOf[dom.html.Element]
    if (menu != null) {
      menu.style.left = el.style.left
      menu.style.top = el.style.top
      locally { val _ = el.parentNode.replaceChild(menu, el) }
    }
  }

  private def selectionModel(): SelectionMenuModel =
    SelectionMenuModel(
      selKind,
      selection.flatMap { case (_, tag) => tag },
      SelectionLabels(
        title = I18n.t("filter.selTitle"),
        kindText = I18n.t("filter.kindText"),
        kindRegex = I18n.t("filter.kindRegex"),
        kindTag = I18n.t("filter.selTag"),
        add = I18n.t("filter.selAdd"),
      ),
    )

  private def menuLabels(): MenuLabels =
    MenuLabels(
      remove = I18n.t("filter.menuRemove"),
      restore = I18n.t("filter.menuRestore"),
      removeAllRules = I18n.t("filter.menuRemoveAllRules"),
      removeAllDocs = I18n.t("filter.menuRemoveAllDocs"),
      removeAllSkills = I18n.t("filter.menuRemoveAllSkills"),
      expand = I18n.t("filter.menuExpand"),
      collapse = I18n.t("filter.menuCollapse"),
    )

  private def handleKeydown(e: dom.KeyboardEvent): Unit =
    if (e.key === "Escape") {
      closeMenu()
      closeSelectionMenu()
    } else ()
}
