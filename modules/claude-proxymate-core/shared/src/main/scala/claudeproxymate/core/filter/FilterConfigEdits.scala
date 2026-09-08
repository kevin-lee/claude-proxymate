package claudeproxymate.core.filter

/** Pure edits the Request Filter sheet applies to its draft config.
  * Out-of-range rule indices are no-ops.
  */
object FilterConfigEdits {

  def setEnabled(on: Boolean): FilterConfig => FilterConfig =
    _.copy(enabled = on)

  def setMode(cat: FilterCategory, mode: CategoryMode): FilterConfig => FilterConfig =
    cfg => cfg.withCategory(cat, cfg.category(cat).copy(mode = mode))

  def toggleKey(cat: FilterCategory, key: String): FilterConfig => FilterConfig =
    cfg => {
      val current = cfg.category(cat)
      val keys    = if (current.keys.contains(key)) current.keys.filterNot(_ == key) else current.keys :+ key
      cfg.withCategory(cat, current.copy(keys = keys))
    }

  def addRule(kind: TextRuleKind): FilterConfig => FilterConfig =
    cfg => cfg.copy(textRules = cfg.textRules :+ TextRule.default.copy(kind = kind))

  def removeRule(idx: Int): FilterConfig => FilterConfig =
    cfg =>
      if (idx < 0 || idx >= cfg.textRules.length) cfg
      else cfg.copy(textRules = cfg.textRules.patch(idx, Nil, 1))

  def setRuleKind(idx: Int, kind: TextRuleKind): FilterConfig => FilterConfig =
    updateRule(idx, _.copy(kind = kind))

  def setRuleScope(idx: Int, scope: TextRuleScope): FilterConfig => FilterConfig =
    updateRule(idx, _.copy(scope = scope))

  def setRulePattern(idx: Int, pattern: String): FilterConfig => FilterConfig =
    updateRule(idx, _.copy(pattern = pattern))

  def setRuleEnabled(idx: Int, on: Boolean): FilterConfig => FilterConfig =
    updateRule(idx, _.copy(enabled = on))

  private def updateRule(idx: Int, f: TextRule => TextRule): FilterConfig => FilterConfig =
    cfg =>
      if (idx < 0 || idx >= cfg.textRules.length) cfg
      else cfg.copy(textRules = cfg.textRules.updated(idx, f(cfg.textRules(idx))))
}
