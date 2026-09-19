package claudeproxymate.core.filter

import cats.syntax.all.*

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
      val keys    = if current.keys.contains(key) then current.keys.filterNot(_ === key) else current.keys :+ key
      cfg.withCategory(cat, current.copy(keys = keys))
    }

  /** Removes `key` in future requests: switches `KeepAll` to `RemoveSelected`
    * with that key, adds the key under `RemoveSelected` (idempotent), and
    * leaves `RemoveAll` alone because the key is already removed.
    */
  def removeKey(cat: FilterCategory, key: String): FilterConfig => FilterConfig =
    cfg => {
      val current = cfg.category(cat)
      current.mode match {
        case CategoryMode.KeepAll => cfg.withCategory(cat, CategoryFilter(CategoryMode.RemoveSelected, List(key)))
        case CategoryMode.RemoveSelected =>
          if (current.keys.contains(key)) cfg
          else cfg.withCategory(cat, current.copy(keys = current.keys :+ key))
        case CategoryMode.RemoveAll => cfg
      }
    }

  def addRule(kind: TextRuleKind): FilterConfig => FilterConfig =
    cfg => cfg.copy(textRules = cfg.textRules :+ TextRule.default.copy(kind = kind))

  /** Appends an enabled rule unless an identical (kind, pattern, scope) rule exists. */
  def appendRule(kind: TextRuleKind, pattern: String, scope: TextRuleScope): FilterConfig => FilterConfig =
    cfg =>
      if (cfg.textRules.exists(r => r.kind === kind && r.pattern === pattern && r.scope === scope)) cfg
      else cfg.copy(textRules = cfg.textRules :+ TextRule(kind, pattern, scope, enabled = true))

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
