package claudeproxymate.renderer.theme

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.i18n.I18n
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Theme system.
  *
  * Tracks the user's theme *selection* (one of "system", "light", "dark") in
  * localStorage under `ci-theme`. Resolves the selection to an effective theme
  * and applies it as `data-theme="light|dark"` on the root `<html>` element.
  *
  * When selection is "system", listens for OS-level `prefers-color-scheme`
  * changes and re-applies the effective theme live.
  *
  * Also swaps the header logo image between light and dark variants based on
  * the effective theme, and updates the toggle button's `data-theme-state`,
  * `aria-label`, and `title` to reflect the current selection.
  */
object Theme {

  private val StorageKey: String = "ci-theme"

  private val Selections: List[String] = List("system", "light", "dark")
  private val LogoLight: String        = "../assets/logo/claude-proxymate-icon-light.svg"
  private val LogoDark: String         = "../assets/logo/claude-proxymate-icon-dark.svg"

  private var _selection: String = readStoredSelection()

  private def readStoredSelection(): String = {
    val stored = dom.window.localStorage.getItem(StorageKey)
    if (stored != null && Selections.contains(stored)) stored else "system"
  }

  /** Current user selection: "system" | "light" | "dark". */
  def selection: String = _selection

  /** Effective theme in effect right now: "light" | "dark". */
  def effective: String =
    if (_selection == "system") systemEffective() else _selection

  private def systemEffective(): String = {
    val mql = dom.window.matchMedia("(prefers-color-scheme: dark)")
    if (mql.matches) "dark" else "light"
  }

  /** Initialize at app startup: apply stored selection and install the OS listener. */
  def init(): Unit = {
    apply()
    installSystemListener()
  }

  @JSExportTopLevel("toggleTheme")
  def toggle(): Unit = {
    val next = _selection match {
      case "system" => "light"
      case "light"  => "dark"
      case "dark"   => "system"
      case _        => "system"
    }
    setSelection(next)
  }

  @JSExportTopLevel("theme_setSelection")
  def setSelection(sel: String): Unit = {
    val normalized = if (Selections.contains(sel)) sel else "system"
    _selection = normalized
    dom.window.localStorage.setItem(StorageKey, normalized)
    apply()
  }

  /** Apply the current selection to the DOM: root attribute, logo, button state. */
  def apply(): Unit = {
    val eff = effective
    dom.document.documentElement.setAttribute("data-theme", eff)

    val logo = dom.document.getElementById(HtmlIds.LogoIcon)
    if (logo != null) {
      val img = logo.asInstanceOf[dom.html.Image]
      img.src = if (eff == "dark") LogoDark else LogoLight
    }

    val btn = dom.document.getElementById(HtmlIds.ThemeToggleBtn)
    if (btn != null) {
      val el = btn.asInstanceOf[dom.html.Element]
      el.setAttribute("data-theme-state", _selection)
      val label = _selection match {
        case "system" => I18n.t("theme.system")
        case "light"  => I18n.t("theme.light")
        case "dark"   => I18n.t("theme.dark")
        case _        => "Theme"
      }
      el.setAttribute("aria-label", label)
      el.setAttribute("title", label)
    }
  }

  private def installSystemListener(): Unit = {
    val mql = dom.window.matchMedia("(prefers-color-scheme: dark)")
    val listener: js.Function1[dom.Event, Unit] = { (_: dom.Event) =>
      if (_selection == "system") apply()
    }
    val dyn = mql.asInstanceOf[js.Dynamic]
    if (!js.isUndefined(dyn.addEventListener)) {
      locally { val _ = dyn.addEventListener("change", listener) }
    } else if (!js.isUndefined(dyn.addListener)) {
      locally { val _ = dyn.addListener(listener) }
    }
  }
}
