package claudeproxymate.renderer.i18n

import cats.syntax.all.*
import claudeproxymate.core.HtmlIds
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Internationalization system.
  *
  * Ports the `i18n` object and `applyI18n()` from renderer.js.
  * Translations are loaded at runtime from `i18n/{locale}.json` files via `fetch`.
  */
object I18n {

  private var _locale: String = {
    val stored = dom.window.localStorage.getItem("ci-lang")
    if (stored != null && stored.nonEmpty) stored
    else if (dom.window.navigator.language.startsWith("ko")) "ko"
    else "en"
  }

  private var _translations: Map[String, Map[String, String]] = Map.empty

  def locale: String = _locale

  /** Load locale JSON files via fetch and populate `_translations`. */
  def loadLocales(): js.Promise[Unit] = {
    val locales  = List("en", "ko")
    val promises = locales.map { lang =>
      dom
        .window
        .fetch(s"i18n/$lang.json")
        .`then`[js.Dynamic] { (r: dom.Response) => r.json().asInstanceOf[js.Promise[js.Dynamic]] }
        .`then`[Unit] { (obj: js.Dynamic) =>
          val dict = obj.asInstanceOf[js.Dictionary[String]]
          val map  = dict.toMap
          _translations = _translations.updated(lang, map)
        }
    }
    js.Promise.all(js.Array(promises*)).`then`[Unit] { (_: js.Array[Unit]) => () }
  }

  /** Translate a dot-separated key with optional variable substitution. */
  @JSExportTopLevel("i18n_t")
  def t(key: String, vars: Map[String, String] = Map.empty): String = {
    val translations = _translations.getOrElse(_locale, _translations.getOrElse("en", Map.empty))
    val value        = translations.getOrElse(key, key)
    vars.foldLeft(value) { case (s, (k, v)) => s.replace(s"{$k}", v) }
  }

  /** Install the doc-level click listener that dispatches the
    * `#langToggleBtn` button. Replaces the inline
    * `onclick="toggleLocale()"` in the generated `index.html`.
    */
  def install(): Unit =
    dom.document.addEventListener("click", handleClick(_))

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    if (target.closest(s"#${HtmlIds.LangToggleBtn}") != null) toggleLocale()
  }

  def toggleLocale(): Unit =
    setLocale(if (_locale === "ko") "en" else "ko")

  @JSExportTopLevel("i18n_setLocale")
  def setLocale(lang: String): Unit = {
    _locale = lang
    dom.window.localStorage.setItem("ci-lang", lang)
    applyI18n()
  }

  /** Apply translations to all elements with data-i18n* attributes. */
  def applyI18n(): Unit = {
    dom.document.querySelectorAll("[data-i18n]").foreach { node =>
      val el  = node.asInstanceOf[dom.HTMLElement]
      val key = el.dataset.get("i18n").getOrElse("")
      if (key.nonEmpty) el.textContent = t(key)
    }
    dom.document.querySelectorAll("[data-i18n-html]").foreach { node =>
      val el  = node.asInstanceOf[dom.HTMLElement]
      val key = el.dataset.get("i18nHtml").getOrElse("")
      if (key.nonEmpty) el.innerHTML = t(key)
    }
    dom.document.querySelectorAll("[data-i18n-placeholder]").foreach { node =>
      val el  = node.asInstanceOf[dom.html.Input]
      val key = el.dataset.get("i18nPlaceholder").getOrElse("")
      if (key.nonEmpty) el.placeholder = t(key)
    }
    dom.document.querySelectorAll("[data-i18n-title]").foreach { node =>
      val el  = node.asInstanceOf[dom.HTMLElement]
      val key = el.dataset.get("i18nTitle").getOrElse("")
      if (key.nonEmpty) el.title = t(key)
    }
    dom.document.querySelectorAll("[data-i18n-alt]").foreach { node =>
      val el  = node.asInstanceOf[dom.HTMLElement]
      val key = el.dataset.get("i18nAlt").getOrElse("")
      if (key.nonEmpty) el.asInstanceOf[dom.html.Image].alt = t(key)
    }

    // lang toggle button text
    val btn = dom.document.getElementById(HtmlIds.LangToggleBtn)
    if (btn != null) btn.textContent = if (_locale === "ko") "\ud55c" else "EN"

    // re-render proxy content that depends on locale
    claudeproxymate.renderer.proxy.ProxyList.renderProxyList()
    claudeproxymate.renderer.detail.DetailView.renderProxyDetail()
    // Re-render proxy status to correct the text overwritten by the generic data-i18n loop above.
    // The status element has data-i18n="proxy.stopped" as a default, but when the proxy is running
    // it needs to show "Running on port {port}" (or the Korean equivalent) instead.
    claudeproxymate.renderer.proxy.ProxyControl.renderProxyStatus()
    claudeproxymate.renderer.theme.Theme.apply()
  }
}
