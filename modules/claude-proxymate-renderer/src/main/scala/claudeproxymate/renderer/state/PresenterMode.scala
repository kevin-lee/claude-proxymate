package claudeproxymate.renderer.state

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.detail.DetailView
import claudeproxymate.renderer.i18n.I18n
import org.scalajs.dom

/** C3 PR 5 — presenter mode (global mask-all / reveal-all).
  *
  * Owns the "decisive global toggle" semantics: a single click on
  * the status-bar "Mask secrets" switch (or the chord ⌘⇧M /
  * Ctrl+Shift+M) flips [[AppState.presenterMaskAll]] and clears
  * [[AppState.maskOverrides]] so the new baseline takes hold
  * cleanly. Per-span clicks (which add/remove individual ids in
  * `maskOverrides`) are dissent against the current baseline and
  * are wiped by each global toggle.
  *
  * Re-renders the active capture's detail view after every toggle
  * because every mask span on the page may need a class swap; the
  * surgical in-place swap helpers in `JsonTreeViewer` /
  * `MessageRenderer` are per-id and don't scale here. Scroll
  * position is preserved across the re-render.
  *
  * Copy follows presenter mode: `MaskedCopy.maskBody` and
  * `MessageCopy.toPlainText` are driven by [[AppState.isRevealed]],
  * so the global baseline (and any per-span overrides) directly
  * determines what lands on the clipboard — WYSIWYG.
  */
object PresenterMode {

  /** Flip the global baseline and clear all per-span overrides.
    * Triggers a full re-render of the active capture's detail view.
    * Updates the status-bar switch.
    */
  def toggle(): Unit = {
    AppState.presenterMaskAll = !AppState.presenterMaskAll
    AppState.maskOverrides.clear()
    refresh()
  }

  /** Re-render the active capture (preserving scroll position) and
    * sync the switch with the current state. Called by [[toggle]] and
    * on init.
    */
  def refresh(): Unit = {
    val container = dom.document.getElementById(HtmlIds.ProxyDetailView)
    if (container != null) {
      val el          = container.asInstanceOf[dom.html.Element]
      val savedScroll = el.scrollTop
      DetailView.renderProxyDetail()
      val _           = dom.window.requestAnimationFrame { _ =>
        el.scrollTop = savedScroll
      }
    }
    renderButton()
  }

  /** Sync the status-bar "Mask secrets" switch with the current state:
    * ON (green, knob right) = everything masked. Never writes
    * `textContent` — the switch's knob is a child span that must
    * survive. `data-i18n-title` is rewritten to the current state's key
    * so a later `applyI18n` (locale switch) keeps the tooltip truthful.
    */
  def renderButton(): Unit = {
    val btn = dom.document.getElementById(HtmlIds.MaskToggleBtn)
    if (btn == null) return
    val el  = btn.asInstanceOf[dom.html.Element]
    val on  = AppState.presenterMaskAll
    if (on) {
      locally { val _ = el.classList.add("on") }
    } else {
      locally { val _ = el.classList.remove("on") }
    }
    val titleKey = if (on) "mask.switchTitleOn" else "mask.switchTitleOff"
    val title    = I18n.t(titleKey)
    el.setAttribute("aria-checked", on.toString)
    el.setAttribute("title", title)
    el.setAttribute("aria-label", title)
    el.setAttribute("data-i18n-title", titleKey)
  }
}
