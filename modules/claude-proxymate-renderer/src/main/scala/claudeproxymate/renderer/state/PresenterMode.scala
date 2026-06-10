package claudeproxymate.renderer.state

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.detail.DetailView
import claudeproxymate.renderer.i18n.I18n
import org.scalajs.dom

/** C3 PR 5 — presenter mode (global mask-all / reveal-all).
  *
  * Owns the "decisive global toggle" semantics: a single click on
  * the toolbar button (or the chord ⌘⇧M / Ctrl+Shift+M, or the
  * status chip) flips [[AppState.presenterMaskAll]] and clears
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
    * Updates the toolbar button label and status chip text.
    */
  def toggle(): Unit = {
    AppState.presenterMaskAll = !AppState.presenterMaskAll
    AppState.maskOverrides.clear()
    refresh()
  }

  /** Re-render the active capture (preserving scroll position) and
    * sync the button label + chip text with the current state.
    * Called by [[toggle]] and on init.
    */
  def refresh(): Unit = {
    val container = dom.document.getElementById(HtmlIds.ProxyDetailView)
    if (container != null) {
      val el          = container.asInstanceOf[dom.html.Element]
      val savedScroll = el.scrollTop
      DetailView.renderProxyDetail()
      val _ = dom.window.requestAnimationFrame { _ =>
        el.scrollTop = savedScroll
      }
    }
    renderButton()
    renderChip()
  }

  /** Update the toolbar button's label and aria-label based on the
    * current state. When `presenterMaskAll=true` the next action is
    * "reveal all"; when `false` the next action is "mask all."
    */
  def renderButton(): Unit = {
    val btn = dom.document.getElementById(HtmlIds.MaskToggleBtn)
    if (btn == null) return
    val key = if (AppState.presenterMaskAll) "mask.toggleRevealAll" else "mask.toggleMaskAll"
    val text = I18n.t(key)
    btn.textContent = text
    btn.asInstanceOf[dom.html.Element].setAttribute("aria-label", text)
    btn.asInstanceOf[dom.html.Element].setAttribute("title", text)
  }

  /** Update the status chip's label based on the current state. */
  def renderChip(): Unit = {
    val chip = dom.document.getElementById(HtmlIds.MaskStateChip)
    if (chip == null) return
    val key  = if (AppState.presenterMaskAll) "mask.chipMaskAll" else "mask.chipRevealAll"
    chip.textContent = I18n.t(key)
  }
}
