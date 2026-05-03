package claudeproxymate.renderer.messages

import claudeproxymate.renderer.state.AppState
import org.scalajs.dom

/** Toggle expandable badge sections in user messages.
  *
  * Called by [[MessageRenderer]]'s document-level click handler.
  */
object BadgeToggle {

  def toggleBadge(uid: String): Unit = {
    val content = dom.document.getElementById(s"bc_$uid")
    val btn     = dom.document.getElementById(s"bb_$uid")
    if (content == null) return

    // Deactivate previous badge if different
    AppState.activeBadgeUid.foreach { prevUid =>
      if (prevUid != uid) {
        val prevContent = dom.document.getElementById(s"bc_$prevUid")
        val prevBtn     = dom.document.getElementById(s"bb_$prevUid")
        if (prevContent != null) {
          prevContent.asInstanceOf[dom.html.Element].style.display = "none"
          locally { val _ = prevContent.classList.remove("badge-section-hl") }
        }
        if (prevBtn != null) {
          locally { val _ = prevBtn.classList.remove("open") }
          locally { val _ = prevBtn.classList.remove("hl-active") }
        }
      }
    }

    val contentEl = content.asInstanceOf[dom.html.Element]
    val isOpen    = contentEl.style.display != "none"
    contentEl.style.display = if (isOpen) "none" else "block"
    locally { val _ = contentEl.classList.toggle("badge-section-hl", !isOpen) }

    if (btn != null) {
      locally { val _ = btn.classList.toggle("open", !isOpen) }
      locally { val _ = btn.classList.toggle("hl-active", !isOpen) }
    }

    AppState.activeBadgeUid = if (isOpen) None else Some(uid)
  }
}
