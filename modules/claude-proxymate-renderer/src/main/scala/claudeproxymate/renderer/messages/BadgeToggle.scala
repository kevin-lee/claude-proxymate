package claudeproxymate.renderer.messages

import cats.syntax.all.*
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
    val part    = Option(btn).map(_.getAttribute(MessageView.BadgePartAttr)).filter(_ != null)

    // Deactivate previous badge if different
    AppState.activeBadgeUid.foreach { prevUid =>
      if (prevUid =!= uid) {
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
    val isOpen    = contentEl.style.display =!= "none"
    contentEl.style.display = if (isOpen) "none" else "block"
    locally { val _ = contentEl.classList.toggle("badge-section-hl", !isOpen) }

    if (btn != null) {
      locally { val _ = btn.classList.toggle("open", !isOpen) }
      locally { val _ = btn.classList.toggle("hl-active", !isOpen) }
    }

    AppState.activeBadgeUid = Option.unless(isOpen)(uid)
    AppState.activeBadgePart = if (isOpen) None else part
  }

  /** Re-open the badge with the stable part id `part` after a re-render
    * (which mints new uids), without toggling.
    */
  def reopen(part: String): Unit = {
    val escaped   = scala.scalajs.js.Dynamic.global.CSS.applyDynamic("escape")(part).asInstanceOf[String]
    val btn       = dom.document.querySelector(s"""[${MessageView.BadgePartAttr}="$escaped"]""")
    if (btn == null) return
    val uid       = btn.getAttribute(MessageView.BadgeDataAttr)
    val content   = if (uid == null) null else dom.document.getElementById(s"bc_$uid")
    if (content == null) return
    val contentEl = content.asInstanceOf[dom.html.Element]
    contentEl.style.display = "block"
    locally { val _ = contentEl.classList.add("badge-section-hl") }
    locally { val _ = btn.classList.add("open") }
    locally { val _ = btn.classList.add("hl-active") }
    AppState.activeBadgeUid = Some(uid)
    AppState.activeBadgePart = Some(part)
  }
}
