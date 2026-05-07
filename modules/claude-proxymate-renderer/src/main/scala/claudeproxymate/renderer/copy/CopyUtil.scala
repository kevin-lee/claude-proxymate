package claudeproxymate.renderer.copy

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.state.AppState
import org.scalajs.dom

import scala.scalajs.js

/** Copy utilities for proxy command and detail view.
  *
  * Ports `copyProxyCmd`, `copyProxyDetail` from renderer.js. Click
  * dispatch goes through [[install]] (doc-level click listener
  * filtered by `#proxyCmdCopyBtn` / `#copyDetailBtn`). Inline
  * `onclick` handlers were removed because Scala.js NoModule
  * `let`-exported globals are unreliable from inline HTML attributes
  * in this Electron version.
  */
object CopyUtil {

  def install(): Unit =
    dom.document.addEventListener("click", handleClick _)

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target == null) return
    if (target.closest(s"#${HtmlIds.ProxyCmdCopyBtn}") != null) {
      copyProxyCmd()
      return
    }
    if (target.closest(s"#${HtmlIds.CopyDetailBtn}") != null) copyProxyDetail()
  }

  private def flashCopyButton(selector: String): js.Function1[Any, Unit] = { (_: Any) =>
    val btn = dom.document.querySelector(selector)
    if (btn != null) {
      val orig = btn.textContent
      btn.textContent = "\u2713"
      locally { val _ = js.timers.setTimeout(1500.0) { btn.textContent = orig } }
    }
  }

  private val onCopyError: js.Function1[Any, Unit] = { (e: Any) =>
    dom.console.error("copy failed", e.asInstanceOf[js.Any])
  }

  def copyProxyCmd(): Unit = {
    val el = dom.document.getElementById(HtmlIds.ProxyCmdText)
    if (el == null || !AppState.proxyRunning) return
    locally { val _ = dom.window.navigator.clipboard.writeText(el.textContent)
      .asInstanceOf[js.Dynamic]
      .`then`(flashCopyButton(s"#${HtmlIds.ProxyCmdCopyBtn}"))
      .`catch`(onCopyError) }
  }

  def copyProxyDetail(): Unit = {
    val entry = AppState.proxyCaptures.find(e => e.id == AppState.selectedProxyId.map(_.asInstanceOf[js.Any]).orNull)
    entry match {
      case None => ()
      case Some(e) =>
        val data: js.Dynamic = if (AppState.proxyDetailTab == "request") {
          e.selectDynamic("body")
        } else {
          val resp = e.selectDynamic("response")
          if (!js.isUndefined(resp) && resp != null) resp.selectDynamic("body")
          else null
        }
        if (js.isUndefined(data) || data == null) return

        val text = if (js.typeOf(data) == "string") data.asInstanceOf[String]
        else js.JSON.stringify(data, null.asInstanceOf[js.Array[js.Any]], 2)

        locally { val _ = dom.window.navigator.clipboard.writeText(text)
          .asInstanceOf[js.Dynamic]
          .`then`(flashCopyButton(s"#${HtmlIds.CopyDetailBtn}"))
          .`catch`(onCopyError) }
    }
  }
}
