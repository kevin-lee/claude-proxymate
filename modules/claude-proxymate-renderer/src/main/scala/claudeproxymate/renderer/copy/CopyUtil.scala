package claudeproxymate.renderer.copy

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.state.AppState
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Copy utilities for proxy command and detail view.
  *
  * Ports `copyProxyCmd`, `copyProxyDetail` from renderer.js.
  */
object CopyUtil {

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

  @JSExportTopLevel("copyProxyCmd")
  def copyProxyCmd(): Unit = {
    val el = dom.document.getElementById(HtmlIds.ProxyCmdText)
    if (el == null || !AppState.proxyRunning) return
    locally { val _ = dom.window.navigator.clipboard.writeText(el.textContent)
      .asInstanceOf[js.Dynamic]
      .`then`(flashCopyButton("""[onclick="copyProxyCmd()"]"""))
      .`catch`(onCopyError) }
  }

  @JSExportTopLevel("copyProxyDetail")
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
          .`then`(flashCopyButton("""[onclick="copyProxyDetail()"]"""))
          .`catch`(onCopyError) }
    }
  }
}
