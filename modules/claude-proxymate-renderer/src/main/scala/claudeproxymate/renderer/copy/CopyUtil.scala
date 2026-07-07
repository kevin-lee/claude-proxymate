package claudeproxymate.renderer.copy

import cats.syntax.all.*
import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.i18n.I18n
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
  *
  * Detail copies are WYSIWYG: the clipboard mirrors the effective
  * on-screen per-span mask state (`AppState.isRevealed` — presenter
  * baseline XOR per-span overrides). There is no raw escape hatch;
  * force-raw is Reveal All → Copy, force-masked is Mask All → Copy.
  * Counts follow what is rendered: the messages tab copies the
  * currently visible (filtered) cards, and the request/response
  * tabs share the `$…` span-id namespace, so a reveal on one tab
  * also applies to the other.
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
    if (target.closest(s"#${HtmlIds.CopyDetailBtn}") != null) {
      copyProxyDetail()
    }
  }

  private def flashCopyButton(selector: String): js.Function1[Any, Unit] = { (_: Any) =>
    val btn = dom.document.querySelector(selector)
    if (btn != null) {
      val orig = btn.textContent
      btn.textContent = "✓"
      locally { val _ = js.timers.setTimeout(1500.0) { btn.textContent = orig } }
    }
  }

  /** Flash the detail Copy button with span-count feedback:
    *   - no maskable spans → `✓` (1.5 s)
    *   - all spans masked → `copy.flashMasked` (1.5 s)
    *   - any span revealed → `copy.flashRevealed` with counts (3 s,
    *     longer so the warning is readable)
    */
  private def flashDetailButton(revealed: Int, total: Int): js.Function1[Any, Unit] = { (_: Any) =>
    val btn = dom.document.querySelector(s"#${HtmlIds.CopyDetailBtn}")
    if (btn != null) {
      val (text, durationMs) =
        if (total === 0) ("✓", 1500.0)
        else if (revealed === 0) (I18n.t("copy.flashMasked"), 1500.0)
        else
          (
            I18n.t(
              "copy.flashRevealed",
              Map("revealed" -> revealed.toString, "total" -> total.toString),
            ),
            3000.0,
          )
      val orig               = btn.textContent
      btn.textContent = text
      locally { val _ = js.timers.setTimeout(durationMs) { btn.textContent = orig } }
    }
  }

  private val onCopyError: js.Function1[Any, Unit] = { (e: Any) =>
    dom.console.error("copy failed", e.asInstanceOf[js.Any])
  }

  def copyProxyCmd(): Unit = {
    val el = dom.document.getElementById(HtmlIds.ProxyCmdText)
    if (el == null || !AppState.proxyRunning) return
    locally {
      val _ = dom
        .window
        .navigator
        .clipboard
        .writeText(el.textContent)
        .asInstanceOf[js.Dynamic]
        .`then`(flashCopyButton(s"#${HtmlIds.ProxyCmdCopyBtn}"))
        .`catch`(onCopyError)
    }
  }

  /** Copy the active capture's detail to the clipboard, WYSIWYG
    * against `AppState.isRevealed`. Routes by the active tab:
    *   - `messages` → plain-text role-labeled rendering of the
    *     visible cards via
    *     [[claudeproxymate.renderer.messages.MessageCopy.toPlainText]].
    *   - `request` / `response` (default) → JSON stringification
    *     of the request/response body with per-span masking via
    *     [[MaskedCopy.maskBody]].
    */
  def copyProxyDetail(): Unit = {
    val entry = AppState.proxyCaptures.find(e => e.id == AppState.selectedProxyId.map(_.asInstanceOf[js.Any]).orNull)
    entry match {
      case None => ()
      case Some(e) =>
        if (AppState.proxyDetailTab === "messages") copyMessagesDetail(e)
        else copyJsonDetail(e)
    }
  }

  private def copyMessagesDetail(entry: js.Dynamic): Unit = {
    val cards = claudeproxymate.renderer.messages.MessageRenderer.buildVisibleCards(entry)
    val res   = claudeproxymate.renderer.messages.MessageCopy.toPlainText(cards, AppState.isRevealed)
    if (res.text.isEmpty) return
    locally {
      val _ = dom
        .window
        .navigator
        .clipboard
        .writeText(res.text)
        .asInstanceOf[js.Dynamic]
        .`then`(flashDetailButton(res.revealed, res.total))
        .`catch`(onCopyError)
    }
  }

  private def copyJsonDetail(entry: js.Dynamic): Unit = {
    val data: js.Dynamic = if (AppState.proxyDetailTab === "request") {
      entry.selectDynamic("body")
    } else {
      val resp = entry.selectDynamic("response")
      if (!js.isUndefined(resp) && resp != null) resp.selectDynamic("body")
      else null
    }
    if (js.isUndefined(data) || data == null) return

    /* Mirror JsonTreeViewer.renderJsonTree: string bodies are parsed
     * first; unparseable ones render as plain text with zero mask
     * spans, so they copy raw with a plain ✓ flash. */
    val parsed: js.Dynamic =
      if (js.typeOf(data) === "string") {
        try js.JSON.parse(data.asInstanceOf[String])
        catch {
          case _: Throwable =>
            locally {
              val _ = dom
                .window
                .navigator
                .clipboard
                .writeText(data.asInstanceOf[String])
                .asInstanceOf[js.Dynamic]
                .`then`(flashDetailButton(0, 0))
                .`catch`(onCopyError)
            }
            return
        }
      } else data

    val r    = MaskedCopy.maskBody(parsed, AppState.isRevealed)
    val text = js.JSON.stringify(r.body, null.asInstanceOf[js.Array[js.Any]], 2)

    locally {
      val _ = dom
        .window
        .navigator
        .clipboard
        .writeText(text)
        .asInstanceOf[js.Dynamic]
        .`then`(flashDetailButton(r.revealed, r.total))
        .`catch`(onCopyError)
    }
  }
}
