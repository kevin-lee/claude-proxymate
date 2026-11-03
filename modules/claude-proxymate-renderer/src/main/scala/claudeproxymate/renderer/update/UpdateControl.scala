package claudeproxymate.renderer.update

import cats.syntax.all.*
import claudeproxymate.core.HtmlIds
import claudeproxymate.core.update.{UpdateSettings, UpdateState}
import claudeproxymate.renderer.facades.ElectronApi
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.proxy.ProxyInfoPopover
import claudeproxymate.renderer.view.ViewHelpers
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom
import scalatags.Text.all.Frag

import java.util.concurrent.atomic.AtomicReference
import scala.scalajs.js
import scala.scalajs.js.JSON

/** DOM glue for the self-update UI: the header pill, the panel under it, and the
  * update section of the ℹ popover. The state comes from the main process
  * (`UpdateService`) over IPC; this object only renders it and sends actions back.
  */
object UpdateControl {

  private val state            = new AtomicReference[UpdateState](UpdateState.idle)
  private val installedVersion = new AtomicReference[String]("")
  private val settings         = new AtomicReference[UpdateSettings](UpdateSettings.default)

  def labels: UpdateLabels = UpdateLabels(
    pillAvailable = I18n.t("update.pillAvailable"),
    pillDownloading = I18n.t("update.pillDownloading"),
    pillReady = I18n.t("update.pillReady"),
    pillFailed = I18n.t("update.pillFailed"),
    panelAvailableTitle = I18n.t("update.panelAvailableTitle"),
    installed = I18n.t("update.installed"),
    newVersion = I18n.t("update.new"),
    downloadAndInstall = I18n.t("update.downloadAndInstall"),
    whatsNew = I18n.t("update.whatsNew"),
    readyTitle = I18n.t("update.readyTitle"),
    readyBody = I18n.t("update.readyBody"),
    restartNow = I18n.t("update.restartNow"),
    later = I18n.t("update.later"),
    failedTitle = I18n.t("update.failedTitle"),
    tryAgain = I18n.t("update.tryAgain"),
    statusUnsupported = I18n.t("update.statusUnsupported"),
    statusVersion = I18n.t("update.statusVersion"),
    statusChecking = I18n.t("update.statusChecking"),
    statusUpToDate = I18n.t("update.statusUpToDate"),
    statusAvailable = I18n.t("update.statusAvailable"),
    statusDownloading = I18n.t("update.statusDownloading"),
    statusReady = I18n.t("update.statusReady"),
    statusCheckFailed = I18n.t("update.statusCheckFailed"),
    checkNow = I18n.t("update.checkNow"),
    lastChecked = I18n.t("update.lastChecked"),
    autoUpdate = I18n.t("update.autoUpdate"),
  )

  /** The update section for the ℹ popover, from the current values. */
  def infoSection(): Frag =
    UpdateView.infoSectionFrag(state.get(), installedVersion.get(), settings.get(), labels)

  private def decodeState(raw: js.Dynamic): Option[UpdateState] =
    if (js.isUndefined(raw) || raw == null) none[UpdateState]
    else decode[UpdateState](JSON.stringify(raw)).toOption

  private def decodeSettings(raw: js.Dynamic): Option[UpdateSettings] =
    if (js.isUndefined(raw) || raw == null) none[UpdateSettings]
    else decode[UpdateSettings](JSON.stringify(raw)).toOption

  def install(): Unit = {
    dom.document.addEventListener("click", handleClick(_))
    dom.document.addEventListener("change", handleChange(_))
    dom.document.addEventListener("keydown", escapeKeyHandler(_))

    ElectronApi.get.foreach { api =>
      api.onUpdateState { (payload: js.Dynamic) =>
        decodeState(payload).foreach(onState)
      }
      api.onUpdateReveal { (_: js.Dynamic) =>
        revealInfo()
      }
      locally {
        val _ = api.appVersion().`then`[Unit] { (version: String) =>
          installedVersion.set(version)
          rerender()
        }
      }
      locally {
        val _ = api.updateSettingsGet().`then`[Unit] { (raw: js.Dynamic) =>
          decodeSettings(raw).foreach(settings.set)
          rerender()
        }
      }
      locally {
        val _ = api.updateStateGet().`then`[Unit] { (result: js.Dynamic) =>
          decodeState(result.selectDynamic("state")).foreach(onState)
          val reveal = result.selectDynamic("reveal")
          if (!js.isUndefined(reveal) && reveal != null && reveal.asInstanceOf[Boolean]) revealInfo() else ()
        }
      }
    }
  }

  private def onState(next: UpdateState): Unit = {
    state.set(next)
    rerender()
  }

  /** Re-render everything that depends on the state or the locale. */
  def rerender(): Unit = {
    renderPill()
    rerenderPanelIfOpen()
    ProxyInfoPopover.refreshUpdateSection()
  }

  private def renderPill(): Unit = {
    val slot = dom.document.getElementById(HtmlIds.UpdatePill)
    if (slot != null) {
      UpdateView.pillFrag(state.get(), labels) match {
        case Some(pill) => ViewHelpers.setInnerHtml(slot, pill)
        case None => slot.innerHTML = ""
      }
    } else ()
  }

  private def openPanel(): Option[dom.html.Element] =
    Option(dom.document.getElementById(HtmlIds.UpdatePanel)).map(_.asInstanceOf[dom.html.Element])

  private def closePanel(): Unit =
    openPanel().foreach { panel =>
      locally { val _ = panel.parentNode.removeChild(panel) }
    }

  private def rerenderPanelIfOpen(): Unit =
    openPanel().foreach { panel =>
      UpdateView.panelFrag(state.get(), installedVersion.get(), labels) match {
        case Some(content) => ViewHelpers.setInnerHtml(panel, content)
        case None => closePanel()
      }
    }

  /* The panel goes into the header, which is `position: relative`, under the pill. */
  private def togglePanel(): Unit =
    openPanel() match {
      case Some(_) =>
        closePanel()
      case None =>
        val header = dom.document.getElementById(HtmlIds.AppHeader)
        val pill   = dom.document.querySelector(s"#${HtmlIds.UpdatePill} .${UpdateView.PillClass}")
        UpdateView.panelFrag(state.get(), installedVersion.get(), labels) match {
          case Some(content) if header != null && pill != null =>
            val headerRect = header.getBoundingClientRect()
            val pillRect   = pill.getBoundingClientRect()
            val panel      = dom.document.createElement("div").asInstanceOf[dom.html.Div]
            panel.id = HtmlIds.UpdatePanel
            panel.className = UpdateView.PanelClass
            panel.style.left = s"${math.max(0.0, pillRect.left - headerRect.left)}px"
            panel.style.top = s"${pillRect.bottom - headerRect.top + 4}px"
            ViewHelpers.setInnerHtml(panel, content)
            locally { val _ = header.appendChild(panel) }
          case Some(_) | None =>
            ()
        }
    }

  /** Opens the ℹ popover, where the update status and "Check now" are. */
  def revealInfo(): Unit = ProxyInfoPopover.open()

  private def runAction(action: String): Unit =
    ElectronApi.get.foreach { api =>
      action match {
        case UpdateView.ActionDownload =>
          locally { val _ = api.updateDownload() }
        case UpdateView.ActionInstall =>
          locally { val _ = api.updateInstall() }
        case UpdateView.ActionCheck =>
          locally { val _ = api.updateCheck() }
        case UpdateView.ActionClose =>
          closePanel()
        case other =>
          dom.console.warn("unknown update action:", other)
      }
    }

  private def handleClick(e: dom.MouseEvent): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target != null) {
      val actionEl = target.closest(s"[${UpdateView.ActionAttr}]")
      if (actionEl != null) {
        runAction(actionEl.getAttribute(UpdateView.ActionAttr))
      } else if (target.closest(s"#${HtmlIds.UpdatePill} .${UpdateView.PillClass}") != null) {
        state.get() match {
          case UpdateState.Downloading(_, _) => ()
          case UpdateState.Unsupported | UpdateState.Idle | UpdateState.Checking(_) | UpdateState.UpToDate(_) |
               UpdateState.Available(_, _) | UpdateState.Ready(_) | UpdateState.Failed(_, _, _, _) =>
            togglePanel()
        }
      } else if (target.closest(s"#${HtmlIds.UpdatePanel}") == null) {
        closePanel()
      } else ()
    } else ()
  }

  private def handleChange(e: dom.Event): Unit = {
    val target = e.target.asInstanceOf[dom.Element]
    if (target != null && Option(target.getAttribute(UpdateView.SettingAttr)).contains(UpdateView.AutoUpdateSetting)) {
      val autoUpdate = target.asInstanceOf[dom.html.Input].checked
      ElectronApi.get.foreach { api =>
        val raw = JSON.parse(UpdateSettings(autoUpdate).asJson.noSpaces)
        locally {
          val _ = api.updateSettingsSet(raw).`then`[Unit] { (result: js.Dynamic) =>
            val ok = result.selectDynamic("ok")
            if (!js.isUndefined(ok) && ok != null && ok.asInstanceOf[Boolean]) {
              decodeSettings(result.selectDynamic("settings")).foreach(settings.set)
            } else {
              dom.console.warn("update settings not saved:", result.selectDynamic("reason"))
            }
            ProxyInfoPopover.refreshUpdateSection()
          }
        }
      }
    } else ()
  }

  private def escapeKeyHandler(e: dom.KeyboardEvent): Unit =
    if (e.key === "Escape") closePanel() else ()
}
