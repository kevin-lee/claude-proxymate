package claudeproxymate.renderer.update

import cats.syntax.all.*
import claudeproxymate.core.update.{UpdatePhase, UpdateSettings, UpdateState}
import claudeproxymate.renderer.detail.TokenPopoverLinks
import scalatags.Text.all.*

import scala.scalajs.js

/** i18n-sourced texts for the self-update UI.
  *
  * Texts with `{version}`, `{percent}` or `{time}` are templates, filled by [[UpdateView]]
  * the same way `I18n.t` fills its variables.
  */
final case class UpdateLabels(
  pillAvailable: String,
  pillDownloading: String,
  pillReady: String,
  pillFailed: String,
  panelAvailableTitle: String,
  installed: String,
  newVersion: String,
  downloadAndInstall: String,
  whatsNew: String,
  readyTitle: String,
  readyBody: String,
  restartNow: String,
  later: String,
  failedTitle: String,
  tryAgain: String,
  statusUnsupported: String,
  statusVersion: String,
  statusChecking: String,
  statusUpToDate: String,
  statusAvailable: String,
  statusDownloading: String,
  statusReady: String,
  statusCheckFailed: String,
  checkNow: String,
  lastChecked: String,
  autoUpdate: String,
)

/** Pure views for the header update pill, the panel under it, and the update
  * section of the ℹ popover.
  */
object UpdateView {

  val PillClass: String  = "update-pill"
  val PanelClass: String = "update-panel"
  val InfoClass: String  = "proxy-info-popover-update"

  /** Marks a button whose click runs an update action (handled by UpdateControl). */
  val ActionAttr: String = "data-update-action"

  /** Marks the auto update checkbox (handled by UpdateControl). */
  val SettingAttr: String = "data-update-setting"

  val ActionCheck: String    = "check"
  val ActionDownload: String = "download"
  val ActionInstall: String  = "install"
  val ActionClose: String    = "close"

  val AutoUpdateSetting: String = "autoUpdate"

  def releaseHref(version: String): String =
    s"https://github.com/kevin-lee/claude-proxymate/releases/tag/v$version"

  private def fill(template: String, vars: List[(String, String)]): String =
    vars.foldLeft(template) { case (s, (name, value)) => s.replace(s"{$name}", value) }

  private def timeText(checkedAt: Double): String =
    new js.Date(checkedAt).toLocaleTimeString()

  /** The pill in the header. `None` when there is nothing for the user to act on. */
  def pillFrag(state: UpdateState, labels: UpdateLabels): Option[Frag] = state match {
    case UpdateState.Available(version, _) =>
      button(tpe := "button", cls := s"$PillClass $PillClass-available")(
        fill(labels.pillAvailable, List("version" -> version))
      ).some
    case UpdateState.Downloading(version, percent) =>
      span(cls := s"$PillClass $PillClass-downloading")(
        fill(labels.pillDownloading, List("version" -> version, "percent" -> percent.toString)),
        span(
          cls := "update-progress",
          attr("role") := "progressbar",
          attr("aria-valuemin") := "0",
          attr("aria-valuemax") := "100",
          attr("aria-valuenow") := percent.toString,
        )(
          span(cls := "update-progress-fill", style := s"width:$percent%")
        ),
      ).some
    case UpdateState.Ready(version) =>
      button(tpe := "button", cls := s"$PillClass $PillClass-ready")(
        fill(labels.pillReady, List("version" -> version))
      ).some
    case UpdateState.Failed(UpdatePhase.Download | UpdatePhase.Install, _, _, _) =>
      button(tpe := "button", cls := s"$PillClass $PillClass-failed")(labels.pillFailed).some
    case UpdateState.Unsupported | UpdateState.Idle | UpdateState.Checking(_) | UpdateState.UpToDate(_) |
         UpdateState.Failed(UpdatePhase.Check, _, _, _) =>
      none[Frag]
  }

  private def actionButton(primary: Boolean, action: String, label: String): Frag =
    button(
      tpe := "button",
      cls := (if (primary) "update-btn-primary" else "update-btn-secondary"),
      attr(ActionAttr) := action,
    )(label)

  private def whatsNewLink(version: String, labels: UpdateLabels): Frag =
    a(
      href := releaseHref(version),
      cls := s"${TokenPopoverLinks.ExternalLinkClass} update-panel-link",
      target := "_blank",
      rel := "noopener",
    )(labels.whatsNew)

  /** The panel under the pill. Update information only, no Website or Bug report links. */
  def panelFrag(state: UpdateState, installedVersion: String, labels: UpdateLabels): Option[Frag] = state match {
    case UpdateState.Available(version, _) =>
      frag(
        div(cls := s"$PanelClass-title")(labels.panelAvailableTitle),
        div(cls := s"$PanelClass-body")(
          div(cls := s"$PanelClass-versions")(
            span(fill(labels.installed, List("version" -> installedVersion))),
            span(cls := s"$PanelClass-new")(fill(labels.newVersion, List("version" -> version))),
          ),
          div(cls := s"$PanelClass-actions")(
            actionButton(true, ActionDownload, labels.downloadAndInstall),
            whatsNewLink(version, labels),
          ),
        ),
      ).some
    case UpdateState.Ready(version) =>
      frag(
        div(cls := s"$PanelClass-title")(fill(labels.readyTitle, List("version" -> version))),
        div(cls := s"$PanelClass-body")(
          div(cls := s"$PanelClass-note")(labels.readyBody),
          div(cls := s"$PanelClass-actions")(
            actionButton(true, ActionInstall, labels.restartNow),
            actionButton(false, ActionClose, labels.later),
            whatsNewLink(version, labels),
          ),
        ),
      ).some
    case UpdateState.Failed(phase, message, _, _) =>
      val retry = phase match {
        case UpdatePhase.Download => ActionDownload
        case UpdatePhase.Check | UpdatePhase.Install => ActionCheck
      }
      frag(
        div(cls := s"$PanelClass-title")(labels.failedTitle),
        div(cls := s"$PanelClass-body")(
          div(cls := s"$PanelClass-note")(message),
          div(cls := s"$PanelClass-actions")(actionButton(true, retry, labels.tryAgain)),
        ),
      ).some
    case UpdateState.Unsupported | UpdateState.Idle | UpdateState.Checking(_) | UpdateState.UpToDate(_) |
         UpdateState.Downloading(_, _) =>
      none[Frag]
  }

  private def statusText(state: UpdateState, installedVersion: String, labels: UpdateLabels): String =
    state match {
      case UpdateState.Unsupported =>
        labels.statusUnsupported
      case UpdateState.Idle =>
        fill(labels.statusVersion, List("version" -> installedVersion))
      case UpdateState.Checking(_) =>
        labels.statusChecking
      case UpdateState.UpToDate(_) =>
        fill(labels.statusUpToDate, List("version" -> installedVersion))
      case UpdateState.Available(version, _) =>
        fill(labels.statusAvailable, List("version" -> version))
      case UpdateState.Downloading(version, percent) =>
        fill(labels.statusDownloading, List("version" -> version, "percent" -> percent.toString))
      case UpdateState.Ready(version) =>
        fill(labels.statusReady, List("version" -> version))
      case UpdateState.Failed(phase, message, userInitiated, _) =>
        val title = phase match {
          case UpdatePhase.Check => labels.statusCheckFailed
          case UpdatePhase.Download | UpdatePhase.Install => labels.failedTitle
        }
        if (userInitiated && message.nonEmpty) s"$title: $message" else title
    }

  private def checkedAtOf(state: UpdateState): Option[Double] = state match {
    case UpdateState.UpToDate(checkedAt) => checkedAt.some
    case UpdateState.Available(_, checkedAt) => checkedAt.some
    case UpdateState.Failed(_, _, _, checkedAt) => checkedAt
    case UpdateState.Unsupported | UpdateState.Idle | UpdateState.Checking(_) | UpdateState.Downloading(_, _) |
         UpdateState.Ready(_) =>
      none[Double]
  }

  /** The update section of the ℹ popover: status, "Check now", last check time and the auto update checkbox. */
  def infoSectionFrag(
    state: UpdateState,
    installedVersion: String,
    settings: UpdateSettings,
    labels: UpdateLabels,
  ): Frag = {
    val checkButton = button(
      tpe := "button",
      cls := s"$InfoClass-check",
      attr(ActionAttr) := ActionCheck,
    )(labels.checkNow)

    val checkbox = input(
      tpe := "checkbox",
      attr(SettingAttr) := AutoUpdateSetting,
    )

    frag(
      div(cls := s"$InfoClass-status-row")(
        span(cls := s"$InfoClass-status")(statusText(state, installedVersion, labels)),
        if (UpdateState.canCheck(state)) checkButton else checkButton(disabled := true),
      ),
      checkedAtOf(state).fold(frag()) { checkedAt =>
        div(cls := s"$InfoClass-checked")(fill(labels.lastChecked, List("time" -> timeText(checkedAt))))
      },
      label(cls := s"$InfoClass-auto")(
        state match {
          case UpdateState.Unsupported =>
            if (settings.autoUpdate) checkbox(checked := true, disabled := true) else checkbox(disabled := true)
          case UpdateState.Idle | UpdateState.Checking(_) | UpdateState.UpToDate(_) | UpdateState.Available(_, _) |
               UpdateState.Downloading(_, _) | UpdateState.Ready(_) | UpdateState.Failed(_, _, _, _) =>
            if (settings.autoUpdate) checkbox(checked := true) else checkbox
        },
        labels.autoUpdate,
      ),
    )
  }
}
