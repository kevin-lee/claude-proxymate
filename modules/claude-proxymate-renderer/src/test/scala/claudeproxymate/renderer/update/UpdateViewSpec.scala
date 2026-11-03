package claudeproxymate.renderer.update

import claudeproxymate.core.update.{UpdatePhase, UpdateSettings, UpdateState}
import claudeproxymate.renderer.detail.TokenPopoverLinks
import hedgehog.*
import hedgehog.runner.*

object UpdateViewSpec extends Properties {

  override def tests: List[Test] = List(
    example("pillFrag is empty when there is nothing to act on", testPillNone),
    example("pillFrag for Available shows the version", testPillAvailable),
    example("pillFrag for Downloading has the percent and aria-valuenow", testPillDownloading),
    example("pillFrag for Ready shows the restart text", testPillReady),
    example("pillFrag for a failed download or install shows the failure", testPillFailed),
    example("panelFrag for Available has the download action and the release link", testPanelAvailable),
    example("panelFrag for Ready has the install and close actions", testPanelReady),
    example("panelFrag for a failed download retries the download", testPanelFailedDownload),
    example("panelFrag is empty while checking or downloading", testPanelNone),
    example("panelFrag never has the Website, Bug report or Getting Started links", testPanelNoAboutLinks),
    example("infoSectionFrag has Check now, disabled when a check cannot start", testInfoCheckNow),
    example("infoSectionFrag checks the checkbox only when autoUpdate is on", testInfoCheckbox),
    example("infoSectionFrag shows the last check time only when there is one", testInfoLastChecked),
    example("infoSectionFrag shows the message of a failed check the user started", testInfoFailedMessage),
    property("no view leaks raw <script> from labels or versions", testNoScriptLeak),
  )

  private val labels: UpdateLabels = UpdateLabels(
    pillAvailable = "↑ Update to v{version}",
    pillDownloading = "Downloading v{version} · {percent}%",
    pillReady = "Restart to update v{version}",
    pillFailed = "Update failed",
    panelAvailableTitle = "Update available",
    installed = "Installed: {version}",
    newVersion = "New: {version}",
    downloadAndInstall = "Download and install",
    whatsNew = "What's new",
    readyTitle = "Install Claude Proxymate {version}?",
    readyBody = "Restarting stops the proxy and clears the captured requests.",
    restartNow = "Restart now",
    later = "Later",
    failedTitle = "Update failed",
    tryAgain = "Try again",
    statusUnsupported = "Updates are not available in this build",
    statusVersion = "Version {version}",
    statusChecking = "Checking for updates…",
    statusUpToDate = "Version {version} · up to date",
    statusAvailable = "{version} is available",
    statusDownloading = "Downloading {version} · {percent}%",
    statusReady = "{version} is ready. Restart to update.",
    statusCheckFailed = "Could not check for updates",
    checkNow = "Check now",
    lastChecked = "Last checked: {time}",
    autoUpdate = "Update automatically when the app starts",
  )

  private val installed = "0.7.0"

  private def pill(state: UpdateState): Option[String] = UpdateView.pillFrag(state, labels).map(_.render)

  private def panel(state: UpdateState): Option[String] =
    UpdateView.panelFrag(state, installed, labels).map(_.render)

  private def info(state: UpdateState, settings: UpdateSettings): String =
    UpdateView.infoSectionFrag(state, installed, settings, labels).render

  private def action(name: String): String = s"""${UpdateView.ActionAttr}="$name""""

  def testPillNone: Result =
    Result.all(
      List(
        UpdateState.unsupported,
        UpdateState.idle,
        UpdateState.checking(true),
        UpdateState.upToDate(1.0),
        UpdateState.failed(UpdatePhase.Check, "offline", true, None),
      ).map(state => (pill(state) ==== None).log(s"no pill expected for $state"))
    )

  def testPillAvailable: Result =
    pill(UpdateState.available("0.8.0", 1.0)).fold(Result.failure.log("pill expected")) { html =>
      Result.all(
        List(
          Result.assert(html.contains(s"${UpdateView.PillClass}-available")).log(html),
          Result.assert(html.contains("↑ Update to v0.8.0")).log(html),
        )
      )
    }

  def testPillDownloading: Result =
    pill(UpdateState.downloading("0.8.0", 42)).fold(Result.failure.log("pill expected")) { html =>
      Result.all(
        List(
          Result.assert(html.contains(s"${UpdateView.PillClass}-downloading")).log(html),
          Result.assert(html.contains("Downloading v0.8.0 · 42%")).log(html),
          Result.assert(html.contains("""aria-valuenow="42"""")).log(html),
          Result.assert(html.contains("width:42%")).log(html),
        )
      )
    }

  def testPillReady: Result =
    pill(UpdateState.ready("0.8.0")).fold(Result.failure.log("pill expected")) { html =>
      Result.all(
        List(
          Result.assert(html.contains(s"${UpdateView.PillClass}-ready")).log(html),
          Result.assert(html.contains("Restart to update v0.8.0")).log(html),
        )
      )
    }

  def testPillFailed: Result =
    Result.all(
      List(UpdatePhase.Download, UpdatePhase.Install).map { phase =>
        pill(UpdateState.failed(phase, "boom", true, None)).fold(Result.failure.log(s"pill expected for $phase")) {
          html => Result.assert(html.contains(s"${UpdateView.PillClass}-failed")).log(html)
        }
      }
    )

  def testPanelAvailable: Result =
    panel(UpdateState.available("0.8.0", 1.0)).fold(Result.failure.log("panel expected")) { html =>
      Result.all(
        List(
          Result.assert(html.contains(action(UpdateView.ActionDownload))).log(html),
          Result.assert(html.contains(s"""href="${UpdateView.releaseHref("0.8.0")}"""")).log(html),
          Result.assert(html.contains(TokenPopoverLinks.ExternalLinkClass)).log(html),
          Result.assert(html.contains("Installed: 0.7.0")).log(html),
          Result.assert(html.contains("New: 0.8.0")).log(html),
        )
      )
    }

  def testPanelReady: Result =
    panel(UpdateState.ready("0.8.0")).fold(Result.failure.log("panel expected")) { html =>
      Result.all(
        List(
          Result.assert(html.contains(action(UpdateView.ActionInstall))).log(html),
          Result.assert(html.contains(action(UpdateView.ActionClose))).log(html),
          Result.assert(html.contains("Install Claude Proxymate 0.8.0?")).log(html),
        )
      )
    }

  def testPanelFailedDownload: Result =
    panel(UpdateState.failed(UpdatePhase.Download, "disk full", true, None))
      .fold(Result.failure.log("panel expected")) { html =>
        Result.all(
          List(
            Result.assert(html.contains(action(UpdateView.ActionDownload))).log(html),
            Result.assert(html.contains("disk full")).log(html),
          )
        )
      }

  def testPanelNone: Result =
    Result.all(
      List(
        UpdateState.unsupported,
        UpdateState.idle,
        UpdateState.checking(false),
        UpdateState.upToDate(1.0),
        UpdateState.downloading("0.8.0", 10),
      ).map(state => (panel(state) ==== None).log(s"no panel expected for $state"))
    )

  def testPanelNoAboutLinks: Result =
    Result.all(
      List(
        UpdateState.available("0.8.0", 1.0),
        UpdateState.ready("0.8.0"),
        UpdateState.failed(UpdatePhase.Download, "x", true, None),
      ).flatMap(panel).map { html =>
        Result.all(
          List(
            Result.assert(!html.contains("claude-proxymate.kevinly.dev")).log(html),
            Result.assert(!html.contains("/issues")).log(html),
            Result.assert(!html.contains("data-onboard-replay")).log(html),
          )
        )
      }
    )

  def testInfoCheckNow: Result = {
    val enabled  = info(UpdateState.upToDate(1.0), UpdateSettings.default)
    val disabled = info(UpdateState.downloading("0.8.0", 5), UpdateSettings.default)
    Result.all(
      List(
        Result.assert(enabled.contains(action(UpdateView.ActionCheck))).log(enabled),
        Result.assert(enabled.contains("Check now")).log(enabled),
        Result.assert(!enabled.contains("disabled")).log(enabled),
        Result.assert(disabled.contains("disabled")).log(disabled),
      )
    )
  }

  def testInfoCheckbox: Result = {
    val on  = info(UpdateState.idle, UpdateSettings(autoUpdate = true))
    val off = info(UpdateState.idle, UpdateSettings(autoUpdate = false))
    Result.all(
      List(
        Result.assert(on.contains(s"""${UpdateView.SettingAttr}="${UpdateView.AutoUpdateSetting}"""")).log(on),
        Result.assert(on.contains("checked")).log(on),
        Result.assert(!off.contains("checked")).log(off),
        Result.assert(on.contains("Update automatically when the app starts")).log(on),
      )
    )
  }

  def testInfoLastChecked: Result = {
    val withTime    = info(UpdateState.upToDate(1.0), UpdateSettings.default)
    val withoutTime = info(UpdateState.idle, UpdateSettings.default)
    Result.all(
      List(
        Result.assert(withTime.contains("Last checked: ")).log(withTime),
        Result.assert(!withoutTime.contains("Last checked: ")).log(withoutTime),
        Result.assert(withTime.contains("Version 0.7.0 · up to date")).log(withTime),
      )
    )
  }

  def testInfoFailedMessage: Result = {
    val user       = info(UpdateState.failed(UpdatePhase.Check, "offline", true, Some(1.0)), UpdateSettings.default)
    val background = info(UpdateState.failed(UpdatePhase.Check, "offline", false, Some(1.0)), UpdateSettings.default)
    Result.all(
      List(
        Result.assert(user.contains("Could not check for updates: offline")).log(user),
        Result.assert(background.contains("Could not check for updates")).log(background),
        Result.assert(!background.contains("offline")).log(background),
      )
    )
  }

  def testNoScriptLeak: Property =
    for {
      evil <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("evil")
    } yield {
      val payload    = s"<script>alert('$evil')</script>"
      val evilLabels = UpdateLabels(
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
        payload,
      )
      val states     = List(
        UpdateState.available(payload, 1.0),
        UpdateState.downloading(payload, 10),
        UpdateState.ready(payload),
        UpdateState.failed(UpdatePhase.Download, payload, true, Some(1.0)),
      )
      val htmls      = states.flatMap { state =>
        UpdateView.pillFrag(state, evilLabels).map(_.render).toList ++
          UpdateView.panelFrag(state, payload, evilLabels).map(_.render).toList ++
          List(UpdateView.infoSectionFrag(state, payload, UpdateSettings.default, evilLabels).render)
      }
      Result.all(htmls.map(html => Result.assert(!html.contains("<script>")).log(s"raw <script> leaked: $html")))
    }
}
