package claudeproxymate.renderer.state

import scala.collection.mutable
import scala.scalajs.js

/** All global mutable state for the renderer.
  *
  * Ports the top-level `let` variables from renderer.js.
  */
object AppState {

  // Proxy captures
  var proxyCaptures: List[js.Dynamic]   = Nil
  var selectedProxyId: Option[Double]   = None
  var proxyDetailTab: String            = "messages"
  var msgFilter: String                 = "user"
  var msgSearchQuery: String            = ""
  var proxyDetailSearch: String         = ""
  var proxyDetailMechFilter: Option[String] = None
  var proxyRunning: Boolean             = false
  var proxyActualPort: Int              = 8888

  // Badge toggle
  var activeBadgeUid: Option[String] = None

  // Search
  var searchCurrentIdx: Int = -1

  // IME composition flag
  var imeComposing: Boolean = false

  // Debounce / focus tracking
  var msgSearchWasFocused: Boolean     = false
  var detailSearchWasFocused: Boolean  = false

  // JSON tree viewer counters
  var jtId: Int   = 0
  var jtLine: Int = 0

  // Monotonic counter for badge UIDs (replaces Math.random() in MessageRenderer)
  var badgeUidCounter: Int = 0

  // C3 PR 5 — presenter-mode global baseline. `true` (default) means
  // every mask renders masked unless its id is in `maskOverrides`;
  // `false` means every mask renders revealed unless its id is in
  // `maskOverrides`. The baseline is decisive: clicking the global
  // toggle button flips this flag AND clears `maskOverrides` so the
  // new baseline takes hold cleanly. Per-span clicks toggle entries
  // in `maskOverrides` (dissent against the current baseline).
  //
  // Default `true` keeps launch behavior screen-share-safe (matches
  // earlier C3 PRs where `maskRevealed` started empty).
  var presenterMaskAll: Boolean = true

  // C3. Tracks the mask ids the user has flipped relative to the
  // current `presenterMaskAll` baseline. Reset on capture switch and
  // on Clear (the latter also resets the baseline to `true`).
  //
  // Mask ids are derived from the JSON dot-path (or the `m.…` /
  // `corr:…` prefixed variants for the messages tab and correlation
  // ids) at render time so they're stable across re-renders of the
  // same capture and survive the click → re-render → re-render
  // cycle.
  //
  // Renamed from `maskRevealed` in PR 5 to reflect the new
  // semantics: membership now means "flipped from baseline," not
  // "is revealed." Use [[isRevealed]] for the effective state.
  val maskOverrides: mutable.Set[String] = mutable.Set.empty

  /** Effective reveal state for a single mask id, collapsing the
    * global baseline with any per-span override.
    *
    *   - `presenterMaskAll=true,  override absent` → masked (false)
    *   - `presenterMaskAll=true,  override present` → revealed (true)
    *   - `presenterMaskAll=false, override absent` → revealed (true)
    *   - `presenterMaskAll=false, override present` → masked (false)
    *
    * i.e. `revealed = !presenterMaskAll XOR overrides.contains(id)`.
    */
  def isRevealed(maskId: String): Boolean =
    if (presenterMaskAll) maskOverrides.contains(maskId)
    else !maskOverrides.contains(maskId)
}
