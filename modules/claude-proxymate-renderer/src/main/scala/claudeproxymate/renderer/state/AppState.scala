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

  // Field-name masking layer (C3). Tracks the dot-path ids the user
  // has clicked to reveal. Reset on capture switch and on Clear.
  // Mask ids are derived from the JSON dot-path at render time
  // (see `JsonTreeView`), so they are stable across re-renders of
  // the same capture and survive the click → re-render → re-render
  // cycle.
  val maskRevealed: mutable.Set[String] = mutable.Set.empty
}
