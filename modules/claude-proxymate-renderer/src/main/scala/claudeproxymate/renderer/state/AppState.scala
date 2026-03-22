package claudeproxymate.renderer.state

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
}
