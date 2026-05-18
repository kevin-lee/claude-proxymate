package claudeproxymate.core

/** DOM element IDs shared between the HTML generator (JVM) and renderer (JS). */
object HtmlIds {
  // ── Onboarding ──
  val OnboardModal: String    = "onboardModal"
  val OnboardCloseBtn: String = "onboardCloseBtn"

  // ── Header ──
  val BuildVer: String       = "buildVer"
  val UpdateBadge: String    = "updateBadge"
  val LogoIcon: String       = "logoIcon"
  val LangToggleBtn: String  = "langToggleBtn"
  val ThemeToggleBtn: String = "themeToggleBtn"

  // ── Proxy Bar ──
  val ProxyBar: String        = "proxyBar"
  val ProxyInfoBtn: String    = "proxyInfoBtn"
  val ProxyCmdCopyBtn: String = "proxyCmdCopyBtn"
  val ProxyClearBtn: String   = "proxyClearBtn"
  val MaskToggleBtn: String   = "maskToggleBtn"
  val MaskStateChip: String   = "maskStateChip"

  // ── Proxy Panel ──
  val ProxyPanel: String = "proxyPanel"

  // ── Proxy Control ──
  val ProxyPort: String       = "proxyPort"
  val ProxyStatus: String     = "proxyStatus"
  val ProxyStatusText: String = "proxyStatusText"
  val ProxyCmdBox: String     = "proxyCmdBox"
  val ProxyCmdText: String    = "proxyCmdText"
  val ProxyStartBtn: String   = "proxyStartBtn"

  // ── Proxy Stream ──
  val ProxyList: String       = "proxyList"
  val ProxyCount: String      = "proxyCount"
  val ProxyDetailView: String = "proxyDetailView"
  val CopyDetailBtn: String   = "copyDetailBtn"

  // ── Dynamically created (not in index.html, but used across renderer modules) ──
  val ProxyDetailCode: String        = "proxyDetailCode"
  val ProxyDetailSearchInput: String = "proxyDetailSearchInput"
  val SearchCounter: String          = "searchCounter"
  val MsgSearchInput: String         = "msgSearchInput"
  val MsgCountEl: String             = "msgCountEl"
}
