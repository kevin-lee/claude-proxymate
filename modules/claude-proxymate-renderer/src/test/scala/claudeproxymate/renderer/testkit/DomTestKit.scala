package claudeproxymate.renderer.testkit

import cats.syntax.all.*
import claudeproxymate.core.{HtmlIds, RouteMode}
import claudeproxymate.core.filter.FilterConfig
import claudeproxymate.renderer.analysis.MechChips
import claudeproxymate.renderer.copy.CopyUtil
import claudeproxymate.renderer.detail.{DtabListeners, TokenPopover}
import claudeproxymate.renderer.filter.MessageFilterMenu
import claudeproxymate.renderer.i18n.I18n
import claudeproxymate.renderer.json.JsonTreeViewer
import claudeproxymate.renderer.messages.MessageRenderer
import claudeproxymate.renderer.onboarding.Onboarding
import claudeproxymate.renderer.proxy.{ProxyControl, ProxyInfoPopover, ProxyList}
import claudeproxymate.renderer.search.ProxyDetailSearchListeners
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.theme.Theme
import org.scalajs.dom

import scala.scalajs.js

/** Test helper for the DOM event handlers (renderer/test runs in jsdom).
  *
  *   - Installs the document-level handlers once, in the order of
  *     `RendererMain`, so a dispatched event goes through all of them, as
  *     in the app.
  *   - Adds stand-ins for the browser APIs that jsdom does not have:
  *     `requestAnimationFrame` (the callbacks wait in a queue, see
  *     [[runAnimationFrames]]), `navigator.clipboard` (see
  *     [[clipboardTexts]]), `confirm` (see [[answerConfirm]]),
  *     `matchMedia`, `Element.scrollIntoView` and
  *     `Range.getBoundingClientRect`. Also a fake `setTimeout` (see
  *     [[withFakeTimers]]).
  *   - Resets the `AppState` fields and the document body before each test
  *     (see [[setUp]]).
  *
  * Effects that run in a timer, a debounce or a promise are not checked by
  * the tests.
  */
object DomTestKit {

  private val animationFrames: js.Array[js.Function1[Double, Any]] = js.Array()

  private val clipboard: js.Array[String] = js.Array()

  private val errors: js.Array[String] = js.Array()

  private val confirmPrompts: js.Array[String] = js.Array()

  /* The callbacks of the fake `setTimeout` (see [[withFakeTimers]]). The
   * handle of a callback is its index + 1. A cleared callback is undefined. */
  private val timers: js.Array[js.UndefOr[js.Function0[Any]]] = js.Array()

  /* The answer of the `window.confirm` stand-in (one element). */
  private val confirmReply: js.Array[Boolean] = js.Array(true)

  private lazy val installed: Unit = {
    addStandIns()
    TokenPopover.install()
    ProxyList.install()
    ProxyInfoPopover.install()
    JsonTreeViewer.install()
    MechChips.install()
    MessageRenderer.install()
    MessageFilterMenu.install()
    ProxyDetailSearchListeners.install()
    DtabListeners.install()
    Onboarding.install()
    Theme.install()
    I18n.install()
    ProxyControl.install()
    CopyUtil.install()
  }

  private def addStandIns(): Unit = {
    val window = dom.window.asInstanceOf[js.Dynamic] // scalafix:ok DisableSyntax.asInstanceOf
    /* jsdom reports an exception in an event listener as an "error" event
     * on the window. preventDefault stops jsdom from also logging it. */
    dom.window.addEventListener(
      "error",
      { (e: dom.ErrorEvent) =>
        val _ = errors.push(e.message)
        e.preventDefault()
      },
    )
    window.requestAnimationFrame = { (callback: js.Function1[Double, Any]) =>
      animationFrames.push(callback)
    }: js.Function1[js.Function1[Double, Any], Int]
    /* jsdom has no scrollIntoView, and no getBoundingClientRect on a Range. */
    window.Element.prototype.scrollIntoView = { () => () }: js.Function0[Unit]
    window.Range.prototype.getBoundingClientRect = { () =>
      js.Dynamic.literal(left = 0, top = 0, right = 0, bottom = 0, width = 0, height = 0)
    }: js.Function0[js.Dynamic]
    window.confirm = { (message: String) =>
      val _ = confirmPrompts.push(message)
      confirmReply(0)
    }: js.Function1[String, Boolean]
    window.matchMedia = { (_: String) =>
      js.Dynamic.literal(
        matches = false,
        addEventListener = ({ (_: String, _: js.Any) => () }: js.Function2[String, js.Any, Unit]),
        addListener = ({ (_: js.Any) => () }: js.Function1[js.Any, Unit]),
      )
    }: js.Function1[String, js.Dynamic]
    val clipboardObject = js.Dynamic.literal(
      writeText = ({ (text: String) =>
        val _ = clipboard.push(text)
        js.Promise.resolve[Unit](())
      }: js.Function1[String, js.Promise[Unit]])
    )
    val _ = js.Object.defineProperty(
      dom.window.navigator.asInstanceOf[js.Object], // scalafix:ok DisableSyntax.asInstanceOf
      "clipboard",
      js.Dynamic
        .literal(value = clipboardObject, configurable = true)
        .asInstanceOf[js.PropertyDescriptor], // scalafix:ok DisableSyntax.asInstanceOf
    )
  }

  /** Install the handlers (once), reset `AppState`, the clipboard texts and
    * the waiting animation frames, and set the document body to `bodyHtml`.
    * `bodyHtml` is computed after the reset, so it can use the views, which
    * read `AppState`.
    */
  def setUp(bodyHtml: => String): Unit = {
    installed
    /* Onboarding and MessageFilterMenu keep state in private fields: the
     * skip button sets the onboarding stage back to Done, and Escape closes
     * the badge and selection menus. */
    dom.document.body.innerHTML = s"""<button id="${HtmlIds.OnboardSkipBtn}"></button>"""
    val _ = click(dom.document.getElementById(HtmlIds.OnboardSkipBtn))
    val _ = keyDown(dom.document.body, "Escape")
    resetAppState()
    clipboard.length = 0
    errors.length = 0
    confirmPrompts.length = 0
    confirmReply(0) = true
    animationFrames.length = 0
    dom.document.body.innerHTML = bodyHtml
  }

  private def resetAppState(): Unit = {
    AppState.proxyCaptures = Nil
    AppState.selectedProxyId = none[Double]
    AppState.proxyDetailTab = "messages"
    AppState.msgFilter = "all"
    AppState.msgSearchQuery = ""
    AppState.proxyDetailSearch = ""
    AppState.proxyDetailMechFilter = none[String]
    AppState.proxyRunning = false
    AppState.proxyActualPort = 8888
    AppState.routeMode = RouteMode.default
    AppState.filterConfig = FilterConfig.default
    AppState.activeBadgeUid = none[String]
    AppState.activeBadgePart = none[String]
    AppState.searchCurrentIdx = -1
    AppState.imeComposing = false
    AppState.msgSearchWasFocused = false
    AppState.detailSearchWasFocused = false
    AppState.jtId = 0
    AppState.jtLine = 0
    AppState.badgeUidCounter = 0
    AppState.presenterMaskAll = true
    AppState.maskOverrides.clear()
  }

  /** Run the waiting `requestAnimationFrame` callbacks, also the ones that
    * they add. Gives the number of callbacks that ran.
    */
  def runAnimationFrames(): Int = {
    val callbacks = animationFrames.toList
    animationFrames.length = 0
    callbacks.foreach(callback => callback(0.0))
    if (callbacks.isEmpty) 0 else callbacks.length + runAnimationFrames()
  }

  /** The messages of the exceptions that the event listeners threw since
    * [[setUp]], oldest first.
    */
  def listenerErrors: List[String] = errors.toList

  /** The messages given to `window.confirm` since [[setUp]], oldest first. */
  def confirmMessages: List[String] = confirmPrompts.toList

  /** Make `window.confirm` answer `answer` (the default after [[setUp]] is
    * `true`).
    */
  def answerConfirm(answer: Boolean): Unit =
    confirmReply(0) = answer

  /** Run `body` with a fake `setTimeout` and `clearTimeout`: the callbacks
    * wait until [[runTimers]]. The real functions are put back after
    * `body`.
    */
  def withFakeTimers[A](body: => A): A = {
    val window        = dom.window.asInstanceOf[js.Dynamic] // scalafix:ok DisableSyntax.asInstanceOf
    val originalSet   = window.setTimeout
    val originalClear = window.clearTimeout
    timers.length = 0
    window.setTimeout = { (callback: js.Function0[Any], _: js.UndefOr[Double]) =>
      timers.push(callback)
    }: js.Function2[js.Function0[Any], js.UndefOr[Double], Int]
    window.clearTimeout = { (handle: js.UndefOr[Int]) =>
      handle.foreach(h => if (h >= 1 && h <= timers.length) timers(h - 1) = js.undefined else ())
    }: js.Function1[js.UndefOr[Int], Unit]
    try body
    finally {
      window.setTimeout = originalSet
      window.clearTimeout = originalClear
      timers.length = 0
    }
  }

  /** The number of fake timer callbacks that wait (see [[withFakeTimers]]). */
  def pendingTimers: Int = timers.count(t => !js.isUndefined(t))

  /** Run the waiting fake timer callbacks once (not the ones that they
    * add). Gives the number of callbacks that ran.
    */
  def runTimers(): Int = {
    val callbacks = timers.toList.flatMap(_.toOption)
    timers.length = 0
    callbacks.foreach(callback => callback())
    callbacks.length
  }

  /** The texts given to `navigator.clipboard.writeText`, oldest first. */
  def clipboardTexts: List[String] = clipboard.toList

  def element(selector: String): dom.Element =
    dom.document.querySelector(selector)

  def elementOption(selector: String): Option[dom.Element] =
    Option(dom.document.querySelector(selector))

  def html(selector: String): dom.html.Element =
    dom.document.querySelector(selector).asInstanceOf[dom.html.Element] // scalafix:ok DisableSyntax.asInstanceOf

  def click(target: dom.EventTarget): Boolean =
    target.dispatchEvent(
      new dom.MouseEvent(
        "click",
        new dom.MouseEventInit {
          bubbles = true
          cancelable = true
        },
      )
    )

  def mouseUp(target: dom.EventTarget): Boolean =
    target.dispatchEvent(
      new dom.MouseEvent(
        "mouseup",
        new dom.MouseEventInit {
          bubbles = true
          cancelable = true
        },
      )
    )

  def keyDown(target: dom.EventTarget, keyName: String): Boolean =
    target.dispatchEvent(
      new dom.KeyboardEvent(
        "keydown",
        new dom.KeyboardEventInit {
          key = keyName
          bubbles = true
          cancelable = true
        },
      )
    )

  /** Set the value of the input `input` and dispatch an `input` event on it. */
  def typeInto(input: dom.html.Input, value: String): Boolean = {
    input.value = value
    input.dispatchEvent(new dom.Event("input", new dom.EventInit { bubbles = true }))
  }

  def compositionStart(target: dom.EventTarget): Boolean =
    target.dispatchEvent(new dom.Event("compositionstart", new dom.EventInit { bubbles = true }))

  def compositionEnd(target: dom.EventTarget): Boolean =
    target.dispatchEvent(new dom.Event("compositionend", new dom.EventInit { bubbles = true }))

  /** A capture entry as the proxy events give it: `id`, `method`, `url`,
    * `body`, and the `response` with its `status` and `body`.
    */
  def capture(id: Double, requestBody: js.Any, responseBody: js.Any): js.Dynamic =
    js.Dynamic.literal(
      id = id,
      ts = "12:00:00",
      method = "POST",
      url = "/v1/messages",
      body = requestBody,
      response = js.Dynamic.literal(id = id, status = 200, body = responseBody),
    )

  /** Make `entry` the only capture, and select it. */
  def select(entry: js.Dynamic): Unit = {
    AppState.proxyCaptures = List(entry)
    AppState.selectedProxyId = entry.id.asInstanceOf[Double].some // scalafix:ok DisableSyntax.asInstanceOf
  }
}
