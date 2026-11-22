package claudeproxymate.renderer.json

import claudeproxymate.core.HtmlIds
import claudeproxymate.renderer.state.AppState
import claudeproxymate.renderer.testkit.DomTestKit
import hedgehog.*
import hedgehog.runner.*
import org.scalajs.dom

import scala.scalajs.js

/** The click handler of [[JsonTreeViewer]] and the methods it calls, in
  * jsdom. Each early exit of the handler has a test.
  *
  * A swap "in place" keeps the `#marker` element. The fallback
  * (`DetailView.renderProxyDetail()`) replaces the content of
  * `#proxyDetailView`, so `#marker` is gone.
  */
object JsonTreeViewerHandlerSpec extends Properties {

  override def tests: List[Test] = List(
    example("a click on an element that is not part of the tree changes nothing", testUnrelatedClick),
    /* Token masks */
    example("a token mask click reveals the token in place", testTokenRevealInPlace),
    example("a second token mask click hides the token in place", testTokenHideInPlace),
    example("a token mask with no detail code container re-renders the detail", testTokenNoContainer),
    example("a token mask that is not in the detail code re-renders the detail", testTokenNotInContainer),
    example("a token id with no # re-renders the detail", testTokenNoHash),
    example("a token id with a non-number offset re-renders the detail", testTokenBadOffset),
    example("a token mask with no selected capture re-renders the detail", testTokenNoCapture),
    example("a token mask on the response tab with no response re-renders the detail", testTokenNoResponse),
    example("a token id whose path is not a string re-renders the detail", testTokenNotString),
    example("a token id whose offset has no token re-renders the detail", testTokenNoMatch),
    example("a messages-tab token (m.) is left to MessageRenderer", testTokenMessagesTab),
    example("a token mask with an empty id goes on to the container toggle", testTokenEmptyId),
    /* Correlation-id masks */
    example("a correlation-id mask click reveals the id in place", testCorrRevealInPlace),
    example("a correlation id with no corr: prefix re-renders the detail", testCorrNoPrefix),
    example("a correlation id with no # re-renders the detail", testCorrNoHash),
    example("a correlation id with a non-number offset re-renders the detail", testCorrBadOffset),
    example("a correlation-id mask with no detail code container re-renders the detail", testCorrNoContainer),
    example("a correlation-id mask that is not in the detail code re-renders the detail", testCorrNotInContainer),
    example("a correlation-id mask with no selected capture re-renders the detail", testCorrNoCapture),
    example("a correlation id whose path is not a string re-renders the detail", testCorrNotString),
    example("a correlation id whose offset has no id re-renders the detail", testCorrNoMatch),
    example("a messages-tab correlation id (corr:m.) is left to MessageRenderer", testCorrMessagesTab),
    example("a correlation-id mask with an empty id goes on to the container toggle", testCorrEmptyId),
    /* Value masks */
    example("a value mask click reveals the value in place", testMaskRevealInPlace),
    example("a second value mask click hides the value in place", testMaskHideInPlace),
    example("a value mask with no detail code container re-renders the detail", testMaskNoContainer),
    example("a value mask that is not in the detail code re-renders the detail", testMaskNotInContainer),
    example("a value mask with no selected capture re-renders the detail", testMaskNoCapture),
    example("a value mask whose path gives null re-renders the detail", testMaskPathIsNull),
    example("a value mask with an empty id goes on to the container toggle", testMaskEmptyId),
    /* Containers */
    example("a container button click collapses the container", testContainerCollapse),
    example("a second container button click expands the container", testContainerExpand),
    example("a container summary click expands the container", testContainerSummaryClick),
    example("a container toggle with no body element changes nothing", testContainerNoBody),
    example("a container toggle with an empty id goes on to the string toggle", testContainerEmptyId),
    /* Long strings */
    example("a long-string toggle click expands the string and splits its lines once", testStringExpand),
    example("a second long-string toggle click collapses the string", testStringCollapse),
    example("expanding a long string again does not split its lines again", testStringExpandAgain),
    example("a long-string toggle with no body element changes nothing", testStringNoBody),
    example("a long-string toggle with an empty id changes nothing", testStringEmptyId),
    example("expanding a long string that is not in a lined tree does not split lines", testStringNotLined),
    /* renderJsonTree */
    example("renderJsonTree shows a string that is not JSON as text", testRenderNotJson),
    example("renderJsonTree parses a JSON string", testRenderJsonString),
    example("renderJsonTree renders an object", testRenderObject),
  )

  private val TokenId: String = "$.text#4"
  private val CorrId: String  = "corr:$.id#0"
  private val MaskId: String  = "$.api_key"

  private def body(): js.Dynamic =
    js.JSON
      .parse(
        s"""{"api_key":"secret-value","text":"use sk-ant-abcdefghijklmnopqrstuvwxyz12345 now","id":"msg_01ABCDEFGHIJKLMNOP","nested":{"a":1},"long":"${"x" * 301}"}"""
      )

  private val DetailShell: String =
    s"""<div id="${HtmlIds.ProxyDetailView}"><i id="marker"></i><div id="${HtmlIds.ProxyDetailCode}"></div></div>"""

  /** The detail view with the JSON tree of `data` in the detail code
    * container. `data` is the selected capture's request body, and the
    * detail tab is "request".
    */
  private def setUpTree(data: js.Dynamic): Unit = {
    DomTestKit.setUp(DetailShell)
    JsonTreeViewer.renderJsonTree(DomTestKit.html(s"#${HtmlIds.ProxyDetailCode}"), data)
    DomTestKit.select(DomTestKit.capture(1, data, js.Dynamic.literal()))
    AppState.proxyDetailTab = "request"
  }

  /** The detail view, with `extraHtml` after it. `extraHtml` is not in the
    * detail code container.
    */
  private def setUpOutside(extraHtml: String): Unit = {
    DomTestKit.setUp(DetailShell + extraHtml)
    DomTestKit.select(DomTestKit.capture(1, body(), js.Dynamic.literal()))
    AppState.proxyDetailTab = "request"
  }

  /** The detail view with `codeHtml` as the content of the detail code
    * container.
    */
  private def setUpInContainer(codeHtml: String): Unit = {
    DomTestKit.setUp(
      s"""<div id="${HtmlIds.ProxyDetailView}"><i id="marker"></i><div id="${HtmlIds.ProxyDetailCode}">$codeHtml</div></div>"""
    )
    DomTestKit.select(DomTestKit.capture(1, body(), js.Dynamic.literal()))
    AppState.proxyDetailTab = "request"
  }

  private def reRendered: Boolean = DomTestKit.elementOption("#marker").isEmpty

  private def noErrors: Result =
    DomTestKit.listenerErrors ==== Nil

  private def clickOn(selector: String): Unit = {
    val _ = DomTestKit.click(DomTestKit.element(selector))
  }

  private def displayOf(id: String): String =
    DomTestKit.html(s"#$id").style.display

  private def overrides: Set[String] = AppState.maskOverrides.toSet

  private def revealedInPlace(selector: String, id: String): Result =
    Result.all(
      List(
        overrides ==== Set(id),
        Result.assert(DomTestKit.elementOption(selector).isDefined).log(s"$selector is in the DOM"),
        Result.assert(!reRendered).log("the detail is not re-rendered"),
        noErrors,
      )
    )

  private def fellBack(id: String): Result =
    Result.all(
      List(
        overrides ==== Set(id),
        Result.assert(reRendered).log("the detail is re-rendered"),
        noErrors,
      )
    )

  // ── Unrelated click ─────────────────────────────────────────────────────

  def testUnrelatedClick: Result = {
    setUpTree(body())
    val before = DomTestKit.element(s"#${HtmlIds.ProxyDetailCode}").innerHTML
    clickOn("#marker")
    Result.all(
      List(
        DomTestKit.element(s"#${HtmlIds.ProxyDetailCode}").innerHTML ==== before,
        overrides ==== Set.empty[String],
        noErrors,
      )
    )
  }

  // ── Token masks ─────────────────────────────────────────────────────────

  def testTokenRevealInPlace: Result = {
    setUpTree(body())
    clickOn(s"""[data-token-id="$TokenId"]""")
    revealedInPlace(s""".jt-token-mask-revealed[data-token-id="$TokenId"]""", TokenId)
  }

  def testTokenHideInPlace: Result = {
    setUpTree(body())
    clickOn(s"""[data-token-id="$TokenId"]""")
    clickOn(s"""[data-token-id="$TokenId"]""")
    Result.all(
      List(
        overrides ==== Set.empty[String],
        Result
          .assert(DomTestKit.elementOption(s""".jt-token-mask[data-token-id="$TokenId"]""").isDefined)
          .log("the token is masked again"),
        Result.assert(!reRendered).log("the detail is not re-rendered"),
        noErrors,
      )
    )
  }

  def testTokenNoContainer: Result = {
    DomTestKit.setUp(
      s"""<div id="${HtmlIds.ProxyDetailView}"><i id="marker"></i></div><span class="jt-token-mask" data-token-id="$TokenId">t</span>"""
    )
    clickOn(s"""[data-token-id="$TokenId"]""")
    fellBack(TokenId)
  }

  def testTokenNotInContainer: Result = {
    setUpOutside(s"""<span class="jt-token-mask" data-token-id="$TokenId">t</span>""")
    clickOn(s"""[data-token-id="$TokenId"]""")
    fellBack(TokenId)
  }

  def testTokenNoHash: Result = {
    setUpInContainer("""<span class="jt-token-mask" data-token-id="$.text">t</span>""")
    clickOn("""[data-token-id="$.text"]""")
    fellBack("$.text")
  }

  def testTokenBadOffset: Result = {
    setUpInContainer("""<span class="jt-token-mask" data-token-id="$.text#x">t</span>""")
    clickOn("""[data-token-id="$.text#x"]""")
    fellBack("$.text#x")
  }

  def testTokenNoCapture: Result = {
    setUpTree(body())
    AppState.selectedProxyId = None
    clickOn(s"""[data-token-id="$TokenId"]""")
    fellBack(TokenId)
  }

  def testTokenNoResponse: Result = {
    setUpTree(body())
    AppState.proxyCaptures = List(js.Dynamic.literal(id = 1, method = "POST", url = "/v1/messages", body = body()))
    AppState.proxyDetailTab = "response"
    clickOn(s"""[data-token-id="$TokenId"]""")
    fellBack(TokenId)
  }

  def testTokenNotString: Result = {
    setUpInContainer("""<span class="jt-token-mask" data-token-id="$.nested#0">t</span>""")
    clickOn("""[data-token-id="$.nested#0"]""")
    fellBack("$.nested#0")
  }

  def testTokenNoMatch: Result = {
    setUpInContainer("""<span class="jt-token-mask" data-token-id="$.text#5">t</span>""")
    clickOn("""[data-token-id="$.text#5"]""")
    fellBack("$.text#5")
  }

  def testTokenMessagesTab: Result = {
    /* MessageRenderer toggles the id. If JsonTreeViewer also toggled it,
     * the two toggles would cancel out. */
    setUpOutside("""<span class="jt-token-mask" data-token-id="m.0.0#4">t</span>""")
    clickOn("""[data-token-id="m.0.0#4"]""")
    Result.all(List(overrides ==== Set("m.0.0#4"), noErrors))
  }

  def testTokenEmptyId: Result = {
    setUpOutside(
      """<span class="jt-btn" id="jt9-btn" data-jt-id="jt9"><span class="jt-token-mask" data-token-id="">t</span></span><span id="jt9-s" style="display:none">…</span><div id="jt9-b"></div>"""
    )
    clickOn("""[data-token-id=""]""")
    Result.all(
      List(
        displayOf("jt9-b") ==== "none",
        overrides ==== Set.empty[String],
        Result.assert(!reRendered).log("the detail is not re-rendered"),
        noErrors,
      )
    )
  }

  // ── Correlation-id masks ────────────────────────────────────────────────

  def testCorrRevealInPlace: Result = {
    setUpTree(body())
    clickOn(s"""[data-corr-id="$CorrId"]""")
    revealedInPlace(s""".jt-corr-mask-revealed[data-corr-id="$CorrId"]""", CorrId)
  }

  def testCorrNoPrefix: Result = {
    setUpInContainer("""<span class="jt-corr-mask" data-corr-id="$.id#0">c</span>""")
    clickOn("""[data-corr-id="$.id#0"]""")
    fellBack("$.id#0")
  }

  def testCorrNoHash: Result = {
    setUpInContainer("""<span class="jt-corr-mask" data-corr-id="corr:$.id">c</span>""")
    clickOn("""[data-corr-id="corr:$.id"]""")
    fellBack("corr:$.id")
  }

  def testCorrBadOffset: Result = {
    setUpInContainer("""<span class="jt-corr-mask" data-corr-id="corr:$.id#x">c</span>""")
    clickOn("""[data-corr-id="corr:$.id#x"]""")
    fellBack("corr:$.id#x")
  }

  def testCorrNoContainer: Result = {
    DomTestKit.setUp(
      s"""<div id="${HtmlIds.ProxyDetailView}"><i id="marker"></i></div><span class="jt-corr-mask" data-corr-id="$CorrId">c</span>"""
    )
    clickOn(s"""[data-corr-id="$CorrId"]""")
    fellBack(CorrId)
  }

  def testCorrNotInContainer: Result = {
    setUpOutside(s"""<span class="jt-corr-mask" data-corr-id="$CorrId">c</span>""")
    clickOn(s"""[data-corr-id="$CorrId"]""")
    fellBack(CorrId)
  }

  def testCorrNoCapture: Result = {
    setUpTree(body())
    AppState.selectedProxyId = None
    clickOn(s"""[data-corr-id="$CorrId"]""")
    fellBack(CorrId)
  }

  def testCorrNotString: Result = {
    setUpInContainer("""<span class="jt-corr-mask" data-corr-id="corr:$.nested#0">c</span>""")
    clickOn("""[data-corr-id="corr:$.nested#0"]""")
    fellBack("corr:$.nested#0")
  }

  def testCorrNoMatch: Result = {
    setUpInContainer("""<span class="jt-corr-mask" data-corr-id="corr:$.id#1">c</span>""")
    clickOn("""[data-corr-id="corr:$.id#1"]""")
    fellBack("corr:$.id#1")
  }

  def testCorrMessagesTab: Result = {
    setUpOutside("""<span class="jt-corr-mask" data-corr-id="corr:m.0.0#0">c</span>""")
    clickOn("""[data-corr-id="corr:m.0.0#0"]""")
    Result.all(List(overrides ==== Set("corr:m.0.0#0"), noErrors))
  }

  def testCorrEmptyId: Result = {
    setUpOutside(
      """<span class="jt-btn" id="jt9-btn" data-jt-id="jt9"><span class="jt-corr-mask" data-corr-id="">c</span></span><span id="jt9-s" style="display:none">…</span><div id="jt9-b"></div>"""
    )
    clickOn("""[data-corr-id=""]""")
    Result.all(
      List(
        displayOf("jt9-b") ==== "none",
        overrides ==== Set.empty[String],
        Result.assert(!reRendered).log("the detail is not re-rendered"),
        noErrors,
      )
    )
  }

  // ── Value masks ─────────────────────────────────────────────────────────

  def testMaskRevealInPlace: Result = {
    setUpTree(body())
    clickOn(s"""[data-mask-id="$MaskId"]""")
    revealedInPlace(s""".jt-mask-revealed[data-mask-id="$MaskId"]""", MaskId)
  }

  def testMaskHideInPlace: Result = {
    setUpTree(body())
    clickOn(s"""[data-mask-id="$MaskId"]""")
    clickOn(s"""[data-mask-id="$MaskId"]""")
    Result.all(
      List(
        overrides ==== Set.empty[String],
        Result
          .assert(DomTestKit.elementOption(s""".jt-mask[data-mask-id="$MaskId"]""").isDefined)
          .log("the value is masked again"),
        Result.assert(!reRendered).log("the detail is not re-rendered"),
        noErrors,
      )
    )
  }

  def testMaskNoContainer: Result = {
    DomTestKit.setUp(
      s"""<div id="${HtmlIds.ProxyDetailView}"><i id="marker"></i></div><span class="jt-mask" data-mask-id="$MaskId">m</span>"""
    )
    clickOn(s"""[data-mask-id="$MaskId"]""")
    fellBack(MaskId)
  }

  def testMaskNotInContainer: Result = {
    setUpOutside(s"""<span class="jt-mask" data-mask-id="$MaskId">m</span>""")
    clickOn(s"""[data-mask-id="$MaskId"]""")
    fellBack(MaskId)
  }

  def testMaskNoCapture: Result = {
    setUpTree(body())
    AppState.selectedProxyId = None
    clickOn(s"""[data-mask-id="$MaskId"]""")
    fellBack(MaskId)
  }

  def testMaskPathIsNull: Result = {
    setUpInContainer("""<span class="jt-mask" data-mask-id="$.nope.x">m</span>""")
    clickOn("""[data-mask-id="$.nope.x"]""")
    fellBack("$.nope.x")
  }

  def testMaskEmptyId: Result = {
    setUpOutside(
      """<span class="jt-btn" id="jt9-btn" data-jt-id="jt9"><span class="jt-mask" data-mask-id="">m</span></span><span id="jt9-s" style="display:none">…</span><div id="jt9-b"></div>"""
    )
    clickOn("""[data-mask-id=""]""")
    Result.all(
      List(
        displayOf("jt9-b") ==== "none",
        overrides ==== Set.empty[String],
        Result.assert(!reRendered).log("the detail is not re-rendered"),
        noErrors,
      )
    )
  }

  // ── Containers ──────────────────────────────────────────────────────────

  private val ContainerHtml: String =
    """<span class="jt-btn" id="jt9-btn" data-jt-id="jt9">▼</span><span class="jt-tag" id="jt9-s" data-jt-id="jt9" style="display:none">{…}</span><div id="jt9-b"></div>"""

  def testContainerCollapse: Result = {
    setUpOutside(ContainerHtml)
    clickOn("#jt9-btn")
    Result.all(
      List(
        displayOf("jt9-b") ==== "none",
        displayOf("jt9-s") ==== "",
        DomTestKit.element("#jt9-btn").textContent ==== "▶",
        noErrors,
      )
    )
  }

  def testContainerExpand: Result = {
    setUpOutside(ContainerHtml)
    clickOn("#jt9-btn")
    clickOn("#jt9-btn")
    Result.all(
      List(
        displayOf("jt9-b") ==== "",
        displayOf("jt9-s") ==== "none",
        DomTestKit.element("#jt9-btn").textContent ==== "▼",
        noErrors,
      )
    )
  }

  def testContainerSummaryClick: Result = {
    setUpOutside(ContainerHtml)
    clickOn("#jt9-btn")
    clickOn("#jt9-s")
    Result.all(List(displayOf("jt9-b") ==== "", displayOf("jt9-s") ==== "none", noErrors))
  }

  def testContainerNoBody: Result = {
    setUpOutside("""<span class="jt-btn" id="jt9-btn" data-jt-id="jt9">▼</span>""")
    clickOn("#jt9-btn")
    Result.all(List(DomTestKit.element("#jt9-btn").textContent ==== "▼", noErrors))
  }

  def testContainerEmptyId: Result = {
    setUpOutside(
      """<span class="jt-str-preview" id="jts9-s" data-jt-str-id="jts9"><span class="jt-btn" data-jt-id="">x</span></span><div id="jts9-b" style="display:none"></div>"""
    )
    clickOn("""[data-jt-id=""]""")
    Result.all(List(displayOf("jts9-b") ==== "", displayOf("jts9-s") ==== "none", noErrors))
  }

  // ── Long strings ────────────────────────────────────────────────────────

  private def stringId: String =
    DomTestKit.element(s".${JsonTreeView.StringToggleClass}[id]").getAttribute(JsonTreeView.StringDataAttr)

  def testStringExpand: Result = {
    setUpTree(body())
    val sid    = stringId
    clickOn(s"#$sid-btn")
    val frames = DomTestKit.runAnimationFrames()
    Result.all(
      List(
        displayOf(s"$sid-b") ==== "",
        displayOf(s"$sid-s") ==== "none",
        displayOf(s"$sid-btn") ==== "none",
        DomTestKit.html(s"#$sid-b").dataset.get("split") ==== Some("1"),
        frames ==== 1,
        noErrors,
      )
    )
  }

  def testStringCollapse: Result = {
    setUpTree(body())
    val sid = stringId
    clickOn(s"#$sid-btn")
    clickOn(s"#$sid-b .${JsonTreeView.StringToggleClass}")
    Result.all(
      List(
        displayOf(s"$sid-b") ==== "none",
        displayOf(s"$sid-s") ==== "",
        displayOf(s"$sid-btn") ==== "",
        noErrors,
      )
    )
  }

  def testStringExpandAgain: Result = {
    setUpTree(body())
    val sid    = stringId
    clickOn(s"#$sid-btn")
    val first  = DomTestKit.runAnimationFrames()
    clickOn(s"#$sid-b .${JsonTreeView.StringToggleClass}")
    clickOn(s"#$sid-btn")
    val second = DomTestKit.runAnimationFrames()
    Result.all(List(first ==== 1, second ==== 0, displayOf(s"$sid-b") ==== "", noErrors))
  }

  def testStringNoBody: Result = {
    setUpOutside("""<span class="jt-str-toggle" id="jts9-btn" data-jt-str-id="jts9">▶</span>""")
    clickOn("#jts9-btn")
    Result.all(List(displayOf("jts9-btn") ==== "", noErrors))
  }

  def testStringEmptyId: Result = {
    setUpOutside(
      """<span class="jt-str-toggle" id="jts9-btn" data-jt-str-id="">▶</span><div id="jts9-b" style="display:none"></div>"""
    )
    clickOn("#jts9-btn")
    Result.all(List(displayOf("jts9-b") ==== "none", noErrors))
  }

  def testStringNotLined: Result = {
    setUpOutside(
      """<span class="jt-str-toggle" id="jts9-btn" data-jt-str-id="jts9">▶</span><span id="jts9-s">…</span><div id="jts9-b" style="display:none"></div>"""
    )
    clickOn("#jts9-btn")
    Result.all(
      List(
        displayOf("jts9-b") ==== "",
        DomTestKit.runAnimationFrames() ==== 0,
        noErrors,
      )
    )
  }

  // ── renderJsonTree ──────────────────────────────────────────────────────

  private def renderInto(data: js.Dynamic): dom.html.Element = {
    DomTestKit.setUp("""<div id="tree"></div>""")
    val container = DomTestKit.html("#tree")
    JsonTreeViewer.renderJsonTree(container, data)
    container
  }

  def testRenderNotJson: Result = {
    val container = renderInto("not json {".asInstanceOf[js.Dynamic]) // scalafix:ok DisableSyntax.asInstanceOf
    Result.all(
      List(
        container.textContent ==== "not json {",
        Result.assert(!container.classList.contains("jt-lined")).log("no jt-lined class"),
      )
    )
  }

  def testRenderJsonString: Result = {
    val container = renderInto("""{"a":1}""".asInstanceOf[js.Dynamic]) // scalafix:ok DisableSyntax.asInstanceOf
    Result.all(
      List(
        Result.assert(container.classList.contains("jt-lined")).log("jt-lined class"),
        Result
          .assert(container.querySelector(".jt-line-info") != null)
          .log("line info"), // scalafix:ok DisableSyntax.null
        Result.assert(container.textContent.contains("\"a\"")).log(s"key a is shown: ${container.textContent}"),
      )
    )
  }

  def testRenderObject: Result = {
    val container = renderInto(js.Dynamic.literal(b = 2))
    Result.all(
      List(
        Result.assert(container.classList.contains("jt-lined")).log("jt-lined class"),
        Result.assert(container.textContent.contains("\"b\"")).log(s"key b is shown: ${container.textContent}"),
      )
    )
  }
}
