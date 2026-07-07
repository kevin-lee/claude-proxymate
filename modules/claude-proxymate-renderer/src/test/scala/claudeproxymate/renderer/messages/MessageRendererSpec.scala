package claudeproxymate.renderer.messages

import claudeproxymate.renderer.messages.MsgContent.*
import claudeproxymate.renderer.state.AppState
import hedgehog.*
import hedgehog.runner.*

import scala.scalajs.js

object MessageRendererSpec extends Properties {

  override def tests: List[Test] = List(
    // Response message appended to the capture's message list
    example("no response: cards come from request messages only", testNoResponse),
    example("assistant response is appended as the last card", testResponseAppended),
    example("appended response card gets rawIdx one past the request messages", testResponseRawIdx),
    example("string (unparsed/error) response body is not appended", testStringResponseBodyIgnored),
    example("error-object response body (no assistant role) is not appended", testErrorObjectResponseIgnored),
    example("user filter excludes the appended response card", testUserFilterExcludesResponse),
    example("assistant filter includes the appended response card", testAssistantFilterIncludesResponse),
    example("appending the response does not mutate the request body", testNonMutating),
  )

  // ── Helpers ───────────────────────────────────────────────────────────

  private def textBlock(text: String): js.Dynamic =
    js.Dynamic.literal("type" -> "text", "text" -> text)

  private def message(role: String, text: String): js.Dynamic =
    js.Dynamic.literal("role" -> role, "content" -> js.Array(textBlock(text)))

  private def requestEntry(messages: js.Dynamic*): js.Dynamic =
    js.Dynamic
      .literal(
        "id"   -> 1,
        "body" -> js.Dynamic.literal("messages" -> js.Array(messages*)),
      )

  private def withResponse(entry: js.Dynamic, responseBody: js.Any): js.Dynamic = {
    entry.updateDynamic("response")(js.Dynamic.literal("status" -> 200, "body" -> responseBody))
    entry
  }

  private def assistantResponseBody(text: String): js.Dynamic =
    js.Dynamic
      .literal(
        "role"    -> "assistant",
        "type"    -> "message",
        "content" -> js.Array(textBlock(text)),
      )

  private def visibleCards(entry: js.Dynamic, filter: String): List[MsgCard] = {
    AppState.msgFilter = filter
    AppState.msgSearchQuery = ""
    MessageRenderer.buildVisibleCards(entry)
  }

  private def cardText(card: MsgCard): String =
    card.contents.collect { case TextContent(t) => t }.mkString

  // ── Tests ─────────────────────────────────────────────────────────────

  def testNoResponse: Result = {
    val cards = visibleCards(requestEntry(message("user", "hello")), filter = "all")
    Result.assert(cards.map(_.role) == List("user")).log(s"cards: $cards")
  }

  def testResponseAppended: Result = {
    val entry = withResponse(requestEntry(message("user", "hello")), assistantResponseBody("hi there"))
    val cards = visibleCards(entry, filter = "all")
    Result
      .assert(cards.map(_.role) == List("user", "assistant"))
      .log(s"cards: $cards")
      .and(Result.assert(cards.lastOption.map(cardText).contains("hi there")).log(s"cards: $cards"))
  }

  def testResponseRawIdx: Result = {
    val entry = withResponse(
      requestEntry(message("user", "a"), message("assistant", "b"), message("user", "c")),
      assistantResponseBody("d"),
    )
    val cards = visibleCards(entry, filter = "all")
    Result.assert(cards.map(_.rawIdx) == List(0, 1, 2, 3)).log(s"rawIdx: ${cards.map(_.rawIdx)}")
  }

  def testStringResponseBodyIgnored: Result = {
    val entry = withResponse(requestEntry(message("user", "hello")), "upstream connect error")
    val cards = visibleCards(entry, filter = "all")
    Result.assert(cards.map(_.role) == List("user")).log(s"cards: $cards")
  }

  def testErrorObjectResponseIgnored: Result = {
    val errorBody = js
      .Dynamic
      .literal(
        "type"  -> "error",
        "error" -> js.Dynamic.literal("type" -> "overloaded_error", "message" -> "Overloaded"),
      )
    val entry     = withResponse(requestEntry(message("user", "hello")), errorBody)
    val cards     = visibleCards(entry, filter = "all")
    Result.assert(cards.map(_.role) == List("user")).log(s"cards: $cards")
  }

  def testUserFilterExcludesResponse: Result = {
    val entry = withResponse(requestEntry(message("user", "hello")), assistantResponseBody("hi"))
    val cards = visibleCards(entry, filter = "user")
    Result.assert(cards.map(_.role) == List("user")).log(s"cards: $cards")
  }

  def testAssistantFilterIncludesResponse: Result = {
    val entry = withResponse(requestEntry(message("user", "hello")), assistantResponseBody("hi"))
    val cards = visibleCards(entry, filter = "assistant")
    Result
      .assert(cards.map(_.role) == List("assistant"))
      .log(s"cards: $cards")
      .and(Result.assert(cards.headOption.map(cardText).contains("hi")).log(s"cards: $cards"))
  }

  def testNonMutating: Result = {
    val entry   = withResponse(requestEntry(message("user", "hello")), assistantResponseBody("hi"))
    val _       = visibleCards(entry, filter = "all")
    val reqMsgs = entry.body.messages.asInstanceOf[js.Array[js.Dynamic]]
    Result.assert(reqMsgs.length == 1).log(s"request messages length: ${reqMsgs.length}")
  }
}
