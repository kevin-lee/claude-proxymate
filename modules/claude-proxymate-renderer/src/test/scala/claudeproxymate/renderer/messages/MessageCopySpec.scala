package claudeproxymate.renderer.messages

import claudeproxymate.renderer.copy.MaskedCopy
import claudeproxymate.renderer.messages.MsgContent.*
import claudeproxymate.renderer.messages.MsgPart.*
import claudeproxymate.renderer.state.AppState
import hedgehog.*
import hedgehog.runner.*

object MessageCopySpec extends Properties {

  override def tests: List[Test] = List(
    example("empty list returns empty string", testEmpty),
    example("single user card with TextMsgPart renders role + content", testSingleUserText),
    example("user card with InjectedMsgPart renders [label] + content", testInjectedMsgPart),
    example("assistant TextContent renders role + text", testAssistantText),
    example("ToolUseContent renders [tool: name] line", testToolUse),
    example("ToolResultContent truncated appends ellipsis", testToolResultTruncated),
    example("ToolResultContent not truncated has no ellipsis", testToolResultNotTruncated),
    example("OtherContent renders [typeName] label", testOtherContent),
    example("multiple cards separated by blank line", testMultipleCards),
    example("all-masked replaces sk-ant-… with ***", testMaskedRedacts),
    example("all-revealed preserves raw token verbatim", testRevealedPreserves),
    example("all-masked does not mask tool name (out of scope)", testMaskedNotToolName),
    // WYSIWYG: correlation ids follow the on-screen mask state
    example("masked correlation id (msg_…) copies as sentinel", testCorrIdMasked),
    example("revealed correlation id copies verbatim by exact id", testCorrIdRevealed),
    // WYSIWYG per-span id reconstruction (must mirror MessageView prefixes)
    example("TextContent span id is m.<rawIdx>.text.<partIdx>#<offset>", testTextContentId),
    example("TextMsgPart span id is m.<rawIdx>.user.<partIdx>#<offset>", testUserPartId),
    example("InjectedMsgPart span id uses partIdx, not badge uid", testInjectedPartId),
    example("ToolResultContent span id is m.<rawIdx>.tr.<partIdx>#<offset>", testToolResultId),
    example("counts: one revealed + one masked span", testCounts),
    example("counts: span-free cards have total == 0", testCountsSpanFree),
    example("copy queries exactly the ids the messages view renders", testRenderCopyIdAgreement),
  )

  private def textCard(role: String, text: String): MsgCard =
    MsgCard(role, contents = List(TextContent(text)), userParts = Nil, rawIdx = 0)

  private def userCard(parts: List[MsgPart]): MsgCard =
    MsgCard("user", contents = Nil, userParts = parts, rawIdx = 0)

  private val FakeAnthropic = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"
  private val FakeMsgId     = "msg_01ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  /* All-masked / all-revealed predicates: the two global baselines. */
  private val maskAll: String => Boolean   = _ => false
  private val revealAll: String => Boolean = _ => true

  def testEmpty: Result =
    Result
      .assert(MessageCopy.toPlainText(Nil, maskAll).text.isEmpty)
      .log("empty input should produce empty string")

  def testSingleUserText: Result = {
    val card = userCard(List(TextMsgPart("hello world")))
    val out  = MessageCopy.toPlainText(List(card), revealAll).text
    Result
      .assert(out == "# user\n\nhello world\n\n")
      .log(s"unexpected output: ${out.replace("\n", "\\n")}")
  }

  def testInjectedMsgPart: Result = {
    val card = userCard(List(InjectedMsgPart("u1", "system-reminder", "remind me", "badge-x")))
    val out  = MessageCopy.toPlainText(List(card), revealAll).text
    Result.all(
      List(
        Result.assert(out.contains("# user")).log(out),
        Result.assert(out.contains("[system-reminder]")).log(s"label missing: $out"),
        Result.assert(out.contains("remind me")).log(s"content missing: $out"),
      )
    )
  }

  def testAssistantText: Result = {
    val out = MessageCopy.toPlainText(List(textCard("assistant", "hi from claude")), revealAll).text
    Result
      .assert(out == "# assistant\n\nhi from claude\n\n")
      .log(s"unexpected: ${out.replace("\n", "\\n")}")
  }

  def testToolUse: Result = {
    val card = MsgCard("assistant", List(ToolUseContent("Read")), Nil, rawIdx = 0)
    val out  = MessageCopy.toPlainText(List(card), revealAll).text
    Result
      .assert(out.contains("[tool: Read]"))
      .log(s"tool-use line missing: $out")
  }

  def testToolResultTruncated: Result = {
    val card = MsgCard("assistant", List(ToolResultContent("preview", truncated = true)), Nil, rawIdx = 0)
    val out  = MessageCopy.toPlainText(List(card), revealAll).text
    Result.all(
      List(
        Result.assert(out.contains("[tool result] preview")).log(s"label missing: $out"),
        Result.assert(out.contains("…")).log(s"ellipsis missing: $out"),
      )
    )
  }

  def testToolResultNotTruncated: Result = {
    val card = MsgCard("assistant", List(ToolResultContent("done", truncated = false)), Nil, rawIdx = 0)
    val out  = MessageCopy.toPlainText(List(card), revealAll).text
    Result.all(
      List(
        Result.assert(out.contains("[tool result] done")).log(out),
        Result.assert(!out.contains("…")).log(s"unexpected ellipsis: $out"),
      )
    )
  }

  def testOtherContent: Result = {
    val card = MsgCard("assistant", List(OtherContent("image")), Nil, rawIdx = 0)
    val out  = MessageCopy.toPlainText(List(card), revealAll).text
    Result
      .assert(out.contains("[image]"))
      .log(s"other-type label missing: $out")
  }

  def testMultipleCards: Result = {
    val cards = List(textCard("user", "hi"), textCard("assistant", "hello"))
    val out   = MessageCopy.toPlainText(cards, revealAll).text
    Result.all(
      List(
        Result.assert(out.indexOf("# user") < out.indexOf("# assistant")).log(s"order wrong: $out"),
        Result.assert(out.contains("\n\n# assistant")).log(s"blank-line separator missing: $out"),
      )
    )
  }

  def testMaskedRedacts: Result = {
    val card = textCard("user", s"prefix $FakeAnthropic suffix")
    val res  = MessageCopy.toPlainText(List(card), maskAll)
    val out  = res.text
    Result.all(
      List(
        Result.assert(!out.contains(FakeAnthropic)).log(s"raw token leaked: $out"),
        Result.assert(out.contains(MaskedCopy.Sentinel)).log(s"sentinel missing: $out"),
        Result.assert(out.contains("prefix ")).log(s"surrounding text lost: $out"),
        Result.assert(out.contains(" suffix")).log(s"surrounding text lost: $out"),
        Result
          .assert(res.revealed == 0 && res.total == 1)
          .log(s"counts wrong: revealed=${res.revealed} total=${res.total}"),
      )
    )
  }

  def testRevealedPreserves: Result = {
    val card = textCard("user", FakeAnthropic)
    val out  = MessageCopy.toPlainText(List(card), revealAll).text
    Result
      .assert(out.contains(FakeAnthropic))
      .log(s"raw token missing under reveal-all: $out")
  }

  def testMaskedNotToolName: Result = {
    /* Even all-masked, tool names are emitted verbatim
     * (they're not secrets per PR 2 scope).
     */
    val card = MsgCard("assistant", List(ToolUseContent("Read")), Nil, rawIdx = 0)
    val out  = MessageCopy.toPlainText(List(card), maskAll).text
    Result
      .assert(out.contains("[tool: Read]"))
      .log(s"tool name missing: $out")
  }

  // ── WYSIWYG: correlation ids follow the on-screen mask state ──────────

  def testCorrIdMasked: Result = {
    val card = textCard("assistant", s"response id: $FakeMsgId done")
    val res  = MessageCopy.toPlainText(List(card), maskAll)
    Result.all(
      List(
        Result.assert(!res.text.contains(FakeMsgId)).log(s"masked correlation id leaked: ${res.text}"),
        Result.assert(res.text.contains(MaskedCopy.Sentinel)).log(s"sentinel missing: ${res.text}"),
        Result
          .assert(res.revealed == 0 && res.total == 1)
          .log(s"counts wrong: revealed=${res.revealed} total=${res.total}"),
      )
    )
  }

  def testCorrIdRevealed: Result = {
    // "response id: " is 13 chars, so the corr span starts at offset 13.
    val card = textCard("assistant", s"response id: $FakeMsgId done")
    val res  = MessageCopy.toPlainText(List(card), Set("corr:m.0.text.0#13"))
    Result.all(
      List(
        Result.assert(res.text.contains(FakeMsgId)).log(s"revealed correlation id missing: ${res.text}"),
        Result
          .assert(res.revealed == 1 && res.total == 1)
          .log(s"counts wrong: revealed=${res.revealed} total=${res.total}"),
      )
    )
  }

  // ── WYSIWYG per-span id reconstruction ─────────────────────────────────

  def testTextContentId: Result = {
    // Twin of MessageViewSpec.testTokenIdNamespace, which pins the
    // rendered id `m.7.text.0#0` for the same card shape.
    val card = MsgCard("assistant", List(TextContent(FakeAnthropic)), Nil, rawIdx = 7)
    val res  = MessageCopy.toPlainText(List(card), Set("m.7.text.0#0"))
    Result.all(
      List(
        Result.assert(res.text.contains(FakeAnthropic)).log(s"revealed token missing: ${res.text}"),
        Result
          .assert(res.revealed == 1 && res.total == 1)
          .log(s"counts wrong: revealed=${res.revealed} total=${res.total}"),
      )
    )
  }

  def testUserPartId: Result = {
    val card = MsgCard("user", contents = Nil, userParts = List(TextMsgPart(FakeAnthropic)), rawIdx = 2)
    val res  = MessageCopy.toPlainText(List(card), Set("m.2.user.0#0"))
    Result
      .assert(res.text.contains(FakeAnthropic) && res.revealed == 1)
      .log(s"user-part id mismatch: ${res.text} revealed=${res.revealed}")
  }

  def testInjectedPartId: Result = {
    // uid "u999" deliberately differs from the part index: ids must
    // derive from partIdx (deterministic) and ignore the badge uid.
    val card = MsgCard(
      "user",
      contents = Nil,
      userParts = List(TextMsgPart("x"), InjectedMsgPart("u999", "L", FakeAnthropic, "badge-x")),
      rawIdx = 3,
    )
    val res  = MessageCopy.toPlainText(List(card), Set("m.3.inj.1#0"))
    Result
      .assert(res.text.contains(FakeAnthropic) && res.revealed == 1)
      .log(s"injected-part id mismatch: ${res.text} revealed=${res.revealed}")
  }

  def testToolResultId: Result = {
    // "key " is 4 chars, so the token span starts at offset 4.
    val card = MsgCard("assistant", List(ToolResultContent(s"key $FakeAnthropic", truncated = false)), Nil, rawIdx = 4)
    val res  = MessageCopy.toPlainText(List(card), Set("m.4.tr.0#4"))
    Result
      .assert(res.text.contains(FakeAnthropic) && res.revealed == 1)
      .log(s"tool-result id mismatch: ${res.text} revealed=${res.revealed}")
  }

  def testCounts: Result = {
    val k1   = "sk-ant-abcdefghijklmnopqrstuvwxyz11111"
    val k2   = "AKIA0123456789ABCDEF"
    val card = MsgCard("assistant", List(TextContent(s"$k1 and $k2")), Nil, rawIdx = 0)
    val res  = MessageCopy.toPlainText(List(card), Set("m.0.text.0#0"))
    Result.all(
      List(
        Result.assert(res.text.contains(k1)).log(s"revealed token missing: ${res.text}"),
        Result.assert(!res.text.contains(k2)).log(s"masked token leaked: ${res.text}"),
        Result
          .assert(res.revealed == 1 && res.total == 2)
          .log(s"counts wrong: revealed=${res.revealed} total=${res.total}"),
      )
    )
  }

  def testCountsSpanFree: Result = {
    val cards = List(textCard("user", "hi"), MsgCard("assistant", List(ToolUseContent("Read")), Nil, rawIdx = 0))
    val res   = MessageCopy.toPlainText(cards, maskAll)
    Result
      .assert(res.total == 0 && res.revealed == 0)
      .log(s"span-free cards should have 0 counts: revealed=${res.revealed} total=${res.total}")
  }

  def testRenderCopyIdAgreement: Result = {
    /* The rendered messages view and the copy path must derive the
     * same span ids for the same cards — otherwise reveals on screen
     * silently stop applying to the clipboard. */
    val cards = List(
      MsgCard(
        "user",
        contents = Nil,
        userParts = List(
          TextMsgPart(s"typed $FakeAnthropic"),
          InjectedMsgPart("u42", "system-reminder", s"injected $FakeAnthropic", "badge-x"),
        ),
        rawIdx = 1,
      ),
      MsgCard(
        "assistant",
        contents = List(
          TextContent(s"id $FakeMsgId and key $FakeAnthropic"),
          ToolUseContent("Read"),
          ToolResultContent(s"result $FakeAnthropic", truncated = false),
        ),
        userParts = Nil,
        rawIdx = 2,
      ),
    )

    AppState.maskOverrides.clear()
    AppState.presenterMaskAll = true
    val html        = MessageView.buildCardsFrag(cards, isUserFilter = false, query = "").render
    val attrRe      = "data-(?:token|corr)-id=\"([^\"]+)\"".r
    val renderedIds = attrRe.findAllMatchIn(html).map(_.group(1)).toSet

    val queried = scala.collection.mutable.Set.empty[String]
    val _       = MessageCopy.toPlainText(cards, { id => queried += id; false })

    Result
      .assert(queried.toSet == renderedIds)
      .log(s"render ids: $renderedIds\ncopy ids: ${queried.toSet}")
  }
}
