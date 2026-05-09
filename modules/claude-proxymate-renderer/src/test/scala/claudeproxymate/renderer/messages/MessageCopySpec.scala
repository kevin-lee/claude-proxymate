package claudeproxymate.renderer.messages

import claudeproxymate.renderer.copy.MaskedCopy
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
    example("masked=true replaces sk-ant-… with ***", testMaskedRedacts),
    example("masked=false preserves raw token verbatim", testMaskedFalsePreserves),
    example("masked=true does not mask tool name (out of scope)", testMaskedNotToolName),
  )

  private def textCard(role: String, text: String): MsgCard =
    MsgCard(role, contents = List(TextContent(text)), userParts = Nil)

  private def userCard(parts: List[MsgPart]): MsgCard =
    MsgCard("user", contents = Nil, userParts = parts)

  private val FakeAnthropic = "sk-ant-abcdefghijklmnopqrstuvwxyz12345"

  def testEmpty: Result =
    Result.assert(MessageCopy.toPlainText(Nil, masked = true).isEmpty)
      .log("empty input should produce empty string")

  def testSingleUserText: Result = {
    val card = userCard(List(TextMsgPart("hello world")))
    val out  = MessageCopy.toPlainText(List(card), masked = false)
    Result.assert(out == "# user\n\nhello world\n\n")
      .log(s"unexpected output: ${out.replace("\n", "\\n")}")
  }

  def testInjectedMsgPart: Result = {
    val card = userCard(List(InjectedMsgPart("u1", "system-reminder", "remind me", "badge-x")))
    val out  = MessageCopy.toPlainText(List(card), masked = false)
    Result.all(
      List(
        Result.assert(out.contains("# user")).log(out),
        Result.assert(out.contains("[system-reminder]")).log(s"label missing: $out"),
        Result.assert(out.contains("remind me")).log(s"content missing: $out"),
      )
    )
  }

  def testAssistantText: Result = {
    val out = MessageCopy.toPlainText(List(textCard("assistant", "hi from claude")), masked = false)
    Result.assert(out == "# assistant\n\nhi from claude\n\n")
      .log(s"unexpected: ${out.replace("\n", "\\n")}")
  }

  def testToolUse: Result = {
    val card = MsgCard("assistant", List(ToolUseContent("Read")), Nil)
    val out  = MessageCopy.toPlainText(List(card), masked = false)
    Result.assert(out.contains("[tool: Read]"))
      .log(s"tool-use line missing: $out")
  }

  def testToolResultTruncated: Result = {
    val card = MsgCard("assistant", List(ToolResultContent("preview", truncated = true)), Nil)
    val out  = MessageCopy.toPlainText(List(card), masked = false)
    Result.all(
      List(
        Result.assert(out.contains("[tool result] preview")).log(s"label missing: $out"),
        Result.assert(out.contains("…")).log(s"ellipsis missing: $out"),
      )
    )
  }

  def testToolResultNotTruncated: Result = {
    val card = MsgCard("assistant", List(ToolResultContent("done", truncated = false)), Nil)
    val out  = MessageCopy.toPlainText(List(card), masked = false)
    Result.all(
      List(
        Result.assert(out.contains("[tool result] done")).log(out),
        Result.assert(!out.contains("…")).log(s"unexpected ellipsis: $out"),
      )
    )
  }

  def testOtherContent: Result = {
    val card = MsgCard("assistant", List(OtherContent("image")), Nil)
    val out  = MessageCopy.toPlainText(List(card), masked = false)
    Result.assert(out.contains("[image]"))
      .log(s"other-type label missing: $out")
  }

  def testMultipleCards: Result = {
    val cards = List(textCard("user", "hi"), textCard("assistant", "hello"))
    val out   = MessageCopy.toPlainText(cards, masked = false)
    Result.all(
      List(
        Result.assert(out.indexOf("# user") < out.indexOf("# assistant")).log(s"order wrong: $out"),
        Result.assert(out.contains("\n\n# assistant")).log(s"blank-line separator missing: $out"),
      )
    )
  }

  def testMaskedRedacts: Result = {
    val card = textCard("user", s"prefix $FakeAnthropic suffix")
    val out  = MessageCopy.toPlainText(List(card), masked = true)
    Result.all(
      List(
        Result.assert(!out.contains(FakeAnthropic)).log(s"raw token leaked: $out"),
        Result.assert(out.contains(MaskedCopy.Sentinel)).log(s"sentinel missing: $out"),
        Result.assert(out.contains("prefix ")).log(s"surrounding text lost: $out"),
        Result.assert(out.contains(" suffix")).log(s"surrounding text lost: $out"),
      )
    )
  }

  def testMaskedFalsePreserves: Result = {
    val card = textCard("user", FakeAnthropic)
    val out  = MessageCopy.toPlainText(List(card), masked = false)
    Result.assert(out.contains(FakeAnthropic))
      .log(s"raw token missing under masked=false: $out")
  }

  def testMaskedNotToolName: Result = {
    /* Even with masked=true, tool names are emitted verbatim
     * (they're not secrets per PR 2 scope).
     */
    val card = MsgCard("assistant", List(ToolUseContent("Read")), Nil)
    val out  = MessageCopy.toPlainText(List(card), masked = true)
    Result.assert(out.contains("[tool: Read]"))
      .log(s"tool name missing: $out")
  }
}
