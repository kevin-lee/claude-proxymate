package claudeproxymate.core.filter

import hedgehog.*
import hedgehog.runner.*

object ReminderBlocksSpec extends Properties {

  override def tests: List[Test] = List(
    property("join(split(text)) == text", testRoundTrip),
    example("blocks are classified with their inner text", testKinds),
    example("text without tags is one Plain block", testPlainOnly),
    example("adjacent reminders produce no empty Plain blocks", testNoEmptyPlain),
  )

  private def genWord: Gen[String] = Gen.string(Gen.alphaNum, Range.linear(0, 8))

  private def genSegment: Gen[String] =
    Gen.choice1(
      genWord,
      genWord.map(w => s"<system-reminder>$w</system-reminder>"),
      genWord.map(w => s"<command-message>$w</command-message>"),
      genWord.map(w => s"\n$w\n"),
    )

  def testRoundTrip: Property =
    for {
      segments <- Gen.list(genSegment, Range.linear(0, 8)).log("segments")
    } yield {
      val text = segments.mkString
      (ReminderBlocks.join(ReminderBlocks.split(text)) ==== text).log("split must be lossless")
    }

  def testKinds: Result = {
    val text   = "typed <system-reminder>inner</system-reminder> more <command-message>cmd</command-message>"
    val blocks = ReminderBlocks.split(text)
    (blocks ==== List(
      Block.Plain("typed "),
      Block.SystemReminder("<system-reminder>inner</system-reminder>", "inner"),
      Block.Plain(" more "),
      Block.CommandMessage("<command-message>cmd</command-message>"),
    )).log(s"unexpected blocks: $blocks")
  }

  def testPlainOnly: Result =
    (ReminderBlocks.split("just text") ==== List(Block.Plain("just text"))).log("plain text should be one block")

  def testNoEmptyPlain: Result = {
    val blocks = ReminderBlocks.split("<system-reminder>a</system-reminder><system-reminder>b</system-reminder>")
    Result
      .assert(!blocks.exists { case Block.Plain(t) => t.isEmpty; case _ => false })
      .log(s"empty Plain block emitted: $blocks")
  }
}
