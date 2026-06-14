package claudeproxymate.renderer.analysis

import claudeproxymate.core.{AnomalyKind, RequestAnatomy, StructureFacts}
import hedgehog.*
import hedgehog.runner.*

object AnatomyViewSpec extends Properties {

  override def tests: List[Test] = List(
    example("cost estimated -> tag + note, no proportion bar rows", testCostEstimated),
    example("cost from usage -> cache-hit %, 4 segment rows, total", testCostFromUsage),
    example("attribution -> one row per segment, order preserved", testAttribution),
    example("structure -> message/turn/tool counts present", testStructure),
    example("inventory -> one row per row with count + tokens", testInventory),
    example("anomaly strip -> security class + glyph; empty -> none", testAnomalyStrip),
    property("<script> in labels/messages never leaks raw", testNoScriptLeak),
    example("no inline event handlers in output", testNoInlineHandlers),
  )

  private val labels = AnatomyLabels(
    costTitle = "Cost", attributionTitle = "Attribution (est.)", structureTitle = "Structure",
    inventoryTitle = "Inventory", anomaliesTitle = "Diagnostics",
    estimatedTag = "est.", noResponseNote = "No response captured",
    lblMessages = "Messages", lblTurns = "Turns", lblSystemBlocks = "system[]",
    lblTools = "Tools", lblToolUse = "tool_use", lblToolResult = "tool_result",
    lblImages = "Images", lblThinking = "thinking", lblStream = "stream",
  )

  private val structure = StructureFacts(
    messageCount = 8, userTurns = 4, assistantTurns = 3, systemBlocks = 3, cachedSystemBlocks = 2,
    toolsDefined = 14, toolUseCount = 0, toolResultCount = 0, imageCount = 0, thinkingBlocks = 3,
    stream = Some(false),
  )

  private val anatomy = RequestAnatomy(
    model = Some("claude-haiku-4-5-20251001"), totalEstBytes = 32000, totalEstTokens = 9100,
    segments = Nil, structure = structure, inventory = Nil, anomalies = Nil,
  )

  private def render(
    cost: CostCard,
    segments: List[SegmentRow] = Nil,
    inventory: List[InventoryDisplay] = Nil,
    anomalies: List[AnomalyDisplay] = Nil,
  ): String =
    AnatomyView.buildFrag(cost, anatomy, segments, inventory, anomalies, labels).render

  def testCostEstimated: Result = {
    val out = render(AnatomyCost.estimateOnly("m", "32.1", 9100))
    Result.all(List(
      Result.assert(out.contains("est.")).log("estimatedTag missing"),
      Result.assert(out.contains("No response captured")).log("note missing"),
      Result.assert(out.contains("9100")).log("est tokens missing"),
    ))
  }

  def testCostFromUsage: Result = {
    val out = render(AnatomyCost.fromUsage("claude-haiku-4-5-20251001", "10.0", 1000, 2000, 500, 400))
    Result.all(List(
      Result.assert(out.contains("cache hit")).log("cache hit missing"),
      Result.assert(out.contains(AnatomyView.BarClass)).log("bar missing"),
      Result.assert(out.contains("total")).log("total missing"),
    ))
  }

  def testAttribution: Result = {
    val segs = List(SegmentRow("system prompt", 2800, 31), SegmentRow("tools[]", 1715, 19))
    val out  = render(AnatomyCost.estimateOnly("m", "1.0", 1), segments = segs)
    Result.all(List(
      Result.assert(out.contains("system prompt")).log("seg1 missing"),
      Result.assert(out.contains("tools[]")).log("seg2 missing"),
      Result.assert(out.indexOf("system prompt") < out.indexOf("tools[]")).log("order not preserved"),
    ))
  }

  def testStructure: Result = {
    val out = render(AnatomyCost.estimateOnly("m", "1.0", 1))
    Result.all(List(
      Result.assert(out.contains("Messages")).log("messages label missing"),
      Result.assert(out.contains("4 / 3")).log("turns missing"),
      Result.assert(out.contains("Tools")).log("tools label missing"),
    ))
  }

  def testInventory: Result = {
    val inv = List(InventoryDisplay("CLAUDE.md", 2, 615), InventoryDisplay("Slash commands", 1, 0))
    val out = render(AnatomyCost.estimateOnly("m", "1.0", 1), inventory = inv)
    Result.all(List(
      Result.assert(out.contains("CLAUDE.md")).log("inv1 missing"),
      Result.assert(out.contains("~615 tok")).log("token count missing"),
      Result.assert(out.contains("Slash commands")).log("inv2 missing"),
    ))
  }

  def testAnomalyStrip: Result = {
    val withSec = render(
      AnatomyCost.estimateOnly("m", "1.0", 1),
      anomalies = List(AnomalyDisplay(AnomalyKind.Security, "exfil detected")),
    )
    val empty = render(AnatomyCost.estimateOnly("m", "1.0", 1), anomalies = Nil)
    Result.all(List(
      Result.assert(withSec.contains("anatomy-anomaly-security")).log("security class missing"),
      Result.assert(withSec.contains("🛡")).log("glyph missing"),
      Result.assert(!empty.contains("anatomy-anomaly")).log("unexpected anomaly in empty"),
    ))
  }

  def testNoScriptLeak: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val payload = s"<script>alert('$chunk')</script>"
      val segs    = List(SegmentRow(payload, 1, 1))
      val inv     = List(InventoryDisplay(payload, 1, 1))
      val anoms   = List(AnomalyDisplay(AnomalyKind.Info, payload))
      val out = render(AnatomyCost.estimateOnly(payload, "1.0", 1), segments = segs, inventory = inv, anomalies = anoms)
      Result.assert(!out.contains("<script>")).log(s"raw <script> leaked: $out")
    }

  def testNoInlineHandlers: Result = {
    val out = render(
      AnatomyCost.fromUsage("claude-haiku-4-5-20251001", "1.0", 100, 100, 100, 100),
      segments  = List(SegmentRow("s", 1, 1)),
      inventory = List(InventoryDisplay("i", 1, 1)),
      anomalies = List(AnomalyDisplay(AnomalyKind.Warn, "w")),
    )
    Result.all(List(
      Result.assert(!out.contains("onclick=")).log(s"onclick: $out"),
      Result.assert(!out.contains("oninput=")).log(s"oninput: $out"),
    ))
  }
}
