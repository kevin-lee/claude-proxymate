package claudeproxymate.renderer.analysis

import claudeproxymate.renderer.detail.ModelTier

/** One colored slice of the cost breakdown. */
final case class CostSegment(label: String, tokens: Int, costUsd: Double, color: String)

/** Resolved Cost & Tokens card data. In estimate mode (`estimated = true`)
  * there is no usage, so the token-class split and cost are unavailable.
  */
final case class CostCard(
  model: String,
  estimated: Boolean,
  reqKb: String,
  totalInputTokens: Int,
  outputTokens: Int,
  cacheHitPct: Int,
  segments: List[CostSegment],
  totalCostUsd: Double,
  estTokens: Int,
)

/** Pure cost-card assembly. Reuses [[ModelTier]] pricing; no DOM, no
  * `js.Dynamic`, so it is unit-testable.
  *
  * Replicates the math in `DetailView.buildUsageTokenPill`.
  */
object AnatomyCost {

  def fromUsage(
    model: String,
    reqKb: String,
    inputTokens: Int,
    cacheRead: Int,
    cacheWrite: Int,
    outputTokens: Int,
  ): CostCard = {
    val rates    = ModelTier.forModel(model).rates
    val totalIn  = inputTokens + cacheRead + cacheWrite
    val cachePct = if (totalIn > 0) math.round(cacheRead.toDouble / totalIn * 100).toInt else 0

    def cost(tokens: Int, rate: Double): Double = tokens * rate / 1000000.0

    val uncachedCost   = cost(inputTokens, rates.input)
    val cacheReadCost  = cost(cacheRead, rates.cacheRead)
    val cacheWriteCost = cost(cacheWrite, rates.cacheWrite5m)
    val outputCost     = cost(outputTokens, rates.output)

    val segments = List(
      CostSegment("token.uncachedInput", inputTokens, uncachedCost, "var(--blue)"),
      CostSegment("token.cacheRead", cacheRead, cacheReadCost, "var(--green)"),
      CostSegment("token.cacheWrite", cacheWrite, cacheWriteCost, "var(--orange)"),
      CostSegment("token.output", outputTokens, outputCost, "var(--yellow)"),
    )

    CostCard(
      model = model,
      estimated = false,
      reqKb = reqKb,
      totalInputTokens = totalIn,
      outputTokens = outputTokens,
      cacheHitPct = cachePct,
      segments = segments,
      totalCostUsd = uncachedCost + cacheReadCost + cacheWriteCost + outputCost,
      estTokens = 0,
    )
  }

  def estimateOnly(model: String, reqKb: String, estTokens: Int): CostCard =
    CostCard(
      model = model,
      estimated = true,
      reqKb = reqKb,
      totalInputTokens = 0,
      outputTokens = 0,
      cacheHitPct = 0,
      segments = Nil,
      totalCostUsd = 0.0,
      estTokens = estTokens,
    )
}
