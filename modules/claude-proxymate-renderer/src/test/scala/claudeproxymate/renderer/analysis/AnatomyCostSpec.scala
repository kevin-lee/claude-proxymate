package claudeproxymate.renderer.analysis

import hedgehog.*
import hedgehog.runner.*

object AnatomyCostSpec extends Properties {

  override def tests: List[Test] = List(
    property("fromUsage Haiku 4.5 computes exact cost", testHaikuCost),
    property("cacheHitPct math", testCacheHitPct),
    property("estimateOnly is estimated with empty segments", testEstimateOnly),
    property("unknown model falls back to Sonnet rates", testUnknownFallback),
  )

  /* Haiku 4.5 rates: input 1.0, output 5.0, cacheRead 0.1, cacheWrite5m 1.25 ($/MTok). */
  def testHaikuCost: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val card = AnatomyCost.fromUsage(
        model        = "claude-haiku-4-5-20251001",
        reqKb        = "10.0",
        inputTokens  = 1000,    // 1000 * 1.0 / 1e6 = 0.001
        cacheRead    = 2000,    // 2000 * 0.1 / 1e6 = 0.0002
        cacheWrite   = 500,     // 500 * 1.25 / 1e6 = 0.000625
        outputTokens = 400,     // 400 * 5.0 / 1e6 = 0.002
      )
      val expected = 0.001 + 0.0002 + 0.000625 + 0.002
      Result.assert(!card.estimated).log("should not be estimated") and
        Result.assert(math.abs(card.totalCostUsd - expected) < 1e-9).log(s"cost=${card.totalCostUsd} expected=$expected") and
        Result.assert(card.segments.size == 4).log(s"segments=${card.segments.size}")
    }

  def testCacheHitPct: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val card = AnatomyCost.fromUsage("claude-haiku-4-5-20251001", "1.0", inputTokens = 100, cacheRead = 900, cacheWrite = 0, outputTokens = 10)
      Result.assert(card.cacheHitPct == 90).log(s"cacheHitPct=${card.cacheHitPct}")
    }

  def testEstimateOnly: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val card = AnatomyCost.estimateOnly("claude-haiku-4-5-20251001", "32.1", estTokens = 9100)
      Result.assert(card.estimated).log("should be estimated") and
        Result.assert(card.segments.isEmpty).log("segments should be empty") and
        Result.assert(card.totalCostUsd == 0.0).log(s"cost=${card.totalCostUsd}") and
        Result.assert(card.estTokens == 9100).log(s"estTokens=${card.estTokens}")
    }

  /* Unknown model => ModelTier.Unknown which uses Sonnet rates (input 3.0). */
  def testUnknownFallback: Property =
    for { _ <- Gen.constant(()).forAll } yield {
      val card = AnatomyCost.fromUsage("totally-made-up-model", "1.0", inputTokens = 1000000, cacheRead = 0, cacheWrite = 0, outputTokens = 0)
      Result.assert(math.abs(card.totalCostUsd - 3.0) < 1e-9).log(s"cost=${card.totalCostUsd} (expected 3.0 from Sonnet input rate)")
    }
}
