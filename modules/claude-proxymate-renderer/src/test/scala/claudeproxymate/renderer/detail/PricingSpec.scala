package claudeproxymate.renderer.detail

import hedgehog.*
import hedgehog.runner.*

object PricingSpec extends Properties {

  override def tests: List[Test] = List(
    example("Fable 5 alias -> Fable5", testFable5),
    example("Mythos 5 alias -> Fable5", testMythos5),
    example("Opus 4.8 alias -> OpusPremium", testOpus4_8),
    example("Opus 4.7 alias -> OpusPremium", testOpus4_7),
    example("Opus 4.6 alias -> OpusPremium", testOpus4_6),
    example("Opus 4.5 alias -> OpusPremium", testOpus4_5Alias),
    example("Opus 4.5 full ID -> OpusPremium", testOpus4_5FullId),
    example("Opus 4.1 alias -> OpusLegacy", testOpus4_1Alias),
    example("Opus 4.1 full ID -> OpusLegacy", testOpus4_1FullId),
    example("Opus 4.0 alias -> OpusLegacy", testOpus4_0Alias),
    example("Opus 4 legacy ID -> OpusLegacy (latent-bug fix)", testOpus4Legacy),
    example("Opus 3 full ID -> OpusLegacy", testOpus3),
    example("Sonnet 5 alias -> Sonnet5", testSonnet5),
    example("Sonnet 4.6 alias -> Sonnet", testSonnet4_6),
    example("Sonnet 4.5 alias -> Sonnet (not Sonnet5)", testSonnet4_5Alias),
    example("Sonnet 4.5 full ID -> Sonnet", testSonnet4_5FullId),
    example("Sonnet 4.0 alias -> Sonnet", testSonnet4_0Alias),
    example("Sonnet 4 legacy ID -> Sonnet", testSonnet4Legacy),
    example("Sonnet 3.7 full ID -> Sonnet", testSonnet3_7),
    example("Sonnet 3.5 full ID -> Sonnet (not Sonnet5)", testSonnet3_5),
    example("Haiku 4.5 alias -> Haiku4_5", testHaiku4_5Alias),
    example("Haiku 4.5 full ID -> Haiku4_5", testHaiku4_5FullId),
    example("Haiku 3.5 full ID -> Haiku3_5", testHaiku3_5),
    example("Haiku 3 full ID -> Haiku3", testHaiku3),
    example("empty model -> Unknown", testEmptyUnknown),
    example("arbitrary string -> Unknown", testArbitraryUnknown),
    example("Fable5 rates match pricing doc", testRatesFable5),
    example("OpusPremium rates match pricing doc", testRatesOpusPremium),
    example("OpusLegacy rates match pricing doc", testRatesOpusLegacy),
    example("Sonnet5 intro rates (<= cutoff)", testRatesSonnet5Intro),
    example("Sonnet5 standard rates (> cutoff)", testRatesSonnet5Standard),
    example("Sonnet rates match pricing doc", testRatesSonnet),
    example("Haiku4_5 rates match pricing doc", testRatesHaiku4_5),
    example("Haiku3_5 rates match pricing doc", testRatesHaiku3_5),
    example("Haiku3 rates match pricing doc", testRatesHaiku3),
    example("Unknown rates equal Sonnet rates (fallback)", testUnknownFallback),
  )

  import ModelTier.*

  /* A fixed instant just after the Sonnet 5 intro window, for deterministic
   * rate assertions independent of the wall clock. */
  private val afterCutoff: Double  = Sonnet5IntroEndMillis + 1.0
  private val beforeCutoff: Double = Sonnet5IntroEndMillis - 1.0

  def testFable5: Result           = ModelTier.forModel("claude-fable-5") ==== Fable5
  def testMythos5: Result          = ModelTier.forModel("claude-mythos-5") ==== Fable5
  def testOpus4_8: Result          = ModelTier.forModel("claude-opus-4-8") ==== OpusPremium
  def testOpus4_7: Result          = ModelTier.forModel("claude-opus-4-7") ==== OpusPremium
  def testOpus4_6: Result          = ModelTier.forModel("claude-opus-4-6") ==== OpusPremium
  def testOpus4_5Alias: Result     = ModelTier.forModel("claude-opus-4-5") ==== OpusPremium
  def testOpus4_5FullId: Result    = ModelTier.forModel("claude-opus-4-5-20251101") ==== OpusPremium
  def testOpus4_1Alias: Result     = ModelTier.forModel("claude-opus-4-1") ==== OpusLegacy
  def testOpus4_1FullId: Result    = ModelTier.forModel("claude-opus-4-1-20250805") ==== OpusLegacy
  def testOpus4_0Alias: Result     = ModelTier.forModel("claude-opus-4-0") ==== OpusLegacy
  def testOpus4Legacy: Result      = ModelTier.forModel("claude-opus-4-20250514") ==== OpusLegacy
  def testOpus3: Result            = ModelTier.forModel("claude-3-opus-20240229") ==== OpusLegacy
  def testSonnet5: Result          = ModelTier.forModel("claude-sonnet-5") ==== Sonnet5
  def testSonnet4_6: Result        = ModelTier.forModel("claude-sonnet-4-6") ==== Sonnet
  def testSonnet4_5Alias: Result   = ModelTier.forModel("claude-sonnet-4-5") ==== Sonnet
  def testSonnet4_5FullId: Result  = ModelTier.forModel("claude-sonnet-4-5-20250929") ==== Sonnet
  def testSonnet4_0Alias: Result   = ModelTier.forModel("claude-sonnet-4-0") ==== Sonnet
  def testSonnet4Legacy: Result    = ModelTier.forModel("claude-sonnet-4-20250514") ==== Sonnet
  def testSonnet3_7: Result        = ModelTier.forModel("claude-3-7-sonnet-20250219") ==== Sonnet
  def testSonnet3_5: Result        = ModelTier.forModel("claude-3-5-sonnet-20241022") ==== Sonnet
  def testHaiku4_5Alias: Result    = ModelTier.forModel("claude-haiku-4-5") ==== Haiku4_5
  def testHaiku4_5FullId: Result   = ModelTier.forModel("claude-haiku-4-5-20251001") ==== Haiku4_5
  def testHaiku3_5: Result         = ModelTier.forModel("claude-3-5-haiku-20241022") ==== Haiku3_5
  def testHaiku3: Result           = ModelTier.forModel("claude-3-haiku-20240307") ==== Haiku3
  def testEmptyUnknown: Result     = ModelTier.forModel("") ==== Unknown
  def testArbitraryUnknown: Result = ModelTier.forModel("gpt-4") ==== Unknown

  def testRatesFable5: Result         = Fable5.ratesAt(afterCutoff) ==== Rates(10.0, 50.0, 1.0, 12.5)
  def testRatesOpusPremium: Result    = OpusPremium.ratesAt(afterCutoff) ==== Rates(5.0, 25.0, 0.5, 6.25)
  def testRatesOpusLegacy: Result     = OpusLegacy.ratesAt(afterCutoff) ==== Rates(15.0, 75.0, 1.5, 18.75)
  def testRatesSonnet5Intro: Result   = Sonnet5.ratesAt(beforeCutoff) ==== Rates(2.0, 10.0, 0.2, 2.5)
  def testRatesSonnet5Standard: Result = Sonnet5.ratesAt(afterCutoff) ==== Rates(3.0, 15.0, 0.3, 3.75)
  def testRatesSonnet: Result         = Sonnet.ratesAt(afterCutoff) ==== Rates(3.0, 15.0, 0.3, 3.75)
  def testRatesHaiku4_5: Result       = Haiku4_5.ratesAt(afterCutoff) ==== Rates(1.0, 5.0, 0.1, 1.25)
  def testRatesHaiku3_5: Result       = Haiku3_5.ratesAt(afterCutoff) ==== Rates(0.8, 4.0, 0.08, 1.0)
  def testRatesHaiku3: Result         = Haiku3.ratesAt(afterCutoff) ==== Rates(0.25, 1.25, 0.03, 0.3)

  def testUnknownFallback: Result = Unknown.ratesAt(afterCutoff) ==== Sonnet.ratesAt(afterCutoff)
}
