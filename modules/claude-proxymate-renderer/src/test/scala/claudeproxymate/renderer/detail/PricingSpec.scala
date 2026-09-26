package claudeproxymate.renderer.detail

import hedgehog.*
import hedgehog.runner.*

object PricingSpec extends Properties {

  override def tests: List[Test] = List(
    example("Fable 5.1 alias -> Fable5_1", testFable5_1),
    example("Mythos 5.1 alias -> Fable5_1", testMythos5_1),
    example("Fable 5 alias -> Fable5 (not Fable5_1)", testFable5),
    example("Mythos 5 alias -> Fable5 (not Fable5_1)", testMythos5),
    example("Opus 5.5 alias -> Opus5_5", testOpus5_5),
    example("Opus 5.5 Bedrock ID -> Opus5_5", testOpus5_5Bedrock),
    example("Opus 5 alias -> OpusPremium", testOpus5),
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
    example("unreleased Fable -> Fable5_1 (family fallback)", testFutureFable),
    example("unreleased Opus -> Opus5_5 (family fallback)", testFutureOpus),
    example("unreleased Sonnet -> Sonnet5 (family fallback)", testFutureSonnet),
    example("unreleased Haiku -> Haiku4_5 (family fallback)", testFutureHaiku),
    example("empty model -> Unknown", testEmptyUnknown),
    example("arbitrary string -> Unknown", testArbitraryUnknown),
    example("Fable5_1 rates match pricing doc", testRatesFable5_1),
    example("Fable5 rates match pricing doc", testRatesFable5),
    example("Opus5_5 rates match pricing doc", testRatesOpus5_5),
    example("OpusPremium rates match pricing doc", testRatesOpusPremium),
    example("OpusLegacy rates match pricing doc", testRatesOpusLegacy),
    example("Sonnet5 rates match pricing doc", testRatesSonnet5),
    example("Sonnet rates match pricing doc", testRatesSonnet),
    example("Haiku4_5 rates match pricing doc", testRatesHaiku4_5),
    example("Haiku3_5 rates match pricing doc", testRatesHaiku3_5),
    example("Haiku3 rates match pricing doc", testRatesHaiku3),
    example("Unknown rates equal OpusPremium rates (fallback)", testUnknownFallback),
    property("ratesFor Standard + Global equals rates on every tier", testStandardGlobalUnchanged),
    example("ratesFor Opus5_5 fast matches fast-mode pricing doc", testFastOpus5_5),
    example("ratesFor OpusPremium fast matches fast-mode pricing doc", testFastOpusPremium),
    example("ratesFor Sonnet5 US-only is 1.1x", testUsSonnet5),
    example("ratesFor Opus5_5 fast + US-only stacks to 2.2x", testFastUsOpus5_5),
    example("Speed.fromUsage fast -> Fast", testSpeedFast),
    example("Speed.fromUsage standard -> Standard", testSpeedStandard),
    example("Speed.fromUsage unknown string -> Standard", testSpeedUnknown),
    example("Speed.fromUsage missing -> Standard", testSpeedMissing),
    example("InferenceGeo.fromUsage us -> Us", testGeoUs),
    example("InferenceGeo.fromUsage global -> Global", testGeoGlobal),
    example("InferenceGeo.fromUsage missing -> Global", testGeoMissing),
    example("webSearchCost is $0.01 per search", testWebSearchCost),
  )

  import ModelTier.*

  def testFable5_1: Result         = ModelTier.forModel("claude-fable-5-1") ==== Fable5_1
  def testMythos5_1: Result        = ModelTier.forModel("claude-mythos-5-1") ==== Fable5_1
  def testFable5: Result           = ModelTier.forModel("claude-fable-5") ==== Fable5
  def testMythos5: Result          = ModelTier.forModel("claude-mythos-5") ==== Fable5
  def testOpus5_5: Result          = ModelTier.forModel("claude-opus-5-5") ==== Opus5_5
  def testOpus5_5Bedrock: Result   = ModelTier.forModel("anthropic.claude-opus-5-5") ==== Opus5_5
  def testOpus5: Result            = ModelTier.forModel("claude-opus-5") ==== OpusPremium
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
  def testFutureFable: Result      = ModelTier.forModel("claude-fable-6") ==== Fable5_1
  def testFutureOpus: Result       = ModelTier.forModel("claude-opus-6") ==== Opus5_5
  def testFutureSonnet: Result     = ModelTier.forModel("claude-sonnet-6") ==== Sonnet5
  def testFutureHaiku: Result      = ModelTier.forModel("claude-haiku-5") ==== Haiku4_5
  def testEmptyUnknown: Result     = ModelTier.forModel("") ==== Unknown
  def testArbitraryUnknown: Result = ModelTier.forModel("gpt-4") ==== Unknown

  def testRatesFable5_1: Result    = Fable5_1.rates ==== Rates(10.0, 50.0, 0.25, 12.5, 20.0)
  def testRatesFable5: Result      = Fable5.rates ==== Rates(10.0, 50.0, 1.0, 12.5, 20.0)
  def testRatesOpus5_5: Result     = Opus5_5.rates ==== Rates(4.0, 20.0, 0.2, 5.0, 8.0)
  def testRatesOpusPremium: Result = OpusPremium.rates ==== Rates(5.0, 25.0, 0.5, 6.25, 10.0)
  def testRatesOpusLegacy: Result  = OpusLegacy.rates ==== Rates(15.0, 75.0, 1.5, 18.75, 30.0)
  def testRatesSonnet5: Result     = Sonnet5.rates ==== Rates(2.0, 10.0, 0.2, 2.5, 4.0)
  def testRatesSonnet: Result      = Sonnet.rates ==== Rates(3.0, 15.0, 0.3, 3.75, 6.0)
  def testRatesHaiku4_5: Result    = Haiku4_5.rates ==== Rates(1.0, 5.0, 0.1, 1.25, 2.0)
  def testRatesHaiku3_5: Result    = Haiku3_5.rates ==== Rates(0.8, 4.0, 0.08, 1.0, 1.6)
  def testRatesHaiku3: Result      = Haiku3.rates ==== Rates(0.25, 1.25, 0.03, 0.3, 0.5)

  def testUnknownFallback: Result = Unknown.rates ==== OpusPremium.rates

  def testStandardGlobalUnchanged: Property =
    for {
      tier <- Gen.element(ModelTier.values.head, ModelTier.values.toList.drop(1)).log("tier")
    } yield tier.ratesFor(Speed.Standard, InferenceGeo.Global) ==== tier.rates

  def testFastOpus5_5: Result     =
    Opus5_5.ratesFor(Speed.Fast, InferenceGeo.Global) ==== Rates(8.0, 40.0, 0.4, 10.0, 16.0)
  def testFastOpusPremium: Result =
    OpusPremium.ratesFor(Speed.Fast, InferenceGeo.Global) ==== Rates(10.0, 50.0, 1.0, 12.5, 20.0)
  def testUsSonnet5: Result       =
    Sonnet5.ratesFor(Speed.Standard, InferenceGeo.Us) ==== Rates(2.2, 11.0, 0.22, 2.75, 4.4)
  def testFastUsOpus5_5: Result   =
    Opus5_5.ratesFor(Speed.Fast, InferenceGeo.Us) ==== Rates(8.8, 44.0, 0.44, 11.0, 17.6)

  def testSpeedFast: Result     = Speed.fromUsage(Some("fast")) ==== Speed.Fast
  def testSpeedStandard: Result = Speed.fromUsage(Some("standard")) ==== Speed.Standard
  def testSpeedUnknown: Result  = Speed.fromUsage(Some("turbo")) ==== Speed.Standard
  def testSpeedMissing: Result  = Speed.fromUsage(None) ==== Speed.Standard
  def testGeoUs: Result         = InferenceGeo.fromUsage(Some("us")) ==== InferenceGeo.Us
  def testGeoGlobal: Result     = InferenceGeo.fromUsage(Some("global")) ==== InferenceGeo.Global
  def testGeoMissing: Result    = InferenceGeo.fromUsage(None) ==== InferenceGeo.Global

  def testWebSearchCost: Result =
    Result
      .assert(math.abs(ModelTier.webSearchCost(3) - 0.03) < 1e-9)
      .log(s"webSearchCost(3)=${ModelTier.webSearchCost(3)}")
}
