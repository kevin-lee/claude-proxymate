package claudeproxymate.renderer.detail

import scala.annotation.tailrec

/** Pricing rates in $/MTok for a given model tier. */
final case class Rates(
  input: Double,
  output: Double,
  cacheRead: Double,
  cacheWrite5m: Double,
  cacheWrite1h: Double,
) {

  /** Every rate multiplied by `factor`, rounded to 6 decimal places.
    *
    * The token popover prints the raw double, so without the rounding a
    * multiplied rate such as 0.3 x 1.1 would show as `0.33000000000000007`
    * instead of `0.33`. Every published rate has at most 3 decimals, so the
    * rounding loses nothing.
    */
  def scaled(factor: Double): Rates = {
    def scale(rate: Double): Double = math.round(rate * factor * 1000000.0) / 1000000.0
    Rates(
      input = scale(input),
      output = scale(output),
      cacheRead = scale(cacheRead),
      cacheWrite5m = scale(cacheWrite5m),
      cacheWrite1h = scale(cacheWrite1h),
    )
  }
}

/** Speed a response was served at, as reported in `usage.speed`.
  *
  * Fast mode bills every token rate at 2x standard. Claude Opus 4.6 accepts a
  * fast-mode request but runs it at standard speed and reports `"standard"`,
  * so the response field, not the request, decides the price.
  */
enum Speed(val multiplier: Double) {
  case Standard extends Speed(1.0)
  case Fast extends Speed(2.0)
}

object Speed {

  /** `Some("fast")` is [[Fast]]. Anything else, including a missing field in
    * an older capture, is [[Standard]].
    */
  def fromUsage(raw: Option[String]): Speed = raw match {
    case Some("fast") => Fast
    case _ => Standard
  }
}

/** Where inference ran, as reported in `usage.inference_geo`.
  *
  * US-only inference bills every token rate at 1.1x standard on Claude 4.6
  * and later. A workspace `default_inference_geo` can make a request US-only
  * without the request saying so, so the response field decides the price.
  */
enum InferenceGeo(val multiplier: Double) {
  case Global extends InferenceGeo(1.0)
  case Us extends InferenceGeo(1.1)
}

object InferenceGeo {

  /** `Some("us")` is [[Us]]. Anything else, including a missing field in an
    * older capture, is [[Global]].
    */
  def fromUsage(raw: Option[String]): InferenceGeo = raw match {
    case Some("us") => Us
    case _ => Global
  }
}

/** Pricing tier for a Claude model.
  *
  * Retired models stay in the table so older captures still price
  * correctly: [[OpusLegacy]] (Opus 4.1, 4 and 3), [[Haiku3_5]], [[Haiku3]],
  * and the Sonnet 4, 3.7 and 3.5 IDs inside [[Sonnet]].
  */
enum ModelTier {
  case Fable5_1
  case Fable5
  case Opus5_5
  case OpusPremium
  case OpusLegacy
  case Sonnet5
  case Sonnet
  case Haiku4_5
  case Haiku3_5
  case Haiku3
  case Unknown
}

object ModelTier {

  /** Date the rates below were last verified against the official pricing
    * page. Shown in the token popover.
    */
  val PricingDate: String = "2026-09-27"

  /** Web search costs $10 per 1,000 searches. It is billed per search, not
    * per token, so fast mode and inference geo do not affect it.
    */
  val WebSearchUsdPerSearch: Double = 0.01

  def webSearchCost(searches: Int): Double = searches * WebSearchUsdPerSearch

  extension (tier: ModelTier) {

    /** Pricing rates in $/MTok for this tier.
      *
      * Pure: every rate is a constant, so no clock is consulted. Cache reads
      * are 0.1x the base input price on every tier except [[Fable5_1]], which
      * bills them at 0.025x, and [[Opus5_5]], which bills them at 0.05x.
      */
    @tailrec
    def rates: Rates = tier match {
      case Fable5_1 => Rates(10.0, 50.0, 0.25, 12.5, 20.0)
      case Fable5 => Rates(10.0, 50.0, 1.0, 12.5, 20.0)
      case Opus5_5 => Rates(4.0, 20.0, 0.2, 5.0, 8.0)
      case OpusPremium => Rates(5.0, 25.0, 0.5, 6.25, 10.0)
      case OpusLegacy => Rates(15.0, 75.0, 1.5, 18.75, 30.0)
      case Sonnet5 => Rates(2.0, 10.0, 0.2, 2.5, 4.0)
      case Sonnet => Rates(3.0, 15.0, 0.3, 3.75, 6.0)
      case Haiku4_5 => Rates(1.0, 5.0, 0.1, 1.25, 2.0)
      case Haiku3_5 => Rates(0.8, 4.0, 0.08, 1.0, 1.6)
      case Haiku3 => Rates(0.25, 1.25, 0.03, 0.3, 0.5)
      case Unknown => OpusPremium.rates
    }

    /** Pricing rates in $/MTok for this tier at the given speed and inference
      * geo.
      *
      * The pricing page applies the prompt-caching multipliers and data
      * residency on top of fast-mode pricing, so all five rates scale by the
      * same stacked factor. When neither modifier applies, [[rates]] is
      * returned untouched, so ordinary captures price exactly as before.
      */
    def ratesFor(speed: Speed, inferenceGeo: InferenceGeo): Rates = {
      val factor = speed.multiplier * inferenceGeo.multiplier
      if (factor == 1.0) rates else rates.scaled(factor)
    }
  }

  /** Classify a model ID or alias into a pricing tier.
    *
    * Ordering matters, in two layers.
    *
    * The first layer matches known IDs, most-specific substring first, so the
    * broader branches cannot swallow a newer sub-variant. `opus-5-5` sits
    * above `opus-5` for the same reason `fable-5-1` sits above `fable-5`.
    * `claude-sonnet-4-5` and `claude-3-5-sonnet-…` do not contain the
    * substring `sonnet-5`, so the Sonnet 5 branch is safe above them.
    *
    * The second layer matches a bare family name and maps it to the *current*
    * tier of that family, so a model released after this table was written is
    * priced as that family's latest instead of falling through to
    * [[Unknown]].
    */
  def forModel(model: String): ModelTier = {
    if (model.contains("fable-5-1") || model.contains("mythos-5-1")) Fable5_1
    else if (model.contains("fable-5") || model.contains("mythos-5")) Fable5
    else if (model.contains("haiku-4-5")) Haiku4_5
    else if (model.contains("3-5-haiku") || model.contains("haiku-3-5")) Haiku3_5
    else if (model.contains("3-haiku") || model.contains("haiku-3")) Haiku3
    else if (model.contains("opus-5-5")) Opus5_5
    else if (
      model.contains("opus-5") ||
      model.contains("opus-4-5") ||
      model.contains("opus-4-6") ||
      model.contains("opus-4-7") ||
      model.contains("opus-4-8")
    ) OpusPremium
    else if (
      model.contains("opus-4-1") ||
      model.contains("opus-4-0") ||
      model.contains("opus-4-20") ||
      model.contains("3-opus") ||
      model.contains("opus-3")
    ) OpusLegacy
    else if (model.contains("sonnet-5")) Sonnet5
    else if (
      model.contains("sonnet-4") ||
      model.contains("3-7-sonnet") ||
      model.contains("3-5-sonnet") ||
      model.contains("sonnet-3")
    ) Sonnet
    else if (model.contains("fable") || model.contains("mythos")) Fable5_1
    else if (model.contains("opus")) Opus5_5
    else if (model.contains("sonnet")) Sonnet5
    else if (model.contains("haiku")) Haiku4_5
    else Unknown
  }
}
