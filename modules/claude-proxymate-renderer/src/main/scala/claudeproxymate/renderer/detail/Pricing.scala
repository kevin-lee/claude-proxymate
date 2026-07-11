package claudeproxymate.renderer.detail

import scala.scalajs.js

/** Pricing rates in $/MTok for a given model tier. */
final case class Rates(
  input: Double,
  output: Double,
  cacheRead: Double,
  cacheWrite5m: Double,
)

/** Pricing tier for a Claude model. */
enum ModelTier {
  case Fable5
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

  /* End of the Claude Sonnet 5 introductory-price window (2026-08-31,
   * 23:59:59 UTC). js.Date months are 0-based, so 7 = August. On or before
   * this instant Sonnet 5 bills at the intro rate; afterward it bills at the
   * standard Sonnet rate. */
  val Sonnet5IntroEndMillis: Double = js.Date.UTC(2026, 7, 31, 23, 59, 59)

  extension (tier: ModelTier) {

    /** Pricing rates in $/MTok for this tier at the given instant.
      *
      * Pure: only [[Sonnet5]] depends on the timestamp (intro-price window).
      */
    def ratesAt(epochMillis: Double): Rates = tier match {
      case Fable5 => Rates(10.0, 50.0, 1.0, 12.5)
      case OpusPremium => Rates(5.0, 25.0, 0.5, 6.25)
      case OpusLegacy => Rates(15.0, 75.0, 1.5, 18.75)
      case Sonnet5 =>
        if (epochMillis <= Sonnet5IntroEndMillis) Rates(2.0, 10.0, 0.2, 2.5)
        else Sonnet.ratesAt(epochMillis)
      case Sonnet => Rates(3.0, 15.0, 0.3, 3.75)
      case Haiku4_5 => Rates(1.0, 5.0, 0.1, 1.25)
      case Haiku3_5 => Rates(0.8, 4.0, 0.08, 1.0)
      case Haiku3 => Rates(0.25, 1.25, 0.03, 0.3)
      case Unknown => Sonnet.ratesAt(epochMillis)
    }

    /** Pricing rates in $/MTok for this tier as of now. */
    def rates: Rates = tier.ratesAt(js.Date.now())
  }

  /** Classify a model ID or alias into a pricing tier.
    *
    * Ordering matters: check most-specific substrings first to avoid the
    * generic Haiku / Opus / Sonnet branches swallowing newer sub-variants.
    */
  def forModel(model: String): ModelTier = {
    if (model.contains("fable-5") || model.contains("mythos-5")) Fable5
    else if (model.contains("haiku-4-5")) Haiku4_5
    else if (model.contains("3-5-haiku") || model.contains("haiku-3-5")) Haiku3_5
    else if (model.contains("3-haiku") || model.contains("haiku-3")) Haiku3
    else if (
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
    else if (model.contains("sonnet")) Sonnet
    else Unknown
  }
}
