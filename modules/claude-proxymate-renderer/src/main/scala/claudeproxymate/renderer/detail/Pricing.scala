package claudeproxymate.renderer.detail

import scala.annotation.tailrec

/** Pricing rates in $/MTok for a given model tier. */
final case class Rates(
  input: Double,
  output: Double,
  cacheRead: Double,
  cacheWrite5m: Double,
  cacheWrite1h: Double,
)

/** Pricing tier for a Claude model. */
enum ModelTier {
  case Fable5_1
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

  /** Date the rates below were last verified against the official pricing
    * page. Shown in the token popover.
    */
  val PricingDate: String = "2026-09-10"

  extension (tier: ModelTier) {

    /** Pricing rates in $/MTok for this tier.
      *
      * Pure: every rate is a constant, so no clock is consulted. Cache reads
      * are 0.1x the base input price on every tier except [[Fable5_1]], which
      * bills them at 0.025x.
      */
    @tailrec
    def rates: Rates = tier match {
      case Fable5_1 => Rates(10.0, 50.0, 0.25, 12.5, 20.0)
      case Fable5 => Rates(10.0, 50.0, 1.0, 12.5, 20.0)
      case OpusPremium => Rates(5.0, 25.0, 0.5, 6.25, 10.0)
      case OpusLegacy => Rates(15.0, 75.0, 1.5, 18.75, 30.0)
      case Sonnet5 => Rates(2.0, 10.0, 0.2, 2.5, 4.0)
      case Sonnet => Rates(3.0, 15.0, 0.3, 3.75, 6.0)
      case Haiku4_5 => Rates(1.0, 5.0, 0.1, 1.25, 2.0)
      case Haiku3_5 => Rates(0.8, 4.0, 0.08, 1.0, 1.6)
      case Haiku3 => Rates(0.25, 1.25, 0.03, 0.3, 0.5)
      case Unknown => OpusPremium.rates
    }
  }

  /** Classify a model ID or alias into a pricing tier.
    *
    * Ordering matters, in two layers.
    *
    * The first layer matches known IDs, most-specific substring first, so the
    * broader branches cannot swallow a newer sub-variant. `claude-sonnet-4-5`
    * and `claude-3-5-sonnet-…` do not contain the substring `sonnet-5`, so the
    * Sonnet 5 branch is safe above them.
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
    else if (model.contains("opus")) OpusPremium
    else if (model.contains("sonnet")) Sonnet5
    else if (model.contains("haiku")) Haiku4_5
    else Unknown
  }
}
