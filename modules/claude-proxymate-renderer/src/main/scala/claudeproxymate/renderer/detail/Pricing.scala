package claudeproxymate.renderer.detail

/** Pricing rates in $/MTok for a given model tier. */
final case class Rates(
  input: Double,
  output: Double,
  cacheRead: Double,
  cacheWrite5m: Double,
)

/** Pricing tier for a Claude model. */
sealed trait ModelTier {
  def rates: Rates
}

object ModelTier {
  case object OpusPremium extends ModelTier {
    val rates: Rates = Rates(5.0, 25.0, 0.5, 6.25)
  }
  case object OpusLegacy extends ModelTier {
    val rates: Rates = Rates(15.0, 75.0, 1.5, 18.75)
  }
  case object Sonnet extends ModelTier {
    val rates: Rates = Rates(3.0, 15.0, 0.3, 3.75)
  }
  case object Haiku4_5 extends ModelTier {
    val rates: Rates = Rates(1.0, 5.0, 0.1, 1.25)
  }
  case object Haiku3_5 extends ModelTier {
    val rates: Rates = Rates(0.8, 4.0, 0.08, 1.0)
  }
  case object Haiku3 extends ModelTier {
    val rates: Rates = Rates(0.25, 1.25, 0.03, 0.3)
  }
  case object Unknown extends ModelTier {
    val rates: Rates = Sonnet.rates
  }

  /** Classify a model ID or alias into a pricing tier.
    *
    * Ordering matters: check most-specific substrings first to avoid the
    * generic Haiku / Opus branches swallowing newer sub-variants.
    */
  def forModel(model: String): ModelTier = {
    if (model.contains("haiku-4-5")) Haiku4_5
    else if (model.contains("3-5-haiku") || model.contains("haiku-3-5")) Haiku3_5
    else if (model.contains("3-haiku") || model.contains("haiku-3")) Haiku3
    else if (model.contains("opus-4-5") || model.contains("opus-4-6") || model.contains("opus-4-7")) OpusPremium
    else if (
      model.contains("opus-4-1") ||
      model.contains("opus-4-0") ||
      model.contains("opus-4-20") ||
      model.contains("3-opus") ||
      model.contains("opus-3")
    ) OpusLegacy
    else if (model.contains("sonnet")) Sonnet
    else Unknown
  }
}
