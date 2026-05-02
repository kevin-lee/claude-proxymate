package claudeproxymate.renderer.analysis

import hedgehog.*
import hedgehog.runner.*

object MechChipsViewSpec extends Properties {

  override def tests: List[Test] = List(
    // Empty cases
    example("buildChipsFrag empty list with no active key renders empty", testEmptyNoActive),
    example("buildChipsFrag empty list with active key renders empty", testEmptyWithActive),
    // Single chip
    example("buildChipsFrag single chip contains mech-chip class", testSingleChipClass),
    example("buildChipsFrag single chip carries data-key attribute", testSingleChipDataAttr),
    example("buildChipsFrag single chip carries cls subclass and 'found btn'", testSingleChipSubclasses),
    example("buildChipsFrag active key matching single chip adds 'active'", testSingleChipActive),
    // Multi chip
    example("buildChipsFrag multi chips render every key + label", testMultiChipsAllPresent),
    example("buildChipsFrag active key marks only matching chip as active", testActiveOnlyMatchingChip),
    example("buildChipsFrag with active key not matching any chip has no active", testActiveNoMatch),
    // Description banner
    example("buildChipsFrag descMeta None renders no banner", testDescBannerNone),
    example("buildChipsFrag descMeta Some renders all four banner pieces", testDescBannerSome),
    example("buildChipsFrag descMeta Some carries who/what/colour", testDescBannerContent),
    // XSS regression
    property("<script> in chip label never leaks raw", testNoScriptLeakInLabel),
    property("<script> in chip key never leaks raw", testNoScriptLeakInKey),
    property("<script> in ChipMeta who/what never leaks raw", testNoScriptLeakInDesc),
    // Inline-handler regression
    example("buildChipsFrag output never contains inline onclick", testNoInlineOnclick),
    // Active + desc combined
    example("buildChipsFrag active chip + desc banner render together", testActivePlusDesc),
  )

  private def render(
    chips: List[Chip],
    activeKey: Option[String] = None,
    descMeta: Option[ChipMeta] = None,
  ): String =
    MechChipsView.buildChipsFrag(chips, activeKey, descMeta).render

  private def chip(key: String = "cm_0", label: String = "📋 CLAUDE.md", cls: String = "cm", metaKey: String = "cm"): Chip =
    Chip(key, label, cls, metaKey)

  private def meta(color: String = "var(--green)", who: String = "Who", what: String = "What"): ChipMeta =
    ChipMeta(color, who, what)

  // ── Empty cases ───────────────────────────────────────────────────────

  def testEmptyNoActive: Result =
    render(Nil) ==== ""

  def testEmptyWithActive: Result =
    render(Nil, activeKey = Some("cm_0")) ==== ""

  // ── Single chip ───────────────────────────────────────────────────────

  def testSingleChipClass: Result = {
    val out = render(List(chip()))
    Result.assert(out.contains("mech-chip")).log(out)
  }

  def testSingleChipDataAttr: Result = {
    val out = render(List(chip(key = "sc_3")))
    Result.assert(out.contains("data-key=\"sc_3\"")).log(out)
  }

  def testSingleChipSubclasses: Result = {
    val out = render(List(chip(cls = "sk")))
    Result.all(
      List(
        Result.assert(out.contains("mech-chip sk found btn")).log(s"sk subclass missing: $out"),
        Result.assert(!out.contains(" active\"")).log(s"unexpected active: $out"),
      )
    )
  }

  def testSingleChipActive: Result = {
    val out = render(List(chip(key = "cm_0")), activeKey = Some("cm_0"))
    Result.assert(out.contains("mech-chip cm found btn active")).log(out)
  }

  // ── Multi chip ────────────────────────────────────────────────────────

  def testMultiChipsAllPresent: Result = {
    val chips = List(
      chip(key = "cm_0", label = "📋 CLAUDE.md", cls = "cm", metaKey = "cm"),
      chip(key = "sc_0", label = "⌨ /foo", cls = "sc", metaKey = "sc"),
      chip(key = "sk_0", label = "🔧 mySkill", cls = "sk", metaKey = "sk"),
    )
    val out = render(chips)
    Result.all(
      List(
        Result.assert(out.contains("data-key=\"cm_0\"")).log("cm_0 missing"),
        Result.assert(out.contains("data-key=\"sc_0\"")).log("sc_0 missing"),
        Result.assert(out.contains("data-key=\"sk_0\"")).log("sk_0 missing"),
        Result.assert(out.contains("CLAUDE.md")).log("cm label missing"),
        Result.assert(out.contains("/foo")).log("sc label missing"),
        Result.assert(out.contains("mySkill")).log("sk label missing"),
      )
    )
  }

  def testActiveOnlyMatchingChip: Result = {
    val chips = List(chip(key = "cm_0"), chip(key = "sc_0", cls = "sc"))
    val out   = render(chips, activeKey = Some("sc_0"))
    val activeCount = out.split(" active\"", -1).length - 1
    Result.assert(activeCount == 1)
      .log(s"expected 1 active, got $activeCount: $out")
  }

  def testActiveNoMatch: Result = {
    val chips = List(chip(key = "cm_0"), chip(key = "sc_0", cls = "sc"))
    val out   = render(chips, activeKey = Some("nonexistent"))
    Result.assert(!out.contains(" active\"")).log(s"unexpected active: $out")
  }

  // ── Description banner ────────────────────────────────────────────────

  def testDescBannerNone: Result = {
    val out = render(List(chip()), descMeta = None)
    Result.assert(!out.contains("mech-filter-desc")).log(s"unexpected banner: $out")
  }

  def testDescBannerSome: Result = {
    val out = render(List(chip()), descMeta = Some(meta()))
    Result.all(
      List(
        Result.assert(out.contains("class=\"mech-filter-desc\"")).log("desc class missing"),
        Result.assert(out.contains("class=\"mech-filter-desc-dot\"")).log("desc-dot missing"),
        Result.assert(out.contains("class=\"mech-filter-desc-body\"")).log("desc-body missing"),
        Result.assert(out.contains("class=\"mech-filter-desc-who\"")).log("desc-who missing"),
        Result.assert(out.contains("class=\"mech-filter-desc-what\"")).log("desc-what missing"),
      )
    )
  }

  def testDescBannerContent: Result = {
    val m   = meta(color = "var(--purple)", who = "WhoText", what = "WhatText")
    val out = render(List(chip()), descMeta = Some(m))
    Result.all(
      List(
        Result.assert(out.contains("WhoText")).log(s"who text missing: $out"),
        Result.assert(out.contains("WhatText")).log(s"what text missing: $out"),
        Result.assert(out.contains("background:var(--purple)")).log(s"colour missing: $out"),
      )
    )
  }

  // ── XSS regression ────────────────────────────────────────────────────

  def testNoScriptLeakInLabel: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val out = render(List(chip(label = s"<script>$chunk</script>")))
      Result.assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakInKey: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val out = render(List(chip(key = s"<script>$chunk</script>")))
      Result.assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  def testNoScriptLeakInDesc: Property =
    for {
      chunk <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("chunk")
    } yield {
      val m   = meta(who = s"<script>$chunk</script>", what = s"<script>$chunk</script>")
      val out = render(List(chip()), descMeta = Some(m))
      Result.assert(!out.contains("<script>"))
        .log(s"raw <script> leaked for chunk=$chunk: $out")
    }

  // ── Inline-handler regression ─────────────────────────────────────────

  def testNoInlineOnclick: Result = {
    val out = render(List(chip(), chip(key = "sc_0", cls = "sc")), descMeta = Some(meta()))
    Result.assert(!out.contains("onclick=")).log(s"unexpected inline onclick: $out")
  }

  // ── Active + desc combined ────────────────────────────────────────────

  def testActivePlusDesc: Result = {
    val chips = List(chip(key = "cm_0"), chip(key = "sc_0", cls = "sc"))
    val out   = render(chips, activeKey = Some("cm_0"), descMeta = Some(meta()))
    val activeCount = out.split(" active\"", -1).length - 1
    Result.all(
      List(
        activeCount ==== 1,
        Result.assert(out.contains("mech-filter-desc")).log("desc banner missing"),
      )
    )
  }
}
