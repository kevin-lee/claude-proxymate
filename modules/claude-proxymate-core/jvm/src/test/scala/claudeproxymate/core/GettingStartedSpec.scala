package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*
import java.nio.file.Path

object GettingStartedSpec extends Properties {

  private val i18nDir: Path = {
    val dir = System.getProperty("i18n.dir")
    if (dir == null) throw new IllegalStateException("System property 'i18n.dir' not set")
    Path.of(dir)
  }

  private lazy val ko: Map[String, String] = I18nPropertiesLoader.load(i18nDir.resolve("ko.properties"))
  private lazy val en: Map[String, String] = I18nPropertiesLoader.load(i18nDir.resolve("en.properties"))

  override def tests: List[Test] = List(
    example("shouldShow is true when nothing is stored", testShouldShowWhenMissing),
    example("shouldShow is true for the old carousel's stored \"1\"", testShouldShowForLegacyValue),
    example("shouldShow is false once the current version is stored", testShouldNotShowCurrentVersion),
    property("shouldShow(n) is true exactly when n < Version", testShouldShowInteger),
    property("shouldShow is true for a non-integer stored value", testShouldShowNonInteger),
    example("next(Intro) is the first tour step", testNextFromIntro),
    example("next walks Intro through every tour step to Done", testNextReachesDone),
    example("next(Done) stays Done", testNextFromDone),
    example("back(Tour(0)) stays on the first step", testBackFromFirstStep),
    property("back(Tour(i)) is Tour(i - 1) for i > 0", testBackStepsBack),
    example("back leaves Intro and Done unchanged", testBackFromIntroAndDone),
    example("tour target ids are distinct", testTargetIdsDistinct),
    example("every Getting Started text key exists in en and ko", testKeysExistInBothLocales),
  )

  def testShouldShowWhenMissing: Result =
    Result.assert(GettingStarted.shouldShow(None)).log("missing value should show")

  def testShouldShowForLegacyValue: Result =
    Result.assert(GettingStarted.shouldShow(Some("1"))).log("legacy \"1\" should show")

  def testShouldNotShowCurrentVersion: Result =
    Result
      .assert(!GettingStarted.shouldShow(Some(GettingStarted.Version.toString)))
      .log("the current version should not show again")

  def testShouldShowInteger: Property =
    for {
      n <- Gen.int(Range.linear(-100, 100)).log("n")
    } yield GettingStarted.shouldShow(Some(n.toString)) ==== (n < GettingStarted.Version)

  def testShouldShowNonInteger: Property =
    for {
      s <- Gen.string(Gen.alpha, Range.linear(1, 10)).log("s")
    } yield Result.assert(GettingStarted.shouldShow(Some(s)))

  def testNextFromIntro: Result =
    GettingStarted.next(OnboardingStage.Intro) ==== OnboardingStage.Tour(0)

  def testNextReachesDone: Result = {
    val stages   = List.iterate[OnboardingStage](OnboardingStage.Intro, GettingStarted.tourSteps.size + 2)(
      GettingStarted.next
    )
    val expected =
      OnboardingStage.Intro ::
        GettingStarted.tourSteps.indices.toList.map(OnboardingStage.Tour(_)) :::
        List(OnboardingStage.Done)
    stages ==== expected
  }

  def testNextFromDone: Result =
    GettingStarted.next(OnboardingStage.Done) ==== OnboardingStage.Done

  def testBackFromFirstStep: Result =
    GettingStarted.back(OnboardingStage.Tour(0)) ==== OnboardingStage.Tour(0)

  def testBackStepsBack: Property =
    for {
      i <- Gen.int(Range.linear(1, GettingStarted.tourSteps.size - 1)).log("i")
    } yield GettingStarted.back(OnboardingStage.Tour(i)) ==== OnboardingStage.Tour(i - 1)

  def testBackFromIntroAndDone: Result =
    Result.all(
      List(
        GettingStarted.back(OnboardingStage.Intro) ==== OnboardingStage.Intro,
        GettingStarted.back(OnboardingStage.Done) ==== OnboardingStage.Done,
      )
    )

  def testTargetIdsDistinct: Result = {
    val ids = GettingStarted.tourSteps.map(_.targetId)
    ids.distinct.size ==== ids.size
  }

  private val fixedKeys: List[String] = List(
    "tour.progress",
    "tour.back",
    "tour.next",
    "tour.done",
    "tour.skip",
    "onboard.title",
    "onboard.sub",
    "onboard.note",
    "onboard.skip",
    "onboard.replay",
    "onboard.tourBtn",
    "onboard.motionAlt",
    "onboard.motion.prompt",
    "onboard.motion.cap1",
    "onboard.motion.cap2",
    "onboard.motion.cap3",
    "onboard.motion.captured",
    "onboard.motion.tokens",
    "onboard.motion.tagline",
    "proxy.aboutGettingStarted",
  )

  def testKeysExistInBothLocales: Result = {
    val stepKeys = GettingStarted.tourSteps.flatMap(step => List(step.titleKey, step.bodyKey))
    val keys     = stepKeys ++ fixedKeys
    Result.all(
      List(
        keys.filterNot(en.contains) ==== Nil,
        keys.filterNot(ko.contains) ==== Nil,
      )
    )
  }
}
