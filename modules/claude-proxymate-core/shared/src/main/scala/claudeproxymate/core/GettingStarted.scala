package claudeproxymate.core

/** Which side of its target a guided-tour callout prefers. */
enum TourPlacement {
  case Above
  case Below
  case Left
  case Right
}

object TourPlacement {
  given cats.Eq[TourPlacement] = cats.Eq.fromUniversalEquals
}

/** One guided-tour step: the real element it spotlights (an [[HtmlIds]]
  * id) and the i18n keys of its callout text.
  */
final case class TourStep(targetId: String, titleKey: String, bodyKey: String, placement: TourPlacement)

/** Where the first-run Getting Started currently is: the motion intro
  * modal, a guided-tour step (index into [[GettingStarted.tourSteps]]),
  * or finished.
  */
enum OnboardingStage {
  case Intro
  case Tour(step: Int)
  case Done
}

object OnboardingStage {
  given cats.Eq[OnboardingStage] = cats.Eq.fromUniversalEquals
}

/** Pure model of the Getting Started onboarding (motion intro + guided
  * tour). Shared so JVM specs can check the tour targets against the
  * generated `index.html` and the keys against the `.properties` files.
  */
object GettingStarted {

  /** localStorage key holding the last onboarding [[Version]] the user has seen. */
  val StorageKey: String = "ci-onboarded"

  /** Onboarding content version. Version 1 is the old screenshot carousel,
    * which stored `"1"` under [[StorageKey]]. Bump this when the
    * onboarding content changes enough to show it again.
    */
  val Version: Int = 2

  /** Show when nothing is stored, the stored value is not an integer, or
    * it is an older version. A newer stored version (downgrade) never
    * re-shows.
    */
  def shouldShow(stored: Option[String]): Boolean =
    stored.flatMap(_.trim.toIntOption).forall(_ < Version)

  val tourSteps: List[TourStep] = List(
    TourStep(HtmlIds.ProxyStartBtn, "tour.start.title", "tour.start.body", TourPlacement.Below),
    TourStep(HtmlIds.RouteSeg, "tour.route.title", "tour.route.body", TourPlacement.Above),
    TourStep(HtmlIds.ProxyCmdBox, "tour.cmd.title", "tour.cmd.body", TourPlacement.Below),
    TourStep(HtmlIds.ProxyList, "tour.list.title", "tour.list.body", TourPlacement.Right),
    TourStep(HtmlIds.DetailTabs, "tour.tabs.title", "tour.tabs.body", TourPlacement.Below),
  )

  def next(stage: OnboardingStage): OnboardingStage = stage match {
    case OnboardingStage.Intro => OnboardingStage.Tour(0)
    case OnboardingStage.Tour(step) =>
      if (step + 1 < tourSteps.size) OnboardingStage.Tour(step + 1) else OnboardingStage.Done
    case OnboardingStage.Done => OnboardingStage.Done
  }

  def back(stage: OnboardingStage): OnboardingStage = stage match {
    case OnboardingStage.Tour(step) =>
      if (step > 0) OnboardingStage.Tour(step - 1) else stage
    case OnboardingStage.Intro | OnboardingStage.Done => stage
  }
}
