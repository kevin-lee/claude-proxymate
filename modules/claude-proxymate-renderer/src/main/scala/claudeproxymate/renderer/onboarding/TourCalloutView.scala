package claudeproxymate.renderer.onboarding

import scalatags.Text.all.*

/** Localized text for one guided-tour callout. `next` is already the
  * "Done" label on the last step; `isFirst` hides the Back button.
  */
final case class TourCalloutLabels(
  progress: String,
  title: String,
  body: String,
  back: String,
  next: String,
  skip: String,
  isFirst: Boolean,
)

/** Pure view for the guided-tour callout content. Its buttons carry
  * [[ActionAttr]] (`next` / `back` / `skip`), which `Onboarding`
  * dispatches through its document-level click handler.
  */
object TourCalloutView {

  val ActionAttr: String = "data-tour-action"

  private val tourAction = attr(ActionAttr)

  def build(labels: TourCalloutLabels): Frag =
    frag(
      div(cls := "tour-callout-head")(
        span(cls := "tour-callout-progress")(labels.progress),
        button(tpe := "button", cls := "tour-callout-skip", tourAction := "skip")(labels.skip),
      ),
      div(cls := "tour-callout-title")(labels.title),
      div(cls := "tour-callout-body")(labels.body),
      div(cls := "tour-callout-actions")(
        if (labels.isFirst) frag()
        else button(tpe := "button", cls := "tour-callout-back", tourAction := "back")(labels.back),
        button(tpe := "button", cls := "onboard-btn-pill tour-callout-next", tourAction := "next")(labels.next),
      ),
    )
}
