package claudeproxymate.renderer.onboarding

import hedgehog.*
import hedgehog.runner.*

object TourCalloutViewSpec extends Properties {

  override def tests: List[Test] = List(
    example("build renders progress, title and body", testRendersText),
    example("build omits the Back button on the first step", testNoBackOnFirst),
    example("build renders the Back button after the first step", testBackAfterFirst),
    example("build always renders the next and skip actions", testNextAndSkipAlways),
    property("build never leaks raw <script> from labels", testNoScriptLeak),
  )

  private def labels(isFirst: Boolean): TourCalloutLabels =
    TourCalloutLabels(
      progress = "2 / 5",
      title = "Route Claude",
      body = "Global is the default.",
      back = "Back",
      next = "Next",
      skip = "Skip tour",
      isFirst = isFirst,
    )

  private def render(l: TourCalloutLabels): String = TourCalloutView.build(l).render

  private def action(name: String): String = s"""${TourCalloutView.ActionAttr}="$name""""

  def testRendersText: Result = {
    val html = render(labels(isFirst = false))
    Result.all(
      List("2 / 5", "Route Claude", "Global is the default.").map { text =>
        Result.assert(html.contains(text)).log(s"`$text` missing: $html")
      }
    )
  }

  def testNoBackOnFirst: Result = {
    val html = render(labels(isFirst = true))
    Result.assert(!html.contains(action("back"))).log(html)
  }

  def testBackAfterFirst: Result = {
    val html = render(labels(isFirst = false))
    Result.assert(html.contains(action("back"))).log(html)
  }

  def testNextAndSkipAlways: Result =
    Result.all(
      List(true, false).flatMap { isFirst =>
        val html = render(labels(isFirst))
        List("next", "skip").map { name =>
          Result.assert(html.contains(action(name))).log(s"`$name` missing (isFirst=$isFirst): $html")
        }
      }
    )

  def testNoScriptLeak: Property =
    for {
      evil <- Gen.string(Gen.alpha, Range.linear(0, 12)).log("evil")
    } yield {
      val payload = s"<script>alert('$evil')</script>"
      val html    = render(
        TourCalloutLabels(
          progress = payload,
          title = payload,
          body = payload,
          back = payload,
          next = payload,
          skip = payload,
          isFirst = false,
        )
      )
      Result.assert(!html.contains("<script>")).log(html)
    }
}
