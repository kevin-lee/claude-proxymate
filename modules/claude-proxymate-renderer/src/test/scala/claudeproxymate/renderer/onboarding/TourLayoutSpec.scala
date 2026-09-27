package claudeproxymate.renderer.onboarding

import claudeproxymate.core.TourPlacement
import claudeproxymate.renderer.onboarding.TourLayout.{CalloutPosition, Rect, Size}
import hedgehog.*
import hedgehog.runner.*

object TourLayoutSpec extends Properties {

  override def tests: List[Test] = List(
    property("the callout stays inside the viewport margins", testStaysInViewport),
    property("the preferred side is used whenever it has room", testPreferredWhenRoom),
    property("the callout does not overlap its target when the chosen side has room", testNoOverlap),
    property("the arrow offset stays inside the callout", testArrowInside),
    example("a Below target at the bottom edge flips to Above", testFlipsBelowToAbove),
  )

  final private case class Scenario(target: Rect, box: Size, viewport: Size, preferred: TourPlacement)

  private val genPlacement: Gen[TourPlacement] =
    Gen.element1(TourPlacement.Above, TourPlacement.Below, TourPlacement.Left, TourPlacement.Right)

  private val genScenario: Gen[Scenario] =
    for {
      vw        <- Gen.int(Range.linear(980, 2400))
      vh        <- Gen.int(Range.linear(620, 1400))
      bw        <- Gen.int(Range.linear(200, 360))
      bh        <- Gen.int(Range.linear(100, 240))
      tw        <- Gen.int(Range.linear(20, vw / 2))
      th        <- Gen.int(Range.linear(20, vh / 2))
      tl        <- Gen.int(Range.linear(0, vw - tw))
      tt        <- Gen.int(Range.linear(0, vh - th))
      preferred <- genPlacement
    } yield Scenario(
      Rect(tl.toDouble, tt.toDouble, tw.toDouble, th.toDouble),
      Size(bw.toDouble, bh.toDouble),
      Size(vw.toDouble, vh.toDouble),
      preferred,
    )

  private def placeFor(s: Scenario): CalloutPosition =
    TourLayout.place(s.target, s.box, s.viewport, s.preferred)

  private def sideHasRoom(s: Scenario, placement: TourPlacement): Boolean = placement match {
    case TourPlacement.Below => s.target.bottom + TourLayout.Gap + s.box.height <= s.viewport.height - TourLayout.Margin
    case TourPlacement.Above => s.target.top - TourLayout.Gap - s.box.height >= TourLayout.Margin
    case TourPlacement.Right => s.target.right + TourLayout.Gap + s.box.width <= s.viewport.width - TourLayout.Margin
    case TourPlacement.Left => s.target.left - TourLayout.Gap - s.box.width >= TourLayout.Margin
  }

  def testStaysInViewport: Property =
    for {
      s <- genScenario.log("scenario")
    } yield {
      val p                     = placeFor(s)
      val horizontal            =
        List(
          Result.assert(p.left >= TourLayout.Margin).log(s"left ${p.left}"),
          Result
            .assert(p.left + s.box.width <= s.viewport.width - TourLayout.Margin)
            .log(s"right edge ${p.left + s.box.width}"),
        )
      val vertical              =
        List(
          Result.assert(p.top >= TourLayout.Margin).log(s"top ${p.top}"),
          Result
            .assert(p.top + s.box.height <= s.viewport.height - TourLayout.Margin)
            .log(s"bottom edge ${p.top + s.box.height}"),
        )
      /* The cross axis is always clamped. The main axis can only fit when
       * the chosen side has room (no side may have room for a huge target). */
      val (crossAxis, mainAxis) = p.placement match {
        case TourPlacement.Above | TourPlacement.Below => (horizontal, vertical)
        case TourPlacement.Left | TourPlacement.Right => (vertical, horizontal)
      }
      Result.all(crossAxis ++ (if (sideHasRoom(s, p.placement)) mainAxis else Nil))
    }

  def testPreferredWhenRoom: Property =
    for {
      s <- genScenario.log("scenario")
    } yield {
      if (sideHasRoom(s, s.preferred)) placeFor(s).placement ==== s.preferred
      else Result.success
    }

  def testNoOverlap: Property =
    for {
      s <- genScenario.log("scenario")
    } yield {
      val p = placeFor(s)
      if (sideHasRoom(s, p.placement)) {
        val overlaps =
          p.left < s.target.right && p.left + s.box.width > s.target.left &&
            p.top < s.target.bottom && p.top + s.box.height > s.target.top
        Result.assert(!overlaps).log(s"callout $p overlaps target ${s.target}")
      } else Result.success
    }

  def testArrowInside: Property =
    for {
      s <- genScenario.log("scenario")
    } yield {
      val p    = placeFor(s)
      val size = p.placement match {
        case TourPlacement.Above | TourPlacement.Below => s.box.width
        case TourPlacement.Left | TourPlacement.Right => s.box.height
      }
      Result.all(
        List(
          Result.assert(p.arrowOffset >= TourLayout.ArrowInset).log(s"arrow ${p.arrowOffset}"),
          Result.assert(p.arrowOffset <= size - TourLayout.ArrowInset).log(s"arrow ${p.arrowOffset} of $size"),
        )
      )
    }

  def testFlipsBelowToAbove: Result = {
    val viewport = Size(1200, 800)
    val target   = Rect(500, 740, 120, 40)
    val p        = TourLayout.place(target, Size(300, 150), viewport, TourPlacement.Below)
    Result.all(
      List(
        p.placement ==== TourPlacement.Above,
        p.top ==== target.top - TourLayout.Gap - 150,
      )
    )
  }
}
