package claudeproxymate.renderer.onboarding

import claudeproxymate.core.TourPlacement

/** Pure geometry for the guided tour: where a callout box goes next to
  * its spotlighted target, flipping to another side when the preferred
  * one has no room, and where its arrow points.
  */
object TourLayout {

  final case class Rect(left: Double, top: Double, width: Double, height: Double)
  object Rect {
    extension (r: Rect) {
      def right: Double   = r.left + r.width
      def bottom: Double  = r.top + r.height
      def centerX: Double = r.left + r.width / 2
      def centerY: Double = r.top + r.height / 2
    }
  }

  final case class Size(width: Double, height: Double)

  final case class CalloutPosition(placement: TourPlacement, left: Double, top: Double, arrowOffset: Double)

  /** Space between the target and the callout (the arrow sits in it). */
  val Gap: Double = 14.0

  /** Minimum distance between the callout and the viewport edges. */
  val Margin: Double = 12.0

  /** Minimum distance between the arrow and the callout's corners. */
  val ArrowInset: Double = 16.0

  def pad(r: Rect, by: Double): Rect =
    Rect(r.left - by, r.top - by, r.width + by * 2, r.height + by * 2)

  def place(target: Rect, box: Size, viewport: Size, preferred: TourPlacement): CalloutPosition = {
    val candidates = (preferred :: opposite(preferred) :: FallbackOrder).distinct
    val placement  = candidates.find(hasRoom(target, box, viewport, _)).getOrElse(preferred)

    val (left, top) = placement match {
      case TourPlacement.Below =>
        (clamp(target.centerX - box.width / 2, box.width, viewport.width), target.bottom + Gap)
      case TourPlacement.Above =>
        (clamp(target.centerX - box.width / 2, box.width, viewport.width), target.top - Gap - box.height)
      case TourPlacement.Right =>
        (target.right + Gap, clamp(target.centerY - box.height / 2, box.height, viewport.height))
      case TourPlacement.Left =>
        (target.left - Gap - box.width, clamp(target.centerY - box.height / 2, box.height, viewport.height))
    }

    val arrowOffset = placement match {
      case TourPlacement.Below | TourPlacement.Above =>
        clampArrow(target.centerX - left, box.width)
      case TourPlacement.Right | TourPlacement.Left =>
        clampArrow(target.centerY - top, box.height)
    }

    CalloutPosition(placement, left, top, arrowOffset)
  }

  private val FallbackOrder: List[TourPlacement] =
    List(TourPlacement.Below, TourPlacement.Above, TourPlacement.Right, TourPlacement.Left)

  private def opposite(placement: TourPlacement): TourPlacement = placement match {
    case TourPlacement.Below => TourPlacement.Above
    case TourPlacement.Above => TourPlacement.Below
    case TourPlacement.Right => TourPlacement.Left
    case TourPlacement.Left => TourPlacement.Right
  }

  private def hasRoom(target: Rect, box: Size, viewport: Size, placement: TourPlacement): Boolean =
    placement match {
      case TourPlacement.Below => target.bottom + Gap + box.height <= viewport.height - Margin
      case TourPlacement.Above => target.top - Gap - box.height >= Margin
      case TourPlacement.Right => target.right + Gap + box.width <= viewport.width - Margin
      case TourPlacement.Left => target.left - Gap - box.width >= Margin
    }

  /** Keep a box of `size` inside `[Margin, extent - Margin - size]`, or
    * at `Margin` when the box is larger than that range.
    */
  private def clamp(value: Double, size: Double, extent: Double): Double = {
    val max = extent - Margin - size
    if (max < Margin) Margin else math.min(math.max(value, Margin), max)
  }

  private def clampArrow(offset: Double, size: Double): Double =
    math.min(math.max(offset, ArrowInset), size - ArrowInset)
}
