package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.math.geom.{DPoint, Line, Point}

/**
  * A single event in the event queue (the beginning or end of a segment, or the intersection of two or more segments).
  */
abstract class EventPoint[L <: Line](point: DPoint, t: PointType) {

  def isSource: Boolean = t == Source
  def isTarget: Boolean = t == Target
  def isIntersection: Boolean = t == Intersect
  def coordinate: DPoint = point

  /**
    * Determines if this event should occur before another in the event queue by sorting based on coordinates (first
    * x, then y if both events have the same x-coordinates). Handles deadlocks in case the coordinates themselves are
    * equal by considering event type and segment slopes.
    */
  def before(other: EventPoint[L]): Boolean = {
    if (EventPoint.coordinateLessThan(this.coordinate, other.coordinate)) {
      true
    } else if (other == this) {

      /* Special case not mentioned in the algorithm description. This is needed to keep the tree structures from
       * considering one event to be before another when they're the same event. Should be safe to include since
       * no two events should contain the exact same data (if nothing else, the segment id's will differ).
       */
      false

    } else if (coordinate == other.coordinate) {
      (this, other) match {
        case (SegmentPoint(_, Source, _), SegmentPoint(_, Target, _))
             | (SegmentPoint(_, Source, _), Intersection(_, _, _, _))
             | (Intersection(_, _, _, _), SegmentPoint(_, Target, _))=> true
        case (SegmentPoint(_, Source, ps), SegmentPoint(_, Source, qs)) =>

          /* Here we have a special case: If two source points belong to collinear segments, neither will be greater
           * than the other. In this case (per the original article) event order is unimportant. Segment id is used
           * as a tiebreaker.
           */
          if (ps.collinearWith(qs))
            ps.id < qs.id
          else
            ps.slopeGreaterThan(qs)

        case (SegmentPoint(_, Target, ps), SegmentPoint(_, Target, qs)) =>

          /* Same case as above. */
          if (ps.collinearWith(qs))
            ps.id < qs.id
          else
            ps.slopeLessThan(qs)

        case (Intersection(p, pUpper, pLower, _), Intersection(q, qUpper, qLower, _)) =>
          pLower.slopeGreaterThan(qUpper)

        case _ => false
      }

    } else {
      false
    }
  }

  def coordinateLessThan(other: EventPoint[L]): Boolean = coordinate.x < other.coordinate.x || (coordinate.x == other.coordinate.x && coordinate.y < other.coordinate.y)

}

object EventPoint {
  def coordinateLessThan(a: DPoint, b: DPoint): Boolean = a.x < b.x || (a.x == b.x && a.y < b.y)
  def coordinateLessThan(a: Point, b: Point): Boolean = coordinateLessThan(a.toDouble, b.toDouble)
}

case class SegmentPoint[L <: Line](p: Point, t: PointType, segment: Segment[L]) extends EventPoint[L](p, t)

/**
  * Note: The segments in-between are not sorted according to above/below status. Whenever an intersection with n
  * segments are updated by receiving additional segments, if any of the new segments are higher/lower than the current
  * upper/lower segment, they will take their place. Whatever segments remains will be stored in-between.
  */
case class Intersection[L <: Line](p: DPoint, upper: Segment[L], lower: Segment[L], inBetween: Set[Segment[L]]) extends EventPoint[L](p, Intersect) {
  def update(segment: Segment[L]): Intersection[L] = {
    if (upper == segment || lower == segment || inBetween.contains(segment)) {
      this
    } else {
      require(segment.containsPoint(p), "The segment " + segment + " doesn't intersect the point " + p + ", and cannot be added to its intersection.")
      val newUpper = if (upper.below(p.x, segment)) segment else upper
      val newLower = if (segment.below(p.x, lower)) segment else lower
      val newInBetween = if (newUpper != upper) inBetween + upper
      else if (newLower != lower) inBetween + lower
      else inBetween + segment

      Intersection(p, newUpper, newLower, newInBetween)
    }
  }
  def allSegments: Vector[Segment[L]] = Vector(upper, lower) ++ inBetween
}

sealed trait PointType
case object Source extends PointType
case object Target extends PointType
case object Intersect extends PointType