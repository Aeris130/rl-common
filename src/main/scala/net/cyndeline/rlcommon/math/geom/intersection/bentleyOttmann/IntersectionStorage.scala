package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.math.geom._
import net.cyndeline.rlcommon.math.geom.intersection.common.{EventPoint, Segment}
import net.cyndeline.rlcommon.util.UnorderedPair

/**
  * Stores all currently found intersection points and intervals, and updates them as needed.
  *
  * @param pointIntersections An intersection point tupled with the upper, lower segment intersecting it.
  * @param addedOverlaps Keeps (a, b) and (b, a) from both being added.
  */
class IntersectionStorage[L <: Line](val pointIntersections: Vector[(RPoint, Segment[L], Segment[L])],
                                     overlaps: Map[UnorderedPair[RPoint], Set[Segment[L]]],
                                     addedOverlaps: Set[UnorderedPair[Segment[L]]]) {

  def this() = this(Vector[(RPoint, Segment[L], Segment[L])](), Map[UnorderedPair[RPoint], Set[Segment[L]]](), Set[UnorderedPair[Segment[L]]]())

  /**
    * @param previousIntersection The last intersection point processed in the event queue.
    * @param upper The upper segment in a (possible) intersection.
    * @param lower The lower segment in a (possible) intersection.
    * @param addSinglePointIntersection True if single-point intervals should have their segments be registered at
    *                                   single-point intersections, otherwise false.
    * @return A storage object with any possible intersections added.
    */
  def add(previousIntersection: RPoint, upper: Segment[L], lower: Segment[L], addSinglePointIntersection: Boolean = true): IntersectionStorage[L] = {
    val intersection = upper.intersection(lower)

    if (intersection.isDefined) {

      /* Only register intersections beyond the last intersection point. */
      if (intersection.get.isSinglePoint && aboveCurrent(previousIntersection, intersection.get.pointIntersection)) {
        val p = intersection.get.pointIntersection
        new IntersectionStorage(addToPoints(p, upper, lower), overlaps, addedOverlaps)

      } else if (intersection.get.isInterval) {
        val coordinates = intersection.get.overlap
        val updated = addToOverlaps(coordinates._1, coordinates._2, upper, lower, overlaps, addedOverlaps)

        /* Special case: If the interval was triggered using a single-point segment, the point should
         * be registered as such.
         */
        val addSingle = addSinglePointIntersection && coordinates._1 == coordinates._2 && aboveCurrent(previousIntersection, coordinates._1)
        val points = if (addSingle) {
          addToPoints(coordinates._1, upper, lower)
        } else {
          pointIntersections
        }

        new IntersectionStorage(points, updated._1, updated._2)
      } else {
        this
      }
    } else {
      this
    }
  }

  /**
    * Adds two overlapping segments to a specified point intersection.
    * @param point Point to intersect at.
    * @param upper The upper segment in a (possible) intersection.
    * @param lower The lower segment in a (possible) intersection.
    * @return A storage object with the intersection added.
    */
  def addIntervalAsPoint(point: RPoint, upper: Segment[L], lower: Segment[L]): IntersectionStorage[L] = {
    assert(upper.intersects(lower), s"Attempted to register an intersection at $point between $lower and $upper, but no intersection found.")
    new IntersectionStorage(addToPoints(point, upper, lower), overlaps, addedOverlaps)
  }

  /**
    * @param segments 2 or more distinct segments.
    * @return An updated intersection storage with all segments added to the same overlap.
    */
  def addOverlaps(from: RPoint, to: RPoint, segments: Set[Segment[L]]): IntersectionStorage[L] = {
    require(segments.drop(1).nonEmpty, "Attempted to add a single segment to an overlap.")
    val first = segments.head
    var added = addedOverlaps
    var overl = overlaps
    for (s <- segments.drop(1)) {
      val updated = addToOverlaps(from, to, first, s, overl, added)
      added = updated._2
      overl = updated._1
    }
    new IntersectionStorage(pointIntersections, overl, added)
  }

  /**
    * @return Every overlap interval found by the algorithm so far.
    */
  def allOverlaps: Vector[(LineIntersection, Set[Segment[L]])] = {

    /* Distinct needs to be called since every pair of overlapping segments is added, as a single segment may be
     * processed more than once.
     */
    overlaps.toVector.map(kv => (LineIntersection(kv._1._1, kv._1._2), kv._2))
  }

  /**
    * @return Intersection storage with no point intersections in it.
    */
  def clearPointIntersections: IntersectionStorage[L] = {
    new IntersectionStorage(Vector(), overlaps, addedOverlaps)
  }

  private def addToPoints(p: RPoint, upper: Segment[L], lower: Segment[L]) = {
    (p, upper, lower) +: pointIntersections
  }

  private def addToOverlaps(from: RPoint, to: RPoint, upper: Segment[L], lower: Segment[L],
                            overl: Map[UnorderedPair[RPoint], Set[Segment[L]]],
                            added: Set[UnorderedPair[Segment[L]]]): (Map[UnorderedPair[RPoint], Set[Segment[L]]], Set[UnorderedPair[Segment[L]]]) = {
    val pair = UnorderedPair(upper, lower)
    if (!added.contains(pair)) {
      val points = UnorderedPair(from, to)
      val current = overl.getOrElse(points, Set())
      val newOverlaps = overlaps + (points -> (current + upper + lower))
      (newOverlaps, addedOverlaps + pair)

    } else {
      (overlaps, addedOverlaps)
    }
  }

  private def aboveCurrent(current: RPoint, p: RPoint): Boolean = current == null || EventPoint.coordinateLessThan(current, p)

}
