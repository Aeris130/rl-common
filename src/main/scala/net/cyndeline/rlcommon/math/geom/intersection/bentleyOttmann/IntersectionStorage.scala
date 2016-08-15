package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.math.geom._
import net.cyndeline.rlcommon.util.UnorderedPair

/**
  * Stores all currently found intersection points and intervals, and updates them as needed.
  *
  * @param pointIntersections An intersection point tupled with the upper, lower segment intersecting it.
  * @param addedOverlaps Keeps (a, b) and (b, a) from both being added.
  */
class IntersectionStorage[L <: Line](val pointIntersections: Vector[(DPoint, Segment[L], Segment[L])],
                                     overlaps: Map[UnorderedPair[Point], Vector[Segment[L]]],
                                     addedOverlaps: Set[UnorderedPair[Segment[L]]]) {

  def this() = this(Vector[(DPoint, Segment[L], Segment[L])](), Map[UnorderedPair[Point], Vector[Segment[L]]](), Set[UnorderedPair[Segment[L]]]())

  /**
    * @param previousIntersection The last intersection point processed in the event queue.
    * @param upper The upper segment in a (possible) intersection.
    * @param lower The lower segment in a (possible) intersection.
    * @param addSinglePointIntersection True if single-point intervals should have their segments be registered at
    *                                   single-point intersections, otherwise false.
    * @return A storage object with any possible intersections added.
    */
  def add(previousIntersection: DPoint, upper: Segment[L], lower: Segment[L], addSinglePointIntersection: Boolean = true): IntersectionStorage[L] = {
    val intersection = upper.intersection(lower)

    if (intersection.isDefined) {

      /* Only register intersections beyond the last intersection point. */
      if (intersection.get.isSinglePoint && aboveCurrent(previousIntersection, intersection.get.pointIntersection)) {
        new IntersectionStorage(addToPoints(intersection.get.pointIntersection, upper, lower), overlaps, addedOverlaps)

      } else if (intersection.get.isInterval) {
        val coordinates = intersection.get.overlap
        val updated = addToOverlaps(coordinates._1, coordinates._2, upper, lower)

        /* Special case: If the interval was triggered using a single-point segment, the point should
         * be registered as such.
         */
        val points = if (addSinglePointIntersection && coordinates._1 == coordinates._2 && aboveCurrent(previousIntersection, coordinates._1)) {
          addToPoints(coordinates._1.toInt, upper, lower)
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
  def addIntervalAsPoint(point: DPoint, upper: Segment[L], lower: Segment[L]): IntersectionStorage[L] = {
    assert(upper.intersects(lower), s"Attempted to register an intersection at $point between $lower and $upper, but no intersection found.")
    new IntersectionStorage(addToPoints(point, upper, lower), overlaps, addedOverlaps)
  }

  /**
    * @return Every overlap interval found by the algorithm so far.
    */
  def allOverlaps: Vector[(LineIntersection, Vector[Segment[L]])] = {

    /* Distinct needs to be called since every pair of overlapping segments is added, as a single segment may be
     * processed more than once.
     */
    overlaps.toVector.map(kv => (LineIntersection(kv._1._1, kv._1._2), kv._2.distinct))
  }

  /**
    * @return Intersection storage with no point intersections in it.
    */
  def clearPointIntersections: IntersectionStorage[L] = {
    new IntersectionStorage(Vector(), overlaps, addedOverlaps)
  }

  private def addToPoints(p: DPoint, upper: Segment[L], lower: Segment[L]) = {
    (p, upper, lower) +: pointIntersections
  }

  private def addToOverlaps(from: Point, to: Point, upper: Segment[L], lower: Segment[L]): (Map[UnorderedPair[Point], Vector[Segment[L]]], Set[UnorderedPair[Segment[L]]]) = {
    val pair = UnorderedPair(upper, lower)
    if (!addedOverlaps.contains(pair)) {
      val points = UnorderedPair(from, to)
      val current = overlaps.getOrElse(points, Vector())
      val newOverlaps = overlaps + (points -> (upper +: (lower +: current)))
      (newOverlaps, addedOverlaps + pair)

    } else {
      (overlaps, addedOverlaps)
    }
  }

  private def aboveCurrent(current: DPoint, p: DPoint): Boolean = current == null || EventPoint.coordinateLessThan(current, p)

}
