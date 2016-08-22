package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import java.util.Comparator

import net.cyndeline.rlcommon.math.geom._
import net.cyndeline.rlcommon.math.geom.intersection.common._

import scala.collection.mutable

/**
  * Computes all intersections for a set of line segments in O((n + k) * log(n)) time, where k is the number of
  * intersections. Handles degenerate cases using the implementation described in "A Generic Plane-Sweep for Intersecting
  * Line Segments" by W.Freiseisen, P.Pau.
  *
  * @param inclSingleCoordinates If set to true, line segments covering a single coordinate (start == stop)
  *                              will register as both an interval overlap between start and stop, as well
  *                              as a regular single-point intersection at the only coordinate covered.
  *                              Otherwise single-point segments will not be a part of non-collinear
  *                              intersections at all.
  */
class BentleyOttmannIntersection private (inclSingleCoordinates: Boolean) {

  def computeIntersections[L <: Line](s: L, ss: L*): Vector[(LineIntersection, Vector[L])] = computeIntersections(s +: ss.toVector)

  /**
    * Computes all intersections for a list of line segments.
    *
    * @param segments Every segment to compute intersections for. If two ore more segments in this collection are
    *                 equal, they will still be considered unique and reported as intersecting (if nothing else,
    *                 every set of equal segments are overlapping each other).
    * @tparam L Segment type.
    * @return A list of intersections tupled with the list of segments present in the intersection.
    */
  def computeIntersections[L <: Line](segments: Vector[L]): Vector[(LineIntersection, Vector[L])] = {
    if (segments.isEmpty)
      return Vector()

    var queue: EventQueue[L] = EventQueue.fromSegments(segments)
    val lowestStart = segments.flatMap(s => List(s.start, s.stop)).minBy(_.x)
    var sweepLine = SweepLine[L](lowestStart, segments.size).insert(lowestStart)

    var intersectionStorage = new IntersectionStorage[L]()

    // Final intersections to report when the algorithm terminates, not inserted in the event queue.
    val finalIntersections = new mutable.HashMap[RPoint, Vector[Segment[L]]]

    def addToFinalIntersections(p: RPoint, s: Vector[Segment[L]]): Unit = {
      assert(!finalIntersections.contains(p), s"Attempted to insert segments to an intersection point $p multiple times.")
      finalIntersections += p -> s

      if (s.exists(_.isSingleCoordinate))
        intersectionStorage = intersectionStorage.addOverlaps(p, p, s.toSet)
    }

    /* Source case algorithm */
    def sourceCase(event: SegmentPoint[L], previousIntersection: RPoint): Unit = {
      val segment = event.segment
      sweepLine = sweepLine.insert(segment.source, segment)
      val s1 = sweepLine.above(segment)
      val s2 = sweepLine.below(segment)

      if (s1.isDefined) {
        if (s1.get.containsPoint(event.p) && segment.collinearWith(s1.get)) {
          // Collinearity case
          collinearityCase(segment, s2, previousIntersection)

          // Special case for single-point segments
          if (segment.isSingleCoordinate)
            intersectionStorage = intersectionStorage.add(previousIntersection, s1.get, segment)

        } else {
          // S1 is a common neighbor
          intersectionStorage = intersectionStorage.add(previousIntersection, s1.get, segment)

          if (s2.isDefined) {
            intersectionStorage = intersectionStorage.add(previousIntersection, segment, s2.get)
          }
        }
      } else if (s2.isDefined) {
        intersectionStorage = intersectionStorage.add(previousIntersection, segment, s2.get)
      }

      /* A degeneracy needs to be handled here in case s1 or s2 are collinear and overlapping. This is not specified
       * in the algorithm description, but rather in the description of the sweep line data structure. If a collinear
       * overlapping neighbor is found, the closest non-overlapping neighbor in that direction must also be reported
       * in order to prevent some segments from not being registered at intersection points due to an overlapping
       * neighbor blocking them off on the sweep line.
       */
      var nonOverlapAbove: Option[Segment[L]] = None
      var nonOverlapBelow: Option[Segment[L]] = None
      def handleCase(collinearNeighbor: Option[Segment[L]], upper: Boolean): Unit = if (collinearNeighbor.isDefined) {
        val intersection = collinearNeighbor.get.intersection(segment)
        if (intersection.isDefined && intersection.get.isInterval) {
          val nonOverlapping = if (upper) sweepLine.closestNonOverlappingAbove(segment)
                               else sweepLine.closestNonOverlappingBelow(segment)
          if (nonOverlapping.isDefined)
            if (upper) {
              nonOverlapAbove = nonOverlapping
              intersectionStorage = intersectionStorage.add(previousIntersection, nonOverlapping.get, segment)
            } else {
              nonOverlapBelow = nonOverlapping
              intersectionStorage = intersectionStorage.add(previousIntersection, segment, nonOverlapping.get)
            }


        }
      }

      handleCase(s1, true)
      handleCase(s2, false)

      /* A second degeneracy includes the case where a segment s intersects multiple non-collinear segments that are
       * collinear with each other. If these segments are processed before s, no intersection point will be added
       * until the source event of s is found. Once that happens, s will only neighbor one of them on the sweep line,
       * causing the others to not be included in the intersection. To prevent this, every collinear neighbor of s1
       * and s2 is also added to the intersection of s and s1/s2.
       */
      def addCollinearNeighbors(neighbor: Option[Segment[L]], upper: Boolean): Unit = if (neighbor.isDefined) {
        val intersection = segment.intersection(neighbor.get)
        if (intersection.isDefined && intersection.get.isSinglePoint) {
          val neighbors = sweepLine.allCollinearSegments(neighbor.get)
          for (n <- neighbors if n != segment && n.intersects(segment))
            if (upper)
              intersectionStorage = intersectionStorage.addIntervalAsPoint(intersection.get.pointIntersection, n, segment)
            else
              intersectionStorage = intersectionStorage.addIntervalAsPoint(intersection.get.pointIntersection, segment, n)
        }
      }

      addCollinearNeighbors(s1, true)
      addCollinearNeighbors(s2, false)

      /* If the closest neighbor is collinear, but a non-overlapping neighbor was found previously, that neighbor
       * must be checked as well.
       */
      if (nonOverlapAbove.isDefined) addCollinearNeighbors(nonOverlapAbove, true)
      if (nonOverlapBelow.isDefined) addCollinearNeighbors(nonOverlapBelow, false)

    }

    /* Subset of the Source case, used when one or more segments run collinear to s. Handles both cases where the
     * collinear segments have the same source as s, and when they begin before s.
     */
    def collinearityCase(s: Segment[L], s2: Option[Segment[L]], previousIntersection: RPoint): Unit = {
      val lowestNonCollinearAbove = sweepLine.closestNonCollinearAbove(s)

      if (lowestNonCollinearAbove.isDefined)
        intersectionStorage = intersectionStorage.add(previousIntersection, lowestNonCollinearAbove.get, s)

      if (s2.isDefined)
        intersectionStorage = intersectionStorage.add(previousIntersection, s, s2.get)
    }

    /* Target case algorithm */
    def targetCase(event: SegmentPoint[L], previousIntersection: RPoint): Unit = {
      val segment = event.segment
      val targetP = segment.target
      val aboveNeighbor = sweepLine.above(segment)
      val belowNeighbor = sweepLine.below(segment)

      if (aboveNeighbor.isDefined && belowNeighbor.isDefined) {
        val s1 = aboveNeighbor.get
        val s2 = belowNeighbor.get

        if (!segment.collinearWith(s1) || !segment.collinearWith(s2)) {
          val s1s2Intersection = s1.intersection(s2)
          if (s1s2Intersection.isDefined) {
            val i = s1s2Intersection.get

            if (i.isSinglePoint) {
              val p = i.pointIntersection
              if (EventPoint.coordinateLessThan(targetP, p)) {
                intersectionStorage = intersectionStorage.add(previousIntersection, s1, s2, false)

                addCollinearPoints(s1, s2)
                addCollinearPoints(s2, s1)

              }
            } else {
              // Overlaps doesn't need to check if they're beyond p, since no intersection event is added
              intersectionStorage = intersectionStorage.add(previousIntersection, s1, s2, false)
            }

          }

        }

        /* Note: The algorithm description puts this as an else-clause, but this results in overlaps not being found
         * if the target segment has both a non-collinear neighbor s1 or s2, as well as collinear overlapping
         * segments to add.
         */
        addAllCollinearOverlaps(segment)

      } else if (aboveNeighbor.isDefined || belowNeighbor.isDefined) {

        /*
         * This part is not included in the original algorithm description, but is needed to detect overlap between
         * multiple collinear segments ending in the same order they are inserted. These segments will never have
         * both s1 and s2 defined, as the previous segment is always removed before the target point of the next segment
         * occurs, causing that segment to now be the topmost segment.
         */

        val neighbor = if (aboveNeighbor.isDefined) aboveNeighbor.get else belowNeighbor.get
        if (segment.collinearWith(neighbor)) {
          addAllCollinearOverlaps(segment)
        }
      }

      def addAllCollinearOverlaps(s: Segment[L]): Unit = {
        for (cSegment <- sweepLine.allCollinearSegments(segment)) {
          val x = event.p.x
          val intersection = cSegment.intersection(segment)

          if (intersection.get.isInterval) {
            val below = cSegment.below(x, segment)
            val upper = if (below) segment else cSegment
            val lower = if (below) cSegment else segment
            intersectionStorage = intersectionStorage.add(previousIntersection, upper, lower, false)

          }
        }
      }

      // Adds all point intersections between s's collinear neighbors and some neighbor n.
      def addCollinearPoints(s: Segment[L], n: Segment[L]): Unit = {
        for (cSegment <- sweepLine.allCollinearSegments(s)) {
          val intersection = cSegment.intersection(n)

          if (intersection.isDefined && intersection.get.isSinglePoint) {
            val upper = if (sweepLine.orderedBelow(cSegment, n)) n else cSegment
            val below = if (upper == n) cSegment else n
            intersectionStorage = intersectionStorage.add(previousIntersection, upper, below, false)
          }
        }
      }

      /* This part isn't specified in the algorithm description, but rather the elimination-description of the
       * sweep line. Whenever a target event is reached, the segment must be removed from the sweep line.
       */
      sweepLine = sweepLine.remove(segment)
    }

    /* Intersection case algorithm */
    def intersectionCase(event: Intersection[L], previousIntersection: RPoint): Unit = {
      val u = event.upper
      val v = event.lower

      // Look for the closest non-intersecting (i.e not part of the pencil) neighbors above u and below v.
      val pencilSet = event.allSegments.toSet
      val topNeighbor = sweepLine.above(u, (s: Segment[L]) => !pencilSet.contains(s))
      val bottomNeighbor = sweepLine.below(v, (s: Segment[L]) => !pencilSet.contains(s))
      val pencil = event.allSegments

      addToFinalIntersections(event.coordinate, pencil)
      sweepLine = sweepLine.swap(event.coordinate, pencil:_*)

      /* On top of adding any eventual intersections between the upper/lower neighbor and the new (due to the swap)
       * upper/lower segments in the pencil around p, any segment collinear with these segments also has to have their
       * intersections updated.
       */
      def collinearInPencil(s: Segment[L]): Vector[Segment[L]] = pencil.filter(p => p != s && s.collinearWith(p))

      def collinearWithNeighbor(s: Segment[L], top: Boolean) = {
        val n = if (top) topNeighbor else bottomNeighbor

        if (n.isEmpty) {
          Vector()
        } else {
          sweepLine.allCollinearSegments(n.get).filter(_.intersects(s))
        }
      }

      if (bottomNeighbor.isDefined) {
        intersectionStorage = intersectionStorage.add(event.coordinate, u, bottomNeighbor.get)

        for (cs <- collinearInPencil(u))
          intersectionStorage = intersectionStorage.add(event.coordinate, cs, bottomNeighbor.get)

        for (cs <- collinearWithNeighbor(u, false))
          if (sweepLine.orderedBelow(cs, u))
            intersectionStorage = intersectionStorage.add(event.coordinate, u, cs)
          else
            intersectionStorage = intersectionStorage.add(event.coordinate, cs, u)
      }

      if (topNeighbor.isDefined) {
        intersectionStorage = intersectionStorage.add(event.coordinate, topNeighbor.get, v)

        for (cs <- collinearInPencil(v))
          intersectionStorage = intersectionStorage.add(event.coordinate, topNeighbor.get, cs)

        for (cs <- collinearWithNeighbor(v, true))
          if (sweepLine.orderedBelow(cs, v))
            intersectionStorage = intersectionStorage.add(event.coordinate, v, cs)
          else
            intersectionStorage = intersectionStorage.add(event.coordinate, cs, v)
      }

    }

    /*
     * Main algorithm
     */
    var previousIntersection: RPoint = null
    while (queue.nonEmpty) {
      val next: (EventPoint[L], EventQueue[L]) = queue.poll
      val event: EventPoint[L] = next._1
      queue = next._2

      event match {
        case source @ SegmentPoint(_, Source, _) => sourceCase(source, previousIntersection)
        case target @ SegmentPoint(_, Target, _) => targetCase(target, previousIntersection)
        case intersection @ Intersection(p, upper, _, _) =>

          /* Each intersection can only be registered once. Allowing points to be processed multiple times may result
           * in the equivalence relation between segments to be broken, resulting in faulty tree structures (i.e
           * a < b < c && a > c) due to not every segment being swapped at the point at the same time
           */
          assert(previousIntersection == null || EventPoint.coordinateLessThan(previousIntersection, p), s"The sweep line has already moved past $p, but the point has been inserted as an intersection event again.")

          /* At least one segment that isn't collinear with the rest must exist, or this is just an overlap induced
           * by a single-point segment. It's also possible that the segment is collinear but only shares a single
           * point, so check if that is the case before discarding a collinear segment.
           */
          val nonSingle = intersection.allSegments.find(!_.isSingleCoordinate)
          if (nonSingle.isDefined && intersection.allSegments.exists(s => !s.collinearWith(nonSingle.get) || s.intersection(nonSingle.get).get.isSinglePoint)) {
            intersectionCase(intersection, previousIntersection)
            previousIntersection = p
          }
      }

      /* Update the queue with new intersections. */
      for (i <- intersectionStorage.pointIntersections) {
        val p = i._1
        val segAbove = i._2
        val segBelow = i._3

        if (!queue.intersectionExists(p)) {
          queue = queue.insert(Intersection[L](p, segAbove, segBelow, Set()))
        } else {
          val updated = queue.intersectionFor(p).update(segAbove).update(segBelow)
          queue = queue.update(p, updated)
        }
      }

      intersectionStorage = intersectionStorage.clearPointIntersections
    }

    val pointIntersections = finalIntersections.map(kv => (LineIntersection(kv._1), {
      if (inclSingleCoordinates)
        kv._2.distinct
      else
        kv._2.distinct.filterNot(_.isSingleCoordinate)
    })).toVector.filterNot(kv => kv._2.isEmpty)

    val overlapIntervals = intersectionStorage.allOverlaps.map(kv => kv._1 -> kv._2.toVector)

    /* Map each segment back to its original user-supplied line, in case the user extends the line class with
     * unique identifiers.
     */
    (pointIntersections ++ overlapIntervals).map(kv => (kv._1, kv._2.sortBy(_.id).map(_.original)))
  }

}

object BentleyOttmannIntersection {

  /**
    * @return A Bentley Ottmann algorithm that doesn't include single-point line segments into intersections, only
    *         collinear overlaps.
    */
  def withoutSinglePointIntersections: BentleyOttmannIntersection = new BentleyOttmannIntersection(false)

  /**
    * @return A Bentley Ottmann algorithm that includes single-point line segments into intersections and
    *         collinear overlaps.
    */
  def withSinglePointIntersections: BentleyOttmannIntersection = new BentleyOttmannIntersection(true)

}
