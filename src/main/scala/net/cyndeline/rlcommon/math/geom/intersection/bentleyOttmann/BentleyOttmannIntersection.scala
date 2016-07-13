package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import java.util.Comparator

import net.cyndeline.rlcommon.math.geom.{DPoint, Line, LineIntersection, Point}
import net.cyndeline.rlcommon.util.UnorderedPair

import scala.collection.mutable

/**
  * Computes all intersections for a set of line segments in O((n + k) * log(n)) time, where k is the number of
  * intersections. Handles degenerate cases using the implemetation described in "A Generic Plane-Sweep for Intersecting
  * Line Segments" by W.Freiseisen, P.Pau and R.Linz.
  */
/*
    Step 8 of Target Vertex Subalgorithm requires some further development.
    As it is, the portion of the segment between the last two event points is
    reported as common intersection, although for some of the initial segments
    this can be only a part of their intersection

    // Tests
    Check when two vertical segments overlap
      - Have overlapping segments span multiple events, check if multiple intersections are reported.
    Check what happens to segments with the same start/stop coordinate
    Two intersections with the same x coordinate

    // Scripts productions
    - Takes a line in the current set and adds a duplicate

 */
class BentleyOttmannIntersection() {

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

    var queue: EventQueue[L] = initialEvents(segments)
    val lowestStart = segments.flatMap(s => List(s.start, s.stop)).minBy(_.x)
    var sweepLine = SweepLine[L](lowestStart, segments.size).insert(lowestStart.x)

    // Overlapping segments. These are saved separately, as they are not added to the queue in form of an event.
    var finalOverlaps = Vector[(LineIntersection, UnorderedPair[Point])]()
    val addedOverlaps = new mutable.HashSet[UnorderedPair[Segment[L]]] // Keeps overlaps (a,b) and (b,a) from both being added
    val overlapsAtPoints = new mutable.HashMap[UnorderedPair[Point], Vector[Segment[L]]]() // Stores every segment overlapping using the same coordinated

    // Final intersections to report when the algorithm terminates, not inserted in the event queue.
    val finalIntersections = new mutable.HashMap[DPoint, Vector[Segment[L]]]

    // Intersections tupled with upper and lower segments the make up the intersection. These gets added to the event queue.
    var intersections = Vector[(LineIntersection, Segment[L], Segment[L])]()

    /*
     * Helper method that adds intersections (both points and overlapping intervals) while filtering out duplicates.
     */
    def addIntersection(i: Option[LineIntersection], upper: Segment[L], lower: Segment[L]): Unit = {
      if (i.isDefined) {
        if (i.get.isSinglePoint) {
          intersections = (i.get, upper, lower) +: intersections
        } else {
          val pair = UnorderedPair(upper, lower)
          if (!addedOverlaps.contains(pair)) {
            addedOverlaps.add(pair)

            val sourceTarget = UnorderedPair(i.get.overlap)
            val overlapEntryExists = overlapsAtPoints.contains(sourceTarget)
            val segmentVector = overlapsAtPoints.getOrElse(sourceTarget, Vector())
            overlapsAtPoints += (sourceTarget -> (upper +: (lower +: segmentVector)))

            if (!overlapEntryExists) {
              finalOverlaps = (i.get, sourceTarget) +: finalOverlaps
            }
          }
        }
      }
    }

    /* Source case algorithm */
    def sourceCase(event: SegmentPoint[L]): Unit = {
      val segment = event.segment
      sweepLine = sweepLine.insert(segment.source.x, segment)
      val neighbors = sweepLine.aboveAndBelow(segment)
      val s1 = neighbors._1 // Above
      val s2 = neighbors._2 // Below

      if (s1.isDefined) {
        if (s1.get.containsPoint(event.p) && segment.collinearWith(s1.get)) {
          // Collinearity case
          collinearityCase(segment, s2)

        } else {
          // S1 is a common neighbor
          addIntersection(s1.get.intersection(segment), s1.get, segment)

          if (s2.isDefined) {
            addIntersection(s2.get.intersection(segment), segment, s2.get)
          }
        }
      } else if (s2.isDefined) {
        addIntersection(s2.get.intersection(segment), segment, s2.get)
      }
    }

    /* Subset of the Source case, used when one or more segments run collinear to s. Handles both cases where the
     * collinear segments have the same source as s, and when they begin before s.
     */
    def collinearityCase(s: Segment[L], s2: Option[Segment[L]]): Unit = {
      val lowestNonCollinearAbove = sweepLine.closestNonCollinearAbove(s)

      if (lowestNonCollinearAbove.isDefined)
        addIntersection(lowestNonCollinearAbove.get.intersection(s), lowestNonCollinearAbove.get, s)

      if (s2.isDefined)
        addIntersection(s2.get.intersection(s), s2.get, s)
    }

    /* Target case algorithm */
    def targetCase(event: SegmentPoint[L]): Unit = {
      val segment = event.segment
      val targetP = segment.target
      val neighbors = sweepLine.aboveAndBelow(segment)

      if (neighbors._1.isDefined && neighbors._2.isDefined) {
        val s1 = neighbors._1.get
        val s2 = neighbors._2.get

        if (!segment.collinearWith(s1) && !segment.collinearWith(s2)) {
          val s1s2Intersection = s1.intersection(s2)
          if (s1s2Intersection.isDefined) {
            val i = s1s2Intersection.get

            if (i.isSinglePoint) {
              val p = i.pointIntersection.toInt
              if (EventPoint.coordinateLessThan(targetP, p)) {
                addIntersection(s1.intersection(s2), s1, s2)
              }
            } else {
              // Overlaps doesn't need to check if they're beyond p, since no intersection event is added
              addIntersection(s1s2Intersection, s1, s2)
            }

          }

        } else {
          addAllCollinearOverlaps(segment)
        }
      } else if (neighbors._1.isDefined || neighbors._2.isDefined) {

        /*
         * This part is not included in the original algorithm description, but is needed to detect overlap between
         * multiple collinear segments ending in the same order they are inserted. These segments will never have
         * both s1 and s2 defined, as the previous segment is always removed before the target point of the next segment
         * occurs, causing that segment to now be the topmost segment.
         */

        val neighbor = if (neighbors._1.isDefined) neighbors._1.get else neighbors._2.get
        if (segment.collinearWith(neighbor)) {
          addAllCollinearOverlaps(segment)
        }
      }

      def addAllCollinearOverlaps(s: Segment[L]): Unit = {
        for (cSegment <- sweepLine.allCollinearSegments(segment)) {
          val x = event.p.x
          if (cSegment.below(x, segment))
            addIntersection(cSegment.intersection(segment), segment, cSegment)
          else
            addIntersection(cSegment.intersection(segment), cSegment, segment)
        }
      }

      /* This part isn't specified in the algorithm description, but rather the elimination-description of the
       * sweep line. Whenever a target event is reached, the segment must be removed from the sweep line.
       */
      sweepLine = sweepLine.remove(segment)
    }

    /* Intersection case algorithm */
    def intersectionCase(event: Intersection[L]): Unit = {
      val u = event.upper
      val v = event.lower

      // Look for the closest non-intersecting neighbors above u and below v.
      def nonIntersecting(a: Segment[L], b: Segment[L]): Boolean = {
        val i = a.intersection(b)

        /* Algorithm description is a bit iffy on whether to not treat overlaps as intersections, but there doesn't seem
         * to be any other way to make multiple collinear segments detect a common intersecting segment.
         */
        i.isEmpty || i.get.isInterval
      }
      val topNeighbor = sweepLine.above(u, (s: Segment[L]) => nonIntersecting(s, u))
      val bottomNeighbor = sweepLine.below(v, (s: Segment[L]) => nonIntersecting(s, v))
      val pencil = event.allSegments

      if (finalIntersections.contains(event.coordinate)) {
        val currentSegments = finalIntersections(event.coordinate)
        finalIntersections += event.coordinate -> (pencil ++ currentSegments)
      } else {
        finalIntersections += event.coordinate -> pencil
      }
      sweepLine = sweepLine.swap(event.coordinate, pencil:_*)

      if (bottomNeighbor.isDefined)
        addIntersection(u.intersection(bottomNeighbor.get), u, bottomNeighbor.get)

      if (topNeighbor.isDefined)
        addIntersection(v.intersection(topNeighbor.get), topNeighbor.get, v)
    }

    /*
     * Main algorithm
     */
    while (queue.nonEmpty) {
      val next: (EventPoint[L], EventQueue[L]) = queue.poll
      val event: EventPoint[L] = next._1
      queue = next._2

      event match {
        case source @ SegmentPoint(_, Source, _) => sourceCase(source)
        case target @ SegmentPoint(_, Target, _) => targetCase(target)
        case intersection @ Intersection(_, _, _, _) => intersectionCase(intersection)
      }

      /* Update the queue with new intersections. */
      for (i <- intersections) {
        val intersection = i._1
        val segAbove = i._2
        val segBelow = i._3

        if (intersection.isSinglePoint) {
          val p: DPoint = intersection.pointIntersection

          if (!queue.intersectionExists(p)) {
            queue = queue.insert(Intersection[L](p, segAbove, segBelow, Set()))
          } else {
            val updated = queue.intersectionFor(p).update(segAbove).update(segBelow)
            queue = queue.update(p, updated)
          }

        } else {
          throw new Error("Overlapping segment detected as event point in queue.")
        }
      }
      intersections = Vector()
    }

    val pointIntersections = finalIntersections.map(kv => (LineIntersection(kv._1), kv._2.distinct)).toVector

    /* Distinct needs to be called since every pair of overlapping segments is added, and a single segment may be
     * processed more than once.
     */
    val overlapIntervals = finalOverlaps.map(kv => kv._1 -> overlapsAtPoints(kv._2).distinct)

    /* Map each segment back to its original user-supplied line, in case the user extends the line class with
     * unique identifiers.
     */
    (pointIntersections ++ overlapIntervals).map(kv => (kv._1, kv._2.map(_.original)))
  }

  private def initialEvents[L <: Line](seg: Vector[L]): EventQueue[L] = {
    var id = 0
    var queue = new EventQueue[L]()(BentleyOttmannIntersection.eventOrdering[L])
    for (s <- seg) {
      val segment = new Segment(id, s)
      id += 1
      queue = queue.insert(SegmentPoint(segment.source, Source, segment))
      queue = queue.insert(SegmentPoint(segment.target, Target, segment))
    }
    queue
  }



}

object BentleyOttmannIntersection {

  /** This is the ordering that determines an events position in the priority queue (based on the point it represents).
    * A point 'A is greater than 'B if A's x coordinate is lower than B's. This is to make the sweep line move from
    * the leftmost point towards the rightmost.
    */
  private def eventOrdering[L <: Line] = Ordering.comparatorToOrdering[EventPoint[L]](new Comparator[EventPoint[L]] {
    override def compare(o1: EventPoint[L], o2: EventPoint[L]): Int = if (o1 before o2) 1
    else if (o2 before o1) -1
    else 0
  })

}

//private case class YEntry[L <: Line](yCoordinate: Int, lines: Set[Segment[L]])
