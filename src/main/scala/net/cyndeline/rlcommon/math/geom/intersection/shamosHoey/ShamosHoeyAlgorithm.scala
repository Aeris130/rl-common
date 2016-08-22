package net.cyndeline.rlcommon.math.geom.intersection.shamosHoey

import net.cyndeline.rlcommon.math.geom.Line
import net.cyndeline.rlcommon.math.geom.intersection.common._

/**
  * Sweep line algorithm by Shamos and Hoey that checks if an intersection exists. As all intersections are not
  * reported, the algorithm terminates in O(n log n) time and O(n) space.
  */
class ShamosHoeyAlgorithm {

  def hasIntersection[L <: Line](l1: L, ls: L*): Boolean = hasIntersection(l1 +: ls.toVector)

  /**
    * @param lines A sequence of line segments.
    * @tparam L Line type.
    * @return True if at least one intersection or overlap between the segments exists, otherwise false.
    */
  def hasIntersection[L <: Line](lines: Vector[L]): Boolean = if (lines.isEmpty) {
    false
  } else {
    var queue = EventQueue.fromSegments(lines)
    val lowestStart = lines.flatMap(s => List(s.start, s.stop)).minBy(_.x)
    var sweepLine = SweepLine[L](lowestStart, lines.size).insert(lowestStart)

    def poll: EventPoint[L] = {
      val next = queue.poll
      queue = next._2
      next._1
    }

    while (queue.nonEmpty) {
      poll match {
        case SegmentPoint(p, Source, segment) =>
          sweepLine = sweepLine.insert(p, segment)
          val above = sweepLine.above(segment)
          val below = sweepLine.below(segment)

          if ((above.isDefined && above.get.intersects(segment)) || (below.isDefined && below.get.intersects(segment)))
            return true

        case SegmentPoint(p, Target, segment) =>
          val above = sweepLine.above(segment)
          val below = sweepLine.below(segment)
          sweepLine = sweepLine.remove(segment)

          if (above.isDefined && below.isDefined && above.get.intersects(below.get))
            return true
      }
    }

    false
  }

}
