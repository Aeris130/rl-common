package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import java.util.Comparator

import net.cyndeline.rlcommon.collections.RBTree
import SweepLine._
import net.cyndeline.rlcommon.math.geom.{DPoint, Line, Point}

import scala.collection.mutable.ArrayBuffer


/*
        Swaps are performed by updating the sweep line point and then inserting s1 ... s2.

        Store a bool 'before in every line entry that signifies if the x-value is just before or after the
        event point.

        Every time a linear segment is flipped against a collinear neighbor, those two segments must track how
        many times they've been flipped. If the number is odd, they should reverse their below() relationship,
        otherwise not.

        This means that every time a pencil of n segments is flipped, O(n2) collinearity checks must be made?

        // Alternative algorithm: Store all source and target events and use a sweep line. Keep track of which
        // events are currently active on the line at all times, and check every new segment against all active segments
        // to find new intersections. O(n2) in worst case, but generally behaves better.
 */




/**
  * A vertical line that moves from left to right and sorts its intersecting segments according to the
  * y-value at which they intersect the line.
  *
  * @param currentX The x-value the line currently is at.
  * @param data BST storing the lines.
  * @param entries Store each line entry using the id of the original segment. Used for segment-to-entry lookup.
  */
class SweepLine[L <: Line] private (currentX: Double, data: RBTree[LineEntry[L]], entries: Vector[LineEntry[L]], ord: Ordering[LineEntry[L]]) {
  private val neighborLookup = new NeighbourLookup(data, entries)(ord)

  /**
    * @param p The coordinate that the segment should use when comparing to other segments.
    * @param segments Segments to insert.
    * @return A copy of the sweep line with the segment inserted.
    */
  def insert(p: DPoint, segments: Segment[L]*): SweepLine[L] = {
    var current = this
    for (s <- segments) {
      require(!contains(s), "Cannot insert a segment on the sweep line that already exists on it")
      current = current.insertSegment(p, s, true)
    }

    current
  }

  /**
    * @param segment Segment to remove.
    * @return A copy of this sweep line with the segment removed.
    */
  def remove(segment: Segment[L]): SweepLine[L] = {
    requireContains(segment)
    removeSegment(segment)
  }

  /**
    * @param p An intersection of two or more segments.
    * @param pencil A number of segments intersecting at p.
    * @return A copy of this sweep line with the segments having swapped place.
    */
  def swap(p: DPoint, pencil: Segment[L]*): SweepLine[L] = {
    require(pencil.nonEmpty, "Attempted to swap empty pencil.")
    var line = this
    for (seg <- pencil)
      line = line.removeSegment(seg)

    /* Simply inserting the segments anew with the intersection point is enough to swap them now, as the actual swapping
     * is a result of how the segments and points computes their before/below relation using the new x-coordinate and
     * swap data.
     */
    for (seg <- pencil)
      line = line.insertSegment(p, seg, false)

    line
  }

  /**
    * @param segment Segment to check existence for in the sweep line.
    * @return True if the sweep line crosses the segment, otherwise false.
    */
  def contains(segment: Segment[L]): Boolean = {
    val entry = entries(segment.id)
    assert(entry == null || data.contains(entry)(ord), {
      if (entry != null) s"An entry for segment $segment was found, but no corresponding data in the tree structure exists. Line status: ${this.toString}"
      else "A tree-entry was found in the sweep line for " + segment + ", but the corresponding vector did not contain it."
    })
    entry != null && data.contains(entry)(ord)
  }

  /**
    * @param segment A segment intersecting the sweep line.
    * @return The neighbors (above, below) the specified segment, or None if this segment is the top- or bottommost
    *         entry on the sweep line.
    */
  def aboveAndBelow(segment: Segment[L],
                    belowValid: Segment[L] => Boolean = defaultValidation,
                    aboveValid: Segment[L] => Boolean = defaultValidation): (Option[Segment[L]], Option[Segment[L]]) = {
    requireContains(segment)
    neighborLookup.aboveAndBelow(segment, belowValid, aboveValid)
  }

  /**
    * @param segment A segment intersecting the sweep line.
    * @param isValid If the closest neighbor above the segment isn't valid, the second closest will be processed etc. until
    *                a valid segment is found.
    * @return The neighbor above the specified segment, or None if this segment is the topmost entry on the sweep line.
    */
  def above(segment: Segment[L], isValid: Segment[L] => Boolean = defaultValidation): Option[Segment[L]] =
    neighborLookup.aboveAndBelow(segment, defaultValidation, isValid)._1

  /**
    * @param segment A segment intersecting the sweep line.
    * @param isValid If the closest neighbor below the segment isn't valid, the second closest will be processed etc. until
    *                a valid segment is found.
    * @return The neighbor below the specified segment, or None if this segment is the bottommost entry on the sweep
    *         line.
    */
  def below(segment: Segment[L], isValid: Segment[L] => Boolean = defaultValidation): Option[Segment[L]] =
    neighborLookup.aboveAndBelow(segment, isValid, defaultValidation)._2

  /**
    * @param segment A segment s on the sweep line.
    * @return The closest segment s' that lies above s on the sweep line is non-collinear with s. None if no such
    *         segment exists.
    */
  def closestNonCollinearAbove(segment: Segment[L]): Option[Segment[L]] =
    neighborLookup.closestNonCollinear(segment)

  /**
    * @param segment A segment s on the sweep line.
    * @return The closest neighbor above s on the sweep line that isn't collinear and overlapping with s.
    */
  def closestNonOverlappingAbove(segment: Segment[L]): Option[Segment[L]] = neighborLookup.findClosest(segment, true, (s: Segment[L]) => {
    val i = s.intersection(segment)
    i.isEmpty || i.get.isSinglePoint
  })

  /**
    * @param segment A segment s on the sweep line.
    * @return The closest neighbor below s on the sweep line that isn't collinear and overlapping with s.
    */
  def closestNonOverlappingBelow(segment: Segment[L]): Option[Segment[L]] = neighborLookup.findClosest(segment, false, (s: Segment[L]) => {
    val i = s.intersection(segment)
    i.isEmpty || i.get.isSinglePoint
  })

  /**
    * Bounded from above at O(n) running time, where n is the size of the output (the amount of collinear segments).
    *
    * @param s A segment in the sweep line.
    * @return Every segment on the line collinear with s.
    */
  def allCollinearSegments(s: Segment[L]): Vector[Segment[L]] = new NeighbourLookup(data, entries)(ord).findAllCollinearSegments(s)

  /** @return Every segment on the sweep line, ordered from top to bottom in O(n) time. */
  def values: Vector[Segment[L]] = {
    val result = new ArrayBuffer[Segment[L]]()
    def rInOrder(tree: RBTree[LineEntry[L]]): Unit = if (!tree.isEmpty) {
      rInOrder(tree.right)
      result += tree.value.segment
      rInOrder(tree.left)
    }
    rInOrder(data)
    result.toVector
  }

  private def closestCollinearSegment(s: Segment[L], upper: Boolean): Option[Segment[L]] = {
    implicit val o = ord
    val segments = this.allCollinearSegments(s)
    if (segments.isEmpty)
      None
    else {
      var closest:  Segment[L] = null
      for (c <- segments if closest == null || (!c.overlaps(s) && (if (upper) entries(c.id) < entries(closest.id) else entries(c.id) > entries(closest.id))))
        closest = c

      Some(closest)
    }
  }

  /**
    * @param p The coordinate that the segment should use when comparing to other segments.
    * @param segment Segment to insert.
    * @return A copy of the sweep line with the segment inserted.
    */
  private def insertSegment(p: DPoint, segment: Segment[L], before: Boolean): SweepLine[L] = {
    require(p.x >= currentX, "Cannot insert segment behind the sweep line")
    require(p.x >= segment.source.x && p.x <= segment.target.x, "The x-coordinate " + p.x + " is not present on segment " + segment)
    val newEntry = LineEntry[L](p, segment, before)
    new SweepLine(p.x, data.insert(newEntry)(ord), entries.updated(segment.id, newEntry), ord)
  }

  private def removeSegment(s: Segment[L]): SweepLine[L] = {
    new SweepLine(currentX, data.delete(entries(s.id))(ord), entries.updated(s.id, null), ord)
  }

  private def requireContains(segment: Segment[L]): Unit = {
    require(contains(segment), "The segment " + segment + " was not found on the sweep line.")
  }

  private def defaultValidation(s: Segment[L]) = true

  /** @return Prints all values in order top -> bottom */
  override def toString: String = {
    val builder = new StringBuilder()
    val nl = sys.props("line.separator")
    builder.append("[")
    for (v <- values)
      builder.append(v.toString + nl)

    builder.append("]")
    if (!data.isEmpty)
      builder.toString()
    else
      "Empty sweep line"
  }

}

object SweepLine {

  /**
    * @param p The last coordinate where the sweep line intersected this segment. This value is needed since a
    *          segment may be above or below another segment depending on where on the x-axis the line intersects them
    *          both.
    * @param segment Segment on the sweep-line.
    * @param before True if the sweep line lies on the "left" side of the x-coordinate in this entry, false if on the
    *               right side. Used to determine above-below status for swapped segments.
    */
  case class LineEntry[L <: Line](p: DPoint, segment: Segment[L], before: Boolean = true) {
    def x = p.x
    def y = p.y

    def <(other: LineEntry[L])(implicit ord: Ordering[LineEntry[L]]) : Boolean = ord.lt(this, other)
    def >(other: LineEntry[L])(implicit ord: Ordering[LineEntry[L]]): Boolean = ord.gt(this, other)
    def <=(other: LineEntry[L])(implicit ord: Ordering[LineEntry[L]]) : Boolean = ord.lteq(this, other)
    def >=(other: LineEntry[L])(implicit ord: Ordering[LineEntry[L]]): Boolean = ord.gteq(this, other)
  }

  def buildOrdering[L <: Line] = {
    Ordering.comparatorToOrdering(new Comparator[LineEntry[L]]() {
      override def compare(o1: LineEntry[L], o2: LineEntry[L]): Int = {
        val x = Math.max(o1.x, o2.x)

        val result = if (o1.segment.below(x, o2.segment)) -1
        else if (o2.segment.below(x, o1.segment)) 1
        else {

          /* General case: If below-status isn't defined, events are sorted based on the order in which
           * the sweep-line first cut them.
           */

          if (EventPoint.coordinateLessThan(o1.segment.source, o2.segment.source)) 1
          else if (EventPoint.coordinateLessThan(o2.segment.source, o1.segment.source)) -1

          // Use segment id's as tie breakers for identical coordinate sets
          else if (o1.segment.id < o2.segment.id) -1
          else if (o2.segment.id < o1.segment.id) 1
          else 0
        }

        /* With the below-result computed, the only thing remaining is to determine if the result should be reversed
         * due to a swap at the current coordinate, or not. The two cases for this is a regular intersection, and two
         * collinear segments overlapping.
         */
        def reverse(result: Int) = result * -1
        def y(l: LineEntry[L]): (Double, Double) = {
          if (l.segment.isVertical)
            (l.segment.source.y, l.segment.target.y)
          else {
            val yValue = l.segment.y(x)
            (yValue, yValue)
          }
        }

        val o1Y = y(o1)
        val o2Y = y(o2)

        /* Check if the segments are disjoint at this x coordinate. */
        if (o1Y._1 > o2Y._2 || o1Y._2 < o2Y._1) {
          result

        } else if (o1.segment.collinearWith(o2.segment)) {
          if ((!o1.before && !o2.before) && (o2.segment.containsPoint(o1.p) || o1.segment.containsPoint(o2.p)))
            reverse(result)
          else
            result
        } else {
          if (o1.p == o2.p) {
            if (o1.before && o2.before) result
            else if (!o1.before && !o2.before) reverse(result)
            else result

          } else {

            /* Since y-values are equal and no collinearity detected, this means that the segments intersect at a
             * single point here. One of the segments may be vertical, making it possible that neither entry has
             * its coordinate at the intersection. If so, no swap can have occurred there yet, causing the regular
             * ordering to be used. If one of the segments has its coordinate at the intersection, before/after
             * status is used instead (is a segment has its source here, no swap may have taken place).
             */
            val intersection = o1.segment.intersection(o2.segment)
              .getOrElse(throw new Error(s"Two segments ${o1.segment} and ${o2.segment} share the same y coordinate on the sweep line, but no intersection was found."))
            if (intersection.pointIntersection == o1.p) {
              if (o1.before) result
              else reverse(result)
            } else if (intersection.pointIntersection == o2.p) {
              if (o2.before) result
              else reverse(result)
            } else {
              result
            }
          }
        }
      }
    })
  }

  def apply[L <: Line](start: Point, segmentAmount: Int): SweepLine[L] = {
    val entries = Vector.fill[LineEntry[L]](segmentAmount)(null)
    val ordering = buildOrdering[L]
    new SweepLine(start.x, RBTree.empty[LineEntry[L]], entries, ordering)
  }
}
