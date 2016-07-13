package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import java.util.Comparator

import net.cyndeline.rlcommon.collections.RBTree
import SweepLine._
import net.cyndeline.rlcommon.math.geom.{DPoint, Line, LineIntersection, Point}

/**
  * A vertical line that moves from left to right and sorts its intersecting segments according to the
  * y-value at which they intersect the line.
  *
  * @param currentX The x-value the line currently is at.
  * @param data BST storing the lines.
  * @param entries Store each line entry using the id of the original segment. Used for segment-to-entry lookup.
  */
class SweepLine[L <: Line] private (currentX: Double, data: RBTree[LineEntry[L]], entries: Vector[LineEntry[L]], ord: Ordering[LineEntry[L]]) {

  /**
    * @param x The x-value that the segment should use when comparing to other segments.
    * @param segments Segments to insert.
    * @return A copy of the sweep line with the segment inserted.
    */
  def insert(x: Double, segments: Segment[L]*): SweepLine[L] = {
    var current = this
    for (s <- segments)
      current = current.insertSegment(x, s)

    current
  }

  /**
    * @param segment Segment to remove.
    * @return A copy of this sweep line with the segment removed.
    */
  def remove(segment: Segment[L]): SweepLine[L] = {
    requireContains(segment)
    new SweepLine(currentX, data.delete(entries(segment.id))(ord), entries.updated(segment.id, null), ord)
  }

  /**
    * @param p An intersection of two or more segments.
    * @param pencil A number of segments intersecting at p.
    * @return A copy of this sweep line with the segments having swapped place.
    */
  def swap(p: DPoint, pencil: Segment[L]*): SweepLine[L] = {
    var line = this
    for (seg <- pencil)
      line = line.remove(seg)

    /* Simply inserting the segments anew with the intersection point is enough to swap them, as the actual swapping
     * is a result of how the segments and points computes before/below using the new x-coordinate.
     */
    for (seg <- pencil)
      line = line.insert(p.x, seg)

    line
  }

  /**
    * @param segment Segment to check existence for in the sweep line.
    * @return True if the sweep line crosses the segment, otherwise false.
    */
  def contains(segment: Segment[L]): Boolean = {
    val entry = entries(segment.id)
    assert(entry == null || data.contains(entry)(ord), {
      if (entry != null) "An entry for segment " + segment + " was found, but no corresponding data in the tree structure exists."
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
                    belowValid: Segment[L] => Boolean = defaultValidation _,
                    aboveValid: Segment[L] => Boolean = defaultValidation _): (Option[Segment[L]], Option[Segment[L]]) = {
    requireContains(segment)
    new NeighbourLookup(data, entries, ord).aboveAndBelow(segment, belowValid, aboveValid)
  }

  /**
    * @param segment A segment intersecting the sweep line.
    * @param isValid If the closest neighbor above the segment isn't valid, the second closest will be processed etc. until
    *                a valid segment is found.
    * @return The neighbor above the specified segment, or None if this segment is the topmost entry on the sweep line.
    */
  def above(segment: Segment[L], isValid: Segment[L] => Boolean = defaultValidation): Option[Segment[L]] =
    new NeighbourLookup(data, entries, ord).aboveAndBelow(segment, defaultValidation, isValid)._1

  /**
    * @param segment A segment intersecting the sweep line.
    * @param isValid If the closest neighbor below the segment isn't valid, the second closest will be processed etc. until
    *                a valid segment is found.
    * @return The neighbor below the specified segment, or None if this segment is the bottommost entry on the sweep
    *         line.
    */
  def below(segment: Segment[L], isValid: Segment[L] => Boolean = defaultValidation): Option[Segment[L]] =
    new NeighbourLookup(data, entries, ord).aboveAndBelow(segment, isValid, defaultValidation)._2

  /**
    * @param segment A segment s on the sweep line.
    * @return The closest segment s' that lies above s on the sweep line is non-collinear with s. None if no such
    *         segment exists.
    */
  def closestNonCollinearAbove(segment: Segment[L]): Option[Segment[L]] =
    new NeighbourLookup(data, entries, ord).closestNonCollinearAbove(segment)

  /**
    * Bounded from above at O(n) running time, where n is the size of the output (the amount of collinear segments).
    *
    * @param s A segment in the sweep line.
    * @return Every segment on the line collinear with s.
    */
  def allCollinearSegments(s: Segment[L]): Vector[Segment[L]] = new NeighbourLookup(data, entries, ord).findAllCollinearSegments(s)

  /**
    * @param x The x-value that the segment should use when comparing to other segments.
    * @param segment Segment to insert.
    * @return A copy of the sweep line with the segment inserted.
    */
  private def insertSegment(x: Double, segment: Segment[L]): SweepLine[L] = {
    require(!contains(segment), "Cannot insert a segment on the sweep line that already exists on it")
    require(x >= currentX, "Cannot insert segment behind the sweep line")
    require(x >= segment.source.x && x <= segment.target.x, "The x-coordinate " + x + " is not present on segment " + segment)
    val newEntry = LineEntry(x, segment)
    new SweepLine(x, data.insert(newEntry)(ord), entries.updated(segment.id, newEntry), ord)
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
    def rInOrder(tree: RBTree[LineEntry[L]]): Unit = if (!tree.isEmpty) {
      rInOrder(tree.right)
      builder.append(tree.value.segment.toString + nl)
      rInOrder(tree.left)
    }
    rInOrder(data)
    builder.append("]")
    if (!data.isEmpty)
      builder.toString()
    else
      "Empty sweep line"
  }

}

object SweepLine {

  /**
    * @param x The last x coordinate where the sweep line intersected this segment. This value is needed since a
    *          segment may be above or below another segment depending on where on the x-axis the line intersects them
    *          both.
    * @param segment Segment on the sweep-line.
    */
  case class LineEntry[L <: Line](x: Double, segment: Segment[L])

  def buildOrdering[L <: Line] = {
    Ordering.comparatorToOrdering(new Comparator[LineEntry[L]]() {
      override def compare(o1: LineEntry[L], o2: LineEntry[L]): Int = {
        val x = Math.max(o1.x, o2.x)
        if (o1.segment.below(x, o2.segment)) -1
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
      }
    })
  }

  def apply[L <: Line](start: Point, segmentAmount: Int): SweepLine[L] = {
    val entries = Vector.fill[LineEntry[L]](segmentAmount)(null)
    val ordering = buildOrdering[L]
    new SweepLine(start.x, RBTree.empty[LineEntry[L]], entries, ordering)
  }
}
