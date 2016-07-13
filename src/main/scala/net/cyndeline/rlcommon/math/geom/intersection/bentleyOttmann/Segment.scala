package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.math.geom.{DPoint, Line, Point}

/**
  * A line segment between two points.
  */
class Segment[L <: Line](val id: Int, val original: L) extends Line(original.start, original.stop) {

  def isVertical: Boolean = target.x == source.x
  def slopeLessThan(other: Segment[L]): Boolean = compareSlope(other) == -1
  def slopeGreaterThan(other: Segment[L]): Boolean = compareSlope(other) == 1
  def toLine: Line = this.asInstanceOf[Line]

  val source = if (original.start.x < original.stop.x ||
                    (original.start.x == original.stop.x && original.start.y < original.stop.y))
                original.start
              else
                original.stop

  val target = if (original.start != source) original.start else original.stop

  /**
    * Defines if this segment lies below another segment at a given point X. This is true if the y-value of this segment
    * at x is less than the other segment, or if both segments has the same y-value and both segments have source points
    * equal to (x, y1) and (x, y2) and the slope of this segment is greater than the others.
    *
    * @param x An x value inside both this segment and the other.
    * @param other Another segment.
    * @return True if this segment is below the other segment at x, otherwise false.
    */
  def below(x: Double, other: Segment[L]): Boolean = {
    require(x >= this.source.x && x <= this.target.x, "Attempted to compare below-status for segment " + this + " using x-value " + x)
    require(x >= other.source.x && x <= other.target.x, "Attempted to compare below-status for segment " + other + " using x-value " + x)

    /* We need to find the y-value on both segments where x is equal to the input, easiest done using the
     * point-slope formula: Given two points p1 and p2 and a slope m, y - y1 == m(x - x1)
     */
    def yGivenX(seg: Segment[L]): Double = (seg.slope * (x - seg.source.x)) + seg.source.y // y == m(x - x1) + y1

    /* If one or more segment is vertical, below status should be manually specified to avoid slopes. */
    if ((this.isVertical && other.isVertical) || (!this.isVertical && other.isVertical))
      return false
    else if (this.isVertical && !other.isVertical)
      return true

    val thisY = yGivenX(this)
    val otherY = yGivenX(other)

    thisY < otherY || (thisY == otherY && {
      this.slope > other.slope && (DPoint(x, thisY) == source.toDouble || DPoint(x, otherY) == other.source.toDouble)
    })
  }

  private def compareSlope(other: Segment[L]): Int = if (isVertical && other.isVertical)
    0
  else if (!isVertical && other.isVertical)
    -1
  else if (isVertical && !other.isVertical)
    1
  else {
    val s = slope
    val os = other.slope

    if (s < os)
      -1
    else if (s == os)
      0
    else
      1
  }

  override def equals(other: Any): Boolean = other match {
    case s: Segment[L] => id == s.id && original == s.original
    case _ => false
  }

  override def hashCode: Int = id.## ^ original.##

}

object Segment {
  def apply[L <: Line](id: Int, original: L): Segment[L] = new Segment[L](id, original)
  def apply(source: (Int, Int), target: (Int, Int), id: Int): Segment[Line] = new Segment(id, Line(source, target))
  def apply(source: Point, target: Point, id: Int): Segment[Line] = new Segment(id, Line(source, target))
}
