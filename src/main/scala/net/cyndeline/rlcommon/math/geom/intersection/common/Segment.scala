package net.cyndeline.rlcommon.math.geom.intersection.common

import net.cyndeline.rlcommon.math.geom.{Line, Point, RPoint}
import spire.math.Rational

/**
  * A line segment between two points.
  */
class Segment[L <: Line](val id: Int, val original: L) extends Line(Segment.computeSource(original), Segment.computeTarget(original, Segment.computeSource(original))) {

  def isVertical: Boolean = target.x == source.x
  def slopeLessThan(other: Segment[L]): Boolean = compareSlope(other) == -1
  def slopeGreaterThan(other: Segment[L]): Boolean = compareSlope(other) == 1
  def toLine: Line = this.asInstanceOf[Line]

  val source = start
  val target = stop

  /**
    * Defines if this segment lies below another segment at a given point X. This is true if the y-value of this segment
    * at x is less than the other segment, or if both segments has the same y-value and both segments have source points
    * equal to (x, y1) and (x, y2) and the slope of this segment is greater than the others.
    *
    * @param x An x value inside both this segment and the other.
    * @param other Another segment.
    * @return True if this segment is below the other segment at x, otherwise false.
    */
  def below(x: Rational, other: Segment[L]): Boolean = {
    require(x >= this.source.x && x <= this.target.x, s"Attempted to compare below-status for segment $this using x-value $x")
    require(x >= other.source.x && x <= other.target.x, s"Attempted to compare below-status for segment $other using x-value $x")

    /* If one or more segment is vertical, below status should be manually specified to avoid slopes. */
    if (this.isVertical && other.isVertical)
      return belowTieBreaker(other)

    else if (!this.isVertical && other.isVertical)
      return this.y(x) < other.start.y

    else if (this.isVertical && !other.isVertical) {
      return !(other.y(x) < this.start.y)
    }

    val thisY = this.y(x)
    val otherY = other.y(x)

    thisY < otherY || (thisY == otherY && {
      val pIsThisSource = RPoint(x, thisY) == source
      val pIsOtherSource = RPoint(x, otherY) == other.source

      if (this.slope > other.slope && (pIsThisSource || pIsOtherSource)) {
        true
      } else if (this.slope < other.slope && (pIsThisSource || pIsOtherSource)) {
        false
      } else {

        /* We need a special case here to handle what happens if neither segments slope if greater than the others,
        * or if (x,y) isn't the source for either segment (i.e we're at an intersection or a collinear segment).
        * This is needed since tree structures otherwise may search the wrong sub-tree if a.below(b) is called on
        * insertion, but b.below(a) is called on deletion or lookup.
        */
        belowTieBreaker(other)

      }
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

  /**
    * This check is not a part of the original algorithm description, but is an implementation-specific measure
    * to ensure consistency in data structure behaviour.
    */
  private def belowTieBreaker(other: Segment[L]): Boolean = {

    /* We start by comparing slopes, causing collinear overlapping segments to group up on the sweep line. */
    if (this.slopeGreaterThan(other))
      true
    else if (other.slopeGreaterThan(this))
      false
    else {

      // collinear and intersecting/overlapping (otherwise this.y != other.y, causing this tiebreaker to not be called)
      if (EventPoint.coordinateLessThan(this.start, other.start))
        true
      else if (EventPoint.coordinateLessThan(other.start, this.start))
        false
      else if (EventPoint.coordinateLessThan(this.stop, other.stop))
        false
      else if (EventPoint.coordinateLessThan(other.stop, this.stop))
        true
      else
        this.id < other.id

    }
  }

  override def toString: String = s"Seg[$id]<${start.asTuple}, ${stop.asTuple} # original: $original>"

  override def equals(other: Any): Boolean = other match {
    case s: Segment[L] => id == s.id && original == s.original
    case _ => false
  }

  override def hashCode: Int = id.## ^ original.##

}

object Segment {
  def apply[L <: Line](id: Int, original: L): Segment[L] = new Segment[L](id, original)
  def apply(id: Int, source: (Int, Int), target: (Int, Int)): Segment[Line] = new Segment(id, Line((Rational(source._1), Rational(source._2)), (Rational(target._1), Rational(target._2))))
  def apply(id: Int, source: Point, target: Point): Segment[Line] = new Segment(id, Line(source, target))

  def computeSource(l: Line): RPoint = if (l.start.x < l.stop.x ||
    (l.start.x == l.stop.x && l.start.y < l.stop.y))
    l.start
  else
    l.stop

  def computeTarget(l: Line, source: RPoint): RPoint = if (l.start != source) l.start else l.stop
}
