package net.cyndeline.rlcommon.math.geom

import net.cyndeline.rlcommon.util.UnorderedPair

/**
  * Represents a single intersection point or an overlapping interval between two line segments.
  */
class LineIntersection private (p: Option[RPoint], segment: Option[(RPoint, RPoint)]) {
  def isSinglePoint: Boolean = p.isDefined
  def isInterval: Boolean = segment.isDefined

  def pointIntersection: RPoint = {
    assert(isSinglePoint, "Cannot retrieve a single point intersection from an interval.")
    p.get
  }
  def overlap: (RPoint, RPoint) = {
    assert(isInterval, "Cannot retrieve an interval overlap from a single point intersection.")
    segment.get
  }

  override def toString: String = {
    val builder = new StringBuilder()
    builder ++= "Line intersection "
    if (isSinglePoint)
      builder ++= "in a single point: " + p
    else
      builder ++= "across a segment: From " + overlap._1 + " to " + overlap._2

    builder.toString()
  }

  override def equals(other: Any): Boolean = other match {
    case li: LineIntersection => if (isSinglePoint) {
      li.isSinglePoint && li.pointIntersection == pointIntersection
    } else {
      li.isInterval && (li.overlap == overlap || (li.overlap._1 == overlap._2 && li.overlap._2 == overlap._1))
    }
    case _ => false
  }

  override def hashCode: Int = p.## ^ segment.##

}

/** Factory object. */
object LineIntersection {
  def apply(p: RPoint): LineIntersection = new LineIntersection(Some(p), None)
  def apply(start: RPoint, stop: RPoint): LineIntersection = new LineIntersection(None, Some((start, stop)))
}
