package net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree

import net.cyndeline.rlcommon.math.geom.spatialIndex.common.ElementProperty
import net.cyndeline.rlcommon.math.geom.{Point, RPoint, Rectangle}
import spire.math.Rational

/**
  * Provides a trivial property implementation that makes the KD tree compatible with 2D integer points.
  */
class Point2DProperty extends ElementProperty[RPoint] with RangeProperty[RPoint, Rectangle] {
  override val totalDimensions: Int = 2
  override def value(element: RPoint, dimension: Int): Rational = dimension match {
    case 1 => element.x
    case 2 => element.y
    case _ => throw new Error("Unspecified dimension " + dimension + " for point " + element)
  }
  override def distance(a: RPoint, b: RPoint): Rational = a.distanceTo(b)
  override def axisDistance(a: RPoint, b: RPoint, dimension: Int): Rational = (value(a, dimension) - value(b, dimension)).abs
  override def isInRange(e: RPoint, r: Rectangle): Boolean = {
    val from = start(r)
    val to = stop(r)
    e.x >= from.x && e.y >= from.y && e.x <= to.x && e.y <= to.y
  }
  override def hasLeftChildren(e: RPoint, d: Int, r: Rectangle): Boolean = {
    value(e, d) >= value(start(r), d)
  }
  override def hasRightChildren(e: RPoint, d: Int, r: Rectangle): Boolean = {
    value(e, d) <= value(stop(r), d)
  }

  private def start(r: Rectangle) = r.start
  private def stop(r: Rectangle) = r.start + (r.width - 1, r.height - 1)

}
