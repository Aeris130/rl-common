package net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree

import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}

/**
  * Provides a trivial property implementation that makes the KD tree compatible with 2D integer points.
  */
class Point2DProperty extends ElementProperty[Point] with RangeProperty[Point, Rectangle] {
  override val totalDimensions: Int = 2
  override def value(element: Point, dimension: Int): Int = dimension match {
    case 1 => element.x
    case 2 => element.y
    case _ => throw new Error("Unspecified dimension " + dimension + " for point " + element)
  }
  override def isInRange(e: Point, r: Rectangle): Boolean = {
    val from = start(r)
    val to = stop(r)
    e.x >= from.x && e.y >= from.y && e.x <= to.x && e.y <= to.y
  }
  override def hasLeftChildren(e: Point, d: Int, r: Rectangle): Boolean = {
    value(e, d) >= value(start(r), d)
  }
  override def hasRightChildren(e: Point, d: Int, r: Rectangle): Boolean = {
    value(e, d) <= value(stop(r), d)
  }

  private def start(r: Rectangle) = r.start
  private def stop(r: Rectangle) = r.start + (r.width - 1, r.height - 1)
}
