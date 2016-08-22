package net.cyndeline.rlcommon.math.geom

import spire.math.Rational
import net.cyndeline.rlcommon.math.SpireRational._

/**
  * Computes the average position of all points in a shape.
  */
object Centroid {

  def fromPoints(points: Seq[RPoint]): RPoint = centroidFromPoints(points)
  def fromPoints(p: RPoint, ps: RPoint*): RPoint = fromPoints(Seq(p) ++ ps)

  private def centroidFromPoints(ps: Seq[RPoint]): RPoint = {
    require(ps.nonEmpty, "Cannot compute centroid from an empty set of points")
    require(ps.size > 1, "Cannot compute centroid from a single point.")
    val k = ps.size
    val sumX: Rational = ps.map(_.x).sum
    val sumY = ps.map(_.y).sum
    RPoint(sumX / k, sumY / k)
  }
}
