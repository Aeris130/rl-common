package net.cyndeline.rlcommon.math.geom

/**
  * Computes the average position of all points in a shape.
  */
object Centroid {

  def fromPoints(points: Seq[Point]): Point = centroidFromPoints(points.map(_.toDouble), true).toInt
  def fromPoints(p: Point, ps: Point*): Point = fromPoints(Seq(p) ++ ps)
  def fromPoints(points: Seq[DPoint]): DPoint = centroidFromPoints(points, false)
  def fromPoints(p: DPoint, ps: DPoint*): DPoint = fromPoints(Seq(p) ++ ps)

  private def centroidFromPoints(ps: Seq[DPoint], round: Boolean): DPoint = {
    require(ps.nonEmpty, "Cannot compute centroid from an empty set of points")
    require(ps.size > 1, "Cannot compute centroid from a single point.")
    val k = ps.size
    val sumX = ps.map(_.x).sum
    val sumY = ps.map(_.y).sum
    val roundedX = if (round) Math.round(sumX / k) else sumX / k
    val roundedY = if (round) Math.round(sumY / k) else sumY / k
    DPoint(roundedX, roundedY)
  }
}
