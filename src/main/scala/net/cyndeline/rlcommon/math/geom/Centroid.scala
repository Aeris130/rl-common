package net.cyndeline.rlcommon.math.geom

/**
  * Computes the average position of all points in a shape.
  */
object Centroid {

  def fromPoints(points: Seq[Point]): Point = centroidFromPoints(points)
  def fromPoints(p: Point, ps: Point*): Point = fromPoints(Seq(p) ++ ps)

  private def centroidFromPoints(ps: Seq[Point]): Point = {
    require(ps.nonEmpty, "Cannot compute centroid from an empty set of points")
    require(ps.size > 1, "Cannot compute centroid from a single point.")
    val k = ps.size
    val sumX = ps.map(_.x).sum
    val sumY = ps.map(_.y).sum
    Point(Math.floor(sumX.toDouble / k).toInt, Math.floor(sumY.toDouble / k).toInt)
  }
}
