package net.cyndeline.rlcommon.math.geom

/**
  * Finite chain of straight line segments.
  */
class Polygon(val points: Vector[Point]) extends Shape[Polygon] {

  /**
    * Constructs a rectangular polygon.
    * @param start Start coordinate of the rectangle.
    * @param width Rectangle width.
    * @param height Rectangle height.
    * @return A rectangular polygon.
    */
  def this(start: Point, width: Int, height: Int) =
    this(Vector(start, start + (0, height - 1), start + (width - 1, height - 1), start + (width - 1, 0)))

  override def +(x: Int, y: Int): Polygon = new Polygon(points.map(_ + (x, y)))

  override def -(x: Int, y: Int): Polygon = new Polygon(points.map(_ - (x, y)))

  override def *(x: Int, y: Int): Polygon = new Polygon(points.map(_ * (x, y)))

}
