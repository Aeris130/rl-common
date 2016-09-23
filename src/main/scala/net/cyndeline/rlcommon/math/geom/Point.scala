package net.cyndeline.rlcommon.math.geom

import spire.math.Rational

/**
 * A pair of x/y coordinates.
 *
 * @constructor Constructs a new point representing a coordinate on the positive side of the cartesian grid.
 * @param x The x dimension on the grid.
 * @param y The y dimension on the grid.
 */
class Point(val x: Int, val y: Int) extends PointInterface[Point, Int] {

  /**
   * @param t A tuple containing the x and y coordinate the create a new point from.
   * @return A point object using t as x/y coordinates.
   */
  def this(t: (Int, Int)) = this(t._1, t._2)

  override def asTuple: (Int, Int) = (x, y)

  //TODO phase out usage in other libraries
  def modify(amount: Int): Point = Point(x + amount, y + amount)
  def modifyX(amount: Int): Point = Point(x + amount, y)
  def modifyY(amount: Int): Point = Point(x, y + amount)

  override def +(nx: Int, ny: Int): Point = Point(x + nx, y + ny)
  override def +(n: Int): Point = this + (n, n)
  override def +(p: Point): Point = this + (p.x, p.y)
  override def -(nx: Int, ny: Int): Point = Point(x - nx, y - ny)
  override def -(n: Int): Point = this - (n, n)
  override def -(p: Point): Point = this - (p.x, p.y)
  override def *(mx: Int, my: Int): Point = Point(x * mx, y * my)
  override def *(s: Int): Point = this * (s, s)
  override def *(p: Point): Point = this * (p.x, p.y)

  override def move(angle: Rational, distance: Rational): Point = {
    Point(RPoint(this).move(angle, distance))
  }

  override def crossProduct(p: Point): Int = DPoint(this).crossProduct(DPoint(p)).toInt
  override def distanceTo(p: Point): Rational = DPoint(this).distanceTo(DPoint(p))

  override def equals(other: Any): Boolean = other match {
    case p: Point => x == p.x && y == p.y
    case _ => false
  }

  override def hashCode: Int = x ^ (y << 32)

  override def toString: String = "Point(" + x + ", " + y + ")"
}

object Point {
  def apply(x: Int, y: Int): Point = new Point(x, y)
  def apply(xy: (Int, Int)): Point = new Point(xy._1, xy._2)
  def apply(point: DPoint): Point = new Point(point.x.toInt, point.y.toInt)
  def apply(r: RPoint): Point = new Point(r.x.intValue(), r.y.intValue())

  /**
    * @return A basic ordering that sorts points based on x values, then y values.
    */
  def coordinateOrdering: Ordering[Point] = new Ordering[Point]() {
    override def compare(a: Point, b: Point): Int = if (a.x < b.x) -1
    else if (b.x < a.x) 1
    else if (a.y < b.y) -1
    else if (b.y < a.y) 1
    else 0
  }

}
