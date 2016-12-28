package net.cyndeline.rlcommon.math.geom

/**
 * A pair of x/y coordinates.
 *
 * @constructor Constructs a new point representing a coordinate on the positive side of the cartesian grid.
 * @param x The x dimension on the grid.
 * @param y The y dimension on the grid.
 */
class Point(val x: Int, val y: Int) extends Point2D[Point, Int] {

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

  override def move(angle: Double, distance: Double): Point = {
    val newC = Point2D.move(x, y, angle, distance)

    /* Round the new movement values away from the current coordinate if
     * the difference is less than 1.
     */
    def round(newValue: Double, old: Int): Int = {
      if (newValue > old + 1 || newValue < old - 1)
        newValue.toInt
      else {
        if (newValue > old)
          Math.ceil(newValue).toInt
        else if (newValue < old)
          Math.floor(newValue).toInt
        else
          newValue.toInt // Equal, doesn't matter
      }
    }
    Point(round(newC._1, x), round(newC._2.toInt, y))
  }

  override def crossProduct(p: Point): Int = Point2D.crossProduct(x, y, p.x, p.y).toInt
  override def distanceTo(p: Point): Double = Point2D.distanceTo(x, y, p.x, p.y)

  override def angleTo(p: Point): Double = {
    val angle = Math.toDegrees(Math.atan2(p.y - y, p.x - x))
    if (angle < 0){
      angle + 360
    } else {
      angle
    }
  }

  override def equals(other: Any): Boolean = other match {
    case p: Point => x == p.x && y == p.y
    case _ => false
  }

  override def hashCode: Int = x ^ (y << 32)

  override def toString: String = "Point(" + x + ", " + y + ")"

  override def compare(that: Point): Int = if (this.x < that.x) -1
    else if (that.x < this.x) 1
    else if (this.y < that.y) -1
    else if (that.y < this.y) 1
    else 0
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
