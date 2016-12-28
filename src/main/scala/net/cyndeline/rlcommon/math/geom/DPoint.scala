package net.cyndeline.rlcommon.math.geom

import scala.language.implicitConversions

/**
  * X/Y coordinates based on Double values.
  */
class DPoint(val x: Double, val y: Double) extends Point2D[DPoint, Double] {
  private val epsilon = 1e-3

  override def asTuple = (x, y)

  override def +(nx: Double, ny: Double): DPoint = DPoint(x + nx, y + ny)
  override def +(p: DPoint): DPoint = this + (p.x, p.y)
  override def +(n: Double): DPoint = this + (n, n)
  override def -(nx: Double, ny: Double): DPoint = DPoint(x - nx, y - ny)
  override def -(n: Double): DPoint = this - (n, n)
  override def -(p: DPoint): DPoint = this - (p.x, p.y)
  override def *(mx: Double, my: Double): DPoint = DPoint(x * mx, y* my)
  override def *(s: Double): DPoint = this * (s, s)
  override def *(p: DPoint): DPoint = this * (p.x, p.y)

  override def move(angle: Double, distance: Double): DPoint = DPoint(Point2D.move(x, y, angle, distance))

  override def crossProduct(p: DPoint): Double = Point2D.crossProduct(x, y, p.x, p.y)

  override def distanceTo(p: DPoint): Double = Point2D.distanceTo(x, y, p.x, p.y)

  override def angleTo(p: DPoint): Double = {
    val angle = Math.toDegrees(Math.atan2(p.y - y, p.x - x))
    if (angle < 0){
      angle + 360
    } else {
      angle
    }
  }

  override def compare(that: DPoint): Int = if (this.x < that.x) -1
    else if (that.x < this.x) 1
    else if (this.y < that.y) -1
    else if (that.y < this.y) 1
    else 0

  override def equals(other: Any): Boolean = other match {
    case dp: DPoint => Math.abs(x - dp.x) < epsilon && Math.abs(y - dp.y) < epsilon
    case _ => false
  }

  override def hashCode: Int = x.## ^ y.##

  override val toString: String = "DPoint(" + x + ", " + y + ")"

}

object DPoint {
  implicit def intToDoublePoint(ip: Point): DPoint = DPoint(ip)

  def apply(x: Double, y: Double) = new DPoint(x, y)
  def apply(p: Point) = new DPoint(p.x, p.y)
  def apply(xy: (Double, Double)) = new DPoint(xy._1, xy._2)
  def apply(r: RPoint) = new DPoint(r.x.doubleValue(), r.y.doubleValue())

}
