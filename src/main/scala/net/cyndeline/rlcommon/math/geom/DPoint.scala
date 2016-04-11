package net.cyndeline.rlcommon.math.geom

import scala.language.implicitConversions

/**
  * X/Y coordinates based on Double values.
  */
class DPoint(val x: Double, val y: Double) extends PointInterface[DPoint, Double, Point, DPoint] {
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

  override def crossProduct(p: DPoint): Double = (x * p.y) - (y * p.x)

  override def distanceTo(p: DPoint): Double = {
    val dx = x - p.x
    val dy = y - p.y
    Math.sqrt(dx * dx + dy * dy)
  }

  override def toInt: Point = Point(this)
  override def toDouble: DPoint = this

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
}
