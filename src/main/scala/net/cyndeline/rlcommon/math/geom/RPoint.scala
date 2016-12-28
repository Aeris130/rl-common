package net.cyndeline.rlcommon.math.geom

import spire.math.Rational

import scala.language.implicitConversions

/**
  * A 2D coordinate using rational values.
  */
class RPoint(val x: Rational, val y: Rational) extends Point2D[RPoint, Rational] with Ordered[RPoint] {

  override def asTuple: (Rational, Rational) = (x, y)

  override def +(nx: Rational, ny: Rational): RPoint = build(x + nx, y + ny)

  override def +(n: Rational): RPoint = build(x + n, y + n)

  override def +(p: RPoint): RPoint = build(x + p.x, y + p.y)

  override def -(nx: Rational, ny: Rational): RPoint = build(x - nx, y - ny)

  override def -(n: Rational): RPoint = build(x - n, y - n)

  override def -(p: RPoint): RPoint = build(x - p.x, y - p.y)

  override def *(mx: Rational, my: Rational): RPoint = build(x * mx, y * my)

  override def *(s: Rational): RPoint = build(x * s, y * s)

  override def *(p: RPoint): RPoint = build(x * p.x, y * p.y)

  override def move(angle: Double, distance: Double): RPoint = {
    val radians = Math.toRadians(angle)
    val newX = x + (distance * Math.cos(radians))
    val newY = y + (distance * Math.sin(radians))
    RPoint(newX, newY)
  }

  override def crossProduct(p: RPoint): Rational = (x * p.y) - (y * p.x)

  override def distanceTo(p: RPoint): Double = {
    val dx = x - p.x
    val dy = y - p.y
    val d = dx * dx + dy * dy
    Math.sqrt(d.toDouble)
  }

  override def angleTo(p: RPoint): Double = {
    DPoint(this).angleTo(DPoint(p))
  }

  private def build(ox: Rational, oy: Rational) = new RPoint(ox, oy)

  override def equals(other: Any): Boolean = other match {
    case r: RPoint => x == r.x && y == r.y
    case _ => false
  }
  override def hashCode: Int = 41 * (41 + x.## + y.##)
  override def toString: String = s"($x, $y)"

  override def compare(that: RPoint): Int = {
    val xComp = this.x.compare(that.x)
    if (xComp != 0)
      xComp
    else {
      val yComp = this.y.compare(that.y)
      if (yComp != 0)
        yComp
      else
        0
    }
  }
}

object RPoint {
  def apply(x: Rational, y: Rational) = new RPoint(x, y)
  def apply(p: Point) = new RPoint(Rational(p.x), Rational(p.y))
  def apply(x: Int, y: Int) = new RPoint(Rational(x), Rational(y))
  def apply(r: (Rational, Rational)) = new RPoint(r._1, r._2)
  def apply(p: DPoint): RPoint = RPoint(Rational(p.x), Rational(p.y))

  implicit def toPoint(r: RPoint): Point = Point(r)
  implicit def toRPoint(p: Point): RPoint = RPoint(p)
}
