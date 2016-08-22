package net.cyndeline.rlcommon.math.geom

import spire.math.Rational

/**
  * A 2D coordinate using rational values.
  */
class RPoint(val x: Rational, val y: Rational) extends PointInterface[RPoint, Rational] {

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

  override def crossProduct(p: RPoint): Rational = (x * p.y) - (y * p.x)

  override def distanceTo(p: RPoint): Double = {
    val dx = x - p.x
    val dy = y - p.y
    val d = dx * dx + dy * dy
    Math.sqrt(d.toDouble)
  }

  private def build(ox: Rational, oy: Rational) = new RPoint(ox, oy)

  override def equals(other: Any): Boolean = other match {
    case r: RPoint => x == r.x && y == r.y
    case _ => false
  }
  override def hashCode: Int = 41 * (41 + x.## + y.##)
  override def toString: String = s"($x, $y)"
}

object RPoint {
  def apply(x: Rational, y: Rational) = new RPoint(x, y)
  def apply(p: Point) = new RPoint(Rational(p.x), Rational(p.y))
  def apply(t: (Int, Int)) = new RPoint(Rational(t._1), Rational(t._2))
  def apply(x: Int, y: Int) = new RPoint(Rational(x), Rational(y))
}
