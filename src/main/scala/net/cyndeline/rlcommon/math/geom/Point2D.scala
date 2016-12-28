package net.cyndeline.rlcommon.math.geom

/**
  * Common methods for all points.
  *
  * @tparam P Type of class extending this trait.
  * @tparam T Data type used to store coordinates.
  */
trait Point2D[P <: Point2D[P, T], T] extends Ordered[P] {

  def x: T
  def y: T

  def asTuple: (T, T)

  def +(nx: T, ny: T): P
  def +(n: T): P
  def +(p: P): P

  def -(nx: T, ny: T): P
  def -(n: T): P
  def -(p: P): P

  def *(mx: T, my: T): P
  def *(s: T): P
  def *(p: P): P

  def move(angle: Double, distance: Double): P

  def crossProduct(p: P): T
  def distanceTo(p: P): Double
  def angleTo(p: P): Double

}

object Point2D {

  def crossProduct(ax: Double, ay: Double, bx: Double, by: Double) = (ax * by) - (ay * bx)

  def distanceTo(fromX: Double, fromY: Double, toX: Double, toY: Double) = {
    val dx = fromX - toX
    val dy = fromY - toY
    Math.sqrt(dx * dx + dy * dy)
  }

  def move(x: Double, y: Double, angle: Double, distance: Double): (Double, Double) = {
    val radians: Double = Math.toRadians(angle)
    val newX: Double = x + (distance * Math.cos(radians))
    val newY: Double = y + (distance * Math.sin(radians))
    (newX, newY)
  }

}
