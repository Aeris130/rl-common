package net.cyndeline.rlcommon.math.geom

/**
  * Common methods for all points.
  *
  * @tparam P Type of class extending this trait.
  * @tparam T Data type used to store coordinates.
  * @tparam IP Point class that uses Integers.
  * @tparam ID Point class that uses Integers.
  */
trait PointInterface[P <: PointInterface[P, T, IP, ID], T, IP, ID] {

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

  def crossProduct(p: P): Double
  def distanceTo(p: P): Double

  def toInt: IP
  def toDouble: ID

}
