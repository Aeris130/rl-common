package net.cyndeline.rlcommon.math.geom

/**
  * A 2D shape.
  *
  * @tparam S Type of shape implementing this trait.
  */
trait Shape[+S <: Shape[S]] {

  /** @return A set of coordinates that must be traversed when moving around the shape. */
  def points: Vector[Point]

  def +(x: Int, y: Int): S
  final def + (v: Int): S = this + (v, v)

  def -(x: Int, y: Int): S
  final def -(v: Int): S = this - (v, v)

  def *(x: Int, y: Int): S
  final def *(v: Int): S = this * (v, v)

}
