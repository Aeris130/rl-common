package net.cyndeline.rlcommon.math.geom

/**
  * Results from shapes intersecting and overlapping each other.
  */
trait IntersectCase

case object Intersects extends IntersectCase
case object Inside extends IntersectCase
case object Outside extends IntersectCase