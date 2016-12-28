package net.cyndeline.rlcommon.math

/**
  * Created by Tobias Edin on 2016-09-27.
  */
object EuclidianNorm {
  def apply(vector: Vector[Double]) = Math.sqrt(vector.map(Math.pow(_, 2)).sum)
}
