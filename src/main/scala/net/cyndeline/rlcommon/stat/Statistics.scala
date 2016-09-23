package net.cyndeline.rlcommon.stat

import spire.math.Rational
import Numeric.Implicits._

/**
  * http://www.sciencebuddies.org/science-fair-projects/project_data_analysis_variance_std_deviation.shtml
  */
object Statistics {

  def mean[N : Numeric](data: Vector[N]): Double = data.map(_.toDouble()).sum / data.size

  def variance[N : Numeric](data: Vector[N]): Double = {
    val n = data.size
    val m = mean(data)
    (data.map(v => v.toDouble() * v.toDouble()).sum / n) - (m * m)
  }

  def stdDeviation[N : Numeric](data: Vector[N]): Double = {
    Math.sqrt(variance(data))
  }

}
