package net.cyndeline.rlcommon.math

/**
  * Normalizes values to some value between [0, 1].
  */
object Normalize {

  /**
    * Normalizes a single value to the range [0, 1].
    * @param v The value to normalize.
    * @param min The lowest possible value of v.
    * @param max The greatest possible value of v.
    * @param num Numeric for the type being normalized.
    * @tparam N Number tpe to normalize.
    * @return The value v normalized to a value between 0 and 1 (inclusive).
    */
  def apply[N](v: N, min: N, max: N)(implicit num: Numeric[N]): Double = {
    require(num.gteq(v, min) && num.lteq(v, max), s"When normalizing the value v, v must lie within the bounds of the min and max value. $v is outside [$min, $max].")
    val minVal = num.min(min, max)
    val maxVal = num.max(min, max)
    if (num.minus(maxVal, minVal) == num.zero) {
      0
    } else {
      // (v - minV) / (maxV - minV)
      num.toDouble(num.minus(v, minVal)) / num.toDouble(num.minus(maxVal, minVal))
    }
  }
}
