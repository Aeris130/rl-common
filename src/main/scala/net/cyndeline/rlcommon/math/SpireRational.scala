package net.cyndeline.rlcommon.math

import spire.math.Rational

/**
  * Numerics for use with the spire library's Rational class.
  *
  * Usage: import net.cyndeline.rlcommon.math.SpireRational._
  */
object SpireRational {
  implicit object SRNum extends Numeric[Rational] {
    override def plus(x: Rational, y: Rational): Rational = x + y
    override def minus(x: Rational, y: Rational): Rational = x - y
    override def times(x: Rational, y: Rational): Rational = x * y
    override def negate(x: Rational): Rational = -x
    override def fromInt(x: Int): Rational = Rational(x)
    override def toInt(x: Rational): Int = x.intValue()
    override def toLong(x: Rational): Long = x.longValue()
    override def toFloat(x: Rational): Float = x.floatValue()
    override def toDouble(x: Rational): Double = x.doubleValue()
    override def compare(x: Rational, y: Rational): Int = x.compare(y)
  }
}
