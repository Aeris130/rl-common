package net.cyndeline.rlcommon.math

/**
  * A number that can be expressed as a fraction using a numerator and denominator.
  */
class Rational private (init_num: Int, init_denom: Int = 1) extends Ordered[Rational] {
  require(init_denom != 0, "Denominator must be non-zero.")

  private val gcd = Rational.gcd(init_num.abs, init_denom.abs)
  val numer = init_num / gcd * init_denom.signum
  val denom = init_denom.abs / gcd

  def unary_- = new Rational(-numer, denom)
  def abs = new Rational(numer.abs, denom)
  def signum = new Rational(numer.signum)

  def +(that: Rational): Rational = new Rational(that.numer * denom + numer * that.denom, denom * that.denom)

  def -(that: Rational): Rational = this + -that
  def -(that: Int):      Rational = this - new Rational(that)

  def *(that: Rational): Rational = new Rational(numer * that.numer, denom * that.denom)
  def *(that: Int):      Rational = this * new Rational(that)

  def /(that: Rational): Rational = new Rational(numer * that.denom, denom * that.numer)
  def /(that: Int):      Rational = this / new Rational(that)

  def ==(that: Rational): Boolean = that.numer == numer && that.denom == denom
  def ==(that: Int):      Boolean = this == new Rational(that)

  override def <(that: Rational): Boolean = numer * that.denom < that.numer * denom
  def <(that: Int):      Boolean = this < new Rational(that)

  override def >(that: Rational): Boolean = !(this < that)
  def >(that: Int):      Boolean = this > new Rational(that)

  def max(that: Rational): Rational = if (this < that) that else this
  def max(that: Int):      Rational = this.max(new Rational(that))

  override def compare(that: Rational): Int = this.numer * that.denom - that.numer * this.denom

  override def equals(other: Any): Boolean = other match {
    case r: Rational => r.numer == numer && r.denom == denom
    case _ => false
  }
  override def hashCode: Int = 41 * (41 + numer + denom)
  override def toString = if (denom == 1) numer.toString else s"$numer / $denom"

}

object Rational {
  def apply(n: Int) = new Rational(n, 1)
  def apply(n: Int, d: Int) = new Rational(n, d)

  val ONE = new Rational(1)
  val ZERO = new Rational(0)
  val MINUS_ONE = new Rational(-1)

  private def gcd(a: Int, b: Int) : Int = if (b == 0) a else gcd(b, a % b)
}
