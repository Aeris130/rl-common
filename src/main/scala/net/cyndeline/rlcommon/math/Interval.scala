package net.cyndeline.rlcommon.math

import Ordering.Implicits._
import scala.collection.mutable.ArrayBuffer

/**
  * A numeric interval.
  */
trait Interval[N] {

  def from: N
  def to: N
  def overlaps(other: Interval[N]): Boolean
  def +(other: Interval[N]): Interval[N]
  def -(other: Interval[N]): Either[Interval[N], (Interval[N], Interval[N])]
  def diff(other: Interval[N]*): Vector[Interval[N]]
  def diff(other: Vector[Interval[N]]): Vector[Interval[N]]
  def isEmpty: Boolean

}

object Interval {
  def apply[N : Ordering](from: N, to: N): Interval[N] = new OpenInterval(from, to)
  def point[N : Ordering](p: N): Interval[N] = new OpenInterval(p, p)
  def empty[N : Ordering]: Interval[N] = new EmptyInterval[N]()
}

class OpenInterval[N : Ordering](val from: N, val to: N) extends Interval[N] {
  require(from <= to, "Start value must be <= to stop value in interval.")

  /**
    * @param other Another interval.
    * @return True if the intervals overlap, otherwise false.
    */
  def overlaps(other: Interval[N]): Boolean = other.from <= this.to && other.to >= this.from

  /**
    * Adds an interval to this one.
    * @param other Another interval to add. Must overlap this one at least partially.
    * @return The sum of the intervals.
    */
  def +(other: Interval[N]): Interval[N] = {
    require(overlaps(other), "Cannot add disjoint intervals.")
    Interval(from.min(other.from), to.max(other.to))
  }

  /**
    * Subtracts another interval from this one.
    * @param other Another interval.
    * @return This interval if no overlap is found, or the remaining interval if the two intervals overlap. If the other
    *         interval overlaps partially between the start and stop values of this one, a left/right split is
    *         returned instead. When subtracting a single-point interval, a split is made where both ends contains
    *         the point.
    */
  def -(other: Interval[N]): Either[Interval[N], (Interval[N], Interval[N])] = if (!overlaps(other)) {
    Left(this)
  } else if (other.from <= from && other.to >= to) {
    Left(new EmptyInterval[N]())
  } else if (other.from > from && other.to < to) {
    Right((new OpenInterval(from, other.from), new OpenInterval(other.to, to)))
  } else {

    if (other.from <= from)
      Left(new OpenInterval(other.to, to))
    else
      Left(new OpenInterval(from, other.from))

  }

  def diff(other: Interval[N]*): Vector[Interval[N]] = diff(other.toVector)

  /**
    * @param other Intervals to remove from this one.
    * @return The list of intervals remaining after all other intervals have been removed from this one. Example: If
    *         [-1,3] and [4,5] is removed from [0,10], the result is [3,4] and [5,10]
    */
  def diff(other: Vector[Interval[N]]): Vector[Interval[N]] = {
    val result = new ArrayBuffer[Interval[N]]()

    /* By sorting the intervals to subtract according to their starting point, the left part of a split can always
     * be reported since no more splits will be made to the left of the current point.
     */
    val sorted = other.sortBy(_.from).iterator

    /* Stores the right split if one occur, or the remaining interval to subtract from if a subtraction results
     * in a single interval.
     */
    var rightmost: Interval[N] = this

    while (sorted.hasNext) {
      val next = sorted.next()

      if (next.overlaps(rightmost)) {
        rightmost - next match {
          case Left(single) => if (single.isEmpty)
              return result.toVector
            else
              rightmost = single
          case Right((leftSplit, rightSplit)) =>
            result += leftSplit
            rightmost = rightSplit
        }
      }
    }
    result += rightmost
    result.toVector
  }

  def isEmpty: Boolean = false

  override def hashCode: Int = from.## + to.##
  override def equals(other: Any): Boolean = other match {
    case open: OpenInterval[N] => open.from == from && open.to == to
    case _ => false
  }
  override def toString: String = s"[$from, $to]"

}

class EmptyInterval[N] extends Interval[N] {
  override def from: N = error("Cannot retrieve value from empty interval")
  override def to: N = error("Cannot retrieve value from empty interval")
  override def overlaps(other: Interval[N]): Boolean = false
  override def +(other: Interval[N]): Interval[N] = other
  override def -(other: Interval[N]): Either[Interval[N], (Interval[N], Interval[N])] = Left(this)
  override def isEmpty: Boolean = true
  override def diff(other: Interval[N]*): Vector[Interval[N]] = Vector()
  override def diff(other: Vector[Interval[N]]): Vector[Interval[N]] = Vector()

  override def hashCode: Int = 41
  override def equals(other: Any): Boolean = other match {
    case i: Interval[N] => i.isEmpty
    case _ => false
  }
  override def toString: String = "Empty interval"

  private def error(str: String): N = {
    throw new Error(str)
  }
}
