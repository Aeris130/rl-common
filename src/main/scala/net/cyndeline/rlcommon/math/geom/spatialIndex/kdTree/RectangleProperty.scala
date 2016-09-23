package net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree

import net.cyndeline.rlcommon.math.geom.Rectangle
import net.cyndeline.rlcommon.math.geom.spatialIndex.common.ElementProperty
import spire.math.Rational

/**
  * Allows a KD tree to perform lookup on orthogonal rectangles, by treating them as a 4-dimensional point.
  * The difference between this and point objects is the data comparison that determines whether or not a subtree can
  * be pruned when performing ranged searches. Rather than comparing the values having the same dimension when deciding
  * if a certain median can have left or right children still within the range, the opposite value in the range is
  * checked instead (min x <-> max x, min y <-> max y).
  *
  * Example: If min x is currently the dimension that the tree is being split at, then max x of the rectangle range
  * is compared. If min x of the current rectangle lies above (i.e to the right) of the range's max x, then one of the
  * left children may still lie within the range, and cannot be pruned.
  */
class RectangleProperty extends ElementProperty[Rectangle] with RangeProperty[Rectangle, Rectangle] {

  /**
    * Min x = 1
    * Min y = 2
    * Max x = 3
    * Max y = 4
    */
  override val totalDimensions: Int = 4

  /**
    * @return Min x, min y, max x, max y. All values are inclusive.
    */
  override def value(element: Rectangle, dimension: Int): Rational = dimension match {
    case 1 => element.start.x
    case 2 => element.start.y
    case 3 => element.start.x + element.width - 1
    case 4 => element.start.y + element.height - 1
    case _ =>
      unspecified(dimension, element)
      0
  }

  /** @return the Distance between two rectangles. */
  override def distance(a: Rectangle, b: Rectangle): Rational = a.shortestDistance(b)

  /** Computes the distance between two rectangles in some dimension. To represent the rectangles area, both coordinates
    * on the same axis must be used for each dimension.
    * @return The shortest distance between two rectangles on some axis.
    */
  override def axisDistance(a: Rectangle, b: Rectangle, dimension: Int): Rational = dimension match {
    case 1 | 3 => dist(a.start.x, a.stop.x, b.start.x, b.stop.x)
    case 2 | 4 => dist(a.start.y, a.stop.y, b.start.y, b.stop.y)
  }

  /**
    * @param e An element in the KD tree.
    * @param r A range to search for elements within.
    * @return True if the element e lies within range r, otherwise false.
    */
  override def isInRange(e: Rectangle, r: Rectangle): Boolean = e.overlaps(r)

  /**
    * @param e An element in the KD tree.
    * @param d A dimension in e.
    * @param r A range to search for elements within.
    * @return True if the range r may contain values found in the right subtree of e in dimension d, otherwise false.
    */
  override def hasRightChildren(e: Rectangle, d: Int, r: Rectangle): Boolean = {
    d match {
      case 1 => value(e, d) <= value(r, 3)
      case 2 => value(e, d) <= value(r, 4)
      case 3 | 4 => true
      case _ => unspecified(d, e)
    }
  }

  /**
    * @param e An element in the KD tree.
    * @param d A dimension in e.
    * @param r A range to search for elements within.
    * @return True if the range r may contain values found in the left subtree of e in dimension d, otherwise false.
    */
  override def hasLeftChildren(e: Rectangle, d: Int, r: Rectangle): Boolean = {
    d match {
      case 3 => value(e, d) >= value(r, 1)
      case 4 => value(e, d) >= value(r, 2)
      case 1 | 2 => true
      case _ => unspecified(d, e)
    }
  }

  private def unspecified(d: Int, element: Rectangle): Boolean = {
    throw new Error("Unspecified dimension " + d + " for shape " + element + ", only dimensions 1 to 4 are supported.")
  }

  private def dist(aLow: Rational, aHigh: Rational, bLow: Rational, bHigh: Rational): Rational =
    if (aHigh >= bLow && bHigh >= aLow) // Overlap
      0
    else if (aHigh < bLow)
      bLow - aHigh
    else
      aLow - bHigh
}
