package net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree

/**
  * Allows range comparisons for user-supplied data objects.
  * @tparam E The element type in a KD tree.
  * @tparam Range The input type for the object representing a range of values in an orthogonal search. For KD trees of
  *               2D points, this may be a tuple of points representing a from-to range.
  */
trait RangeProperty[E, Range] {

  /**
    * @param e An element in the KD tree.
    * @param r A range to search for elements within.
    * @return True if the element e lies within range r, otherwise false.
    */
  def isInRange(e: E, r: Range): Boolean

  /**
    * @param e An element in the KD tree.
    * @param d A dimension in e.
    * @param r A range to search for elements within.
    * @return True if the range r may contain values found in the left subtree of e in dimension d, otherwise false.
    */
  def hasLeftChildren(e: E, d: Int, r: Range): Boolean

  /**
    * @param e An element in the KD tree.
    * @param d A dimension in e.
    * @param r A range to search for elements within.
    * @return True if the range r may contain values found in the right subtree of e in dimension d, otherwise false.
    */
  def hasRightChildren(e: E, d: Int, r: Range): Boolean

}
