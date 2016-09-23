package net.cyndeline.rlcommon.math.geom.spatialIndex.common

import spire.math.Rational

/**
  * Lets the user enter custom data types into multi-dimensional trees by specifying how many dimensions a data type
  * has, and which value that should be mapped to each dimension.
  *
  * @tparam E The element type in a tree.
  */
trait ElementProperty[E] {

  /**
    * The total number of dimensions being represented in the data set.
    */
  val totalDimensions: Int

  /**
    * @param element An element in the tree.
    * @param dimension The dimension of the element that should be returned. In a 2D point, the x value could be the
    *                  first dimensions, while the y value is the second.
    * @return The spatial value of the element, given the current dimension.
    */
  def value(element: E, dimension: Int): Rational

  /**
    * @param a An element in the tree.
    * @param b An element in the tree.
    * @return The distance between the elements a and b.
    */
  def distance(a: E, b: E): Rational

  /**
    * The distance between two elements along the axis of a single dimension. For a pair of 2D points where the first
    * dimension represents the x axis, this is simply abs(a.x - b.x). However, for points representing shapes, the
    * user may want to include multiple dimensions (a rectangle needs to use both start.x and stop.x when checking
    * its distance (or overlap) with another rectangle).
    * @param a An element in the tree.
    * @param b An element in the tree.
    * @param dimension The dimension for which distance between a and b should be checked.
    * @return The distance between the elements a and b in the specified dimension.
    */
  def axisDistance(a: E, b: E, dimension: Int): Rational

}
