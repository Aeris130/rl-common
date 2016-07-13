package net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree

/**
  * Responsible for computing the split of an element list, where each split is assigned its own left or right node in
  * the kd tree. To do so, the most common method is to compute the median for all values in the list, then split every
  * value that falls below the median into the left sub-tree, and vice versa for the element above the median.
  *
  * As the median may be costly to compute however, this trait lets the user supply a custom function to handle it. The
  * result of this computation is not needed for the correctness of the tree, a bad split will simply make the tree
  * unbalanced, causing considerable slowdowns in search queries.
  *
  * @tparam E The element type in a KD tree.
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
  def value(element: E, dimension: Int): Int

}
