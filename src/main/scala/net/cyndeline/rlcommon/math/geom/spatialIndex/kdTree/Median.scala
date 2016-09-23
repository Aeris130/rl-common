package net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree

import java.util.Comparator

import net.cyndeline.rlcommon.math.geom.spatialIndex.common.ElementProperty
import net.cyndeline.rlcommon.sorting.HeapSort

import Ordering.Implicits._

/**
  * Splits a list of elements based on a median value, then reports the median and both splits. The running time
  * for the algorithm is O(kn log n), as outlined in the journal "Building a Balanced k-d Tree in O(kn log n) Time"
  * by Russel A. Brown, and uses a merge sort to order elements.
  */
class Median[E] private (prop: ElementProperty[E],
                        superKeys: Vector[Ordering[E]],
                        lists: Vector[Vector[E]]) {

  /** @return True if this median doesn't contain any additional elements to split. Otherwise false. */
  def isEmpty: Boolean = lists(0).isEmpty

  /**
    * Splits the current list into two. When doing so, the internal list of elements sorted by the specified dimension
    * doesn't need to be partitioned further, but every other list must be partitioned with regards to the specified
    * dimension. To do that, every element in the list is compared to the median using the super key for the dimension,
    * and placed in the left split if it is lower, or the right split if it is higher.
    *
    * @return The median value that was selected, the remaining median-lists to the left and right of the selected value.
    */
  def split(dimension: Int): (E, Median[E], Median[E]) = {
    val size = lists(0).length
    val dimIdx = dimension - 1

    // Start by finding the new value to use as median
    val newMedian = if (size % 2 == 0) lists(dimIdx)((size / 2) - 1) else lists(dimIdx)(Math.floor(size / 2.0).toInt)
    val superKey = superKeys(dimension - 1)

    var leftSplit = Vector[Vector[E]]() ++ (for (n <- lists.indices) yield Vector())
    var rightSplit = leftSplit

    // Note that the new median won't be less or greater than itself, so it will not be passed into the splits
    for (i <- 0 until prop.totalDimensions; element <- lists(i)) {
      if (superKey.lt(element, newMedian))
        leftSplit = leftSplit.updated(i, element +: leftSplit(i))
      else if (superKey.gt(element, newMedian))
        rightSplit = rightSplit.updated(i, element +: rightSplit(i))
    }

    (newMedian, new Median(prop, superKeys, leftSplit), new Median(prop, superKeys, rightSplit))
  }

  override def toString: String = "Median storing: " + lists.mkString(", ")
}

/**
  * Factory object used to create the initial Median objects that contains the entire tree data set.
  */
object Median {

  /**
    * @param initialElements Every initial element to build a KD tree from, unsorted.
    * @param prop Property helper object of the KD trees data set.
    * @tparam E Element type in the KD tree.
    * @return A median object that splits the initial data set.
    */
  def apply[E](initialElements: Vector[E], prop: ElementProperty[E]): Median[E] = {
    require(initialElements.nonEmpty, "Cannot compute the median for an empty element list.")
    require(prop.totalDimensions > 0, "At least one dimension must be specified.")

    /* Create super keys based on the cyclical permutations of all dimensions. Given 4 dimensions, the first
     * key will have the order [1,2,3,4], the second will have [2,3,4,1], the third [3,4,1,2] etc. The first
     * dimension in each key (the most significant one) will always be in the order 1,2,3...n.
     */
    val keys: Vector[Ordering[E]] = generateSuperKeys(prop, prop.totalDimensions)

    /* For each dimension, generate a sorted list of elements using the ordering where said dimension is the
     * most significant.
     */
    val sortedLists = for (i <- 1 to prop.totalDimensions) yield {
      val superKey = keys(i - 1)
      discardDuplicates(HeapSort(initialElements)(superKey))
    }

    new Median[E](prop, keys, sortedLists.toVector)
  }

  private def generateSuperKeys[E](prop: ElementProperty[E], dimensions: Int): Vector[Ordering[E]] = {
    var current: Vector[Int] = (1 to dimensions).toVector
    val first = superKey(prop, current)

    // Skip index 0, as it is the original dimension list
    val keys = for (i <- 1 until dimensions) yield {
      val p = current.drop(1) :+ current.head
      current = p
      superKey(prop, p)
    }

    first +: keys.toVector
  }

  private def superKey[E](prop: ElementProperty[E], dimensionsPrio: Vector[Int]): Ordering[E] = {
    Ordering.comparatorToOrdering(new SuperKey(prop, dimensionsPrio))
  }

  /**
    * Compares elements by returning lt/gt as soon as it finds a dimension whose value differs between the objects
    * (i.e the "most significant" value). The order in which the dimensions should be significant is user-specified.
    */
  private class SuperKey[T](prop: ElementProperty[T], dimensionPrio: Vector[Int]) extends Comparator[T] {
    override def compare(o1: T, o2: T): Int = {
      for (i <- dimensionPrio.indices) {
        val o1Value = prop.value(o1, dimensionPrio(i))
        val o2Value = prop.value(o2, dimensionPrio(i))
        if (o1Value < o2Value)
          return -1
        else if (o1Value > o2Value)
          return 1
      }

      0 // No differing value found, both objects are equal
    }
  }

  private def discardDuplicates[E](remaining: Vector[E]): Vector[E] = if (remaining.isEmpty) {
    remaining
  } else {
    remaining.head +: discardDuplicates(remaining.drop(1), remaining.head)
  }
  private def discardDuplicates[E](remaining: Vector[E], last: E): Vector[E] = {
    if (remaining.isEmpty)
      remaining
    else if (remaining.head == last)
      discardDuplicates(remaining.drop(1), last)
    else
      remaining.head +: discardDuplicates(remaining.drop(1), remaining.head)
  }

}
