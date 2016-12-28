package net.cyndeline.rlcommon.math.geom.spatialIndex.intervalTree

import net.cyndeline.rlcommon.collections.{RBTree, ValueFactory}
import net.cyndeline.rlcommon.math.geom.spatialIndex.common.ElementProperty
import net.cyndeline.rlcommon.math.geom.spatialIndex.intervalTree.IntervalTree.{IntervalData, IntervalSegmentData, IntervalTreeData}

import scala.language.implicitConversions
import Ordering.Implicits._

/**
  * Takes an interval i and returns every intersecting interval t in the tree in O(log(n)`^`d + k) time, where d is
  * the dimension of the tree, and k is the number of reported intersections. Insertion and deletion takes O(log n)
  * time, where n is the size of the tree. Storage is O(n).
  *
  * @param data The tree structure containing the intervals for this dimension.
  * @param ep Takes an endpoint of an interval and a dimension, and returns the value for that dimension. For an
  *            interval in n dimensions, the first dimension is 1 and the last is n.
  * @param dimension The dimension of the intervals represented by this tree.
  * @param printer Optional function that converts a value T into a more readable format. Mainly for debugging.
  * @tparam T Value used for intervals [T1 ... T2] where T1 <= T2.
  */
class IntervalTree[T: Ordering] private (data: RBTree[IntervalData[T]],
                                         ep: ElementProperty[T],
                                         dimension: Int,
                                         printer: Option[T => String] = None) {
  require(dimension > 0, s"The lowest dimension allowed is 1, currently $dimension")
  require(dimension <= ep.totalDimensions, s"The highest allowed dimension in the element property is ${ep.totalDimensions}, currently $dimension")

  private def this(data: RBTree[IntervalData[T]], ep: ElementProperty[T]) = this(data, ep, 1)

  /** @return True if the tree contains no intervals, otherwise false. */
  def isEmpty: Boolean = data.isEmpty

  /** Inserts an interval into the tree if one doesn't already exist.
    * @param from Starting point in the interval.
    * @param to Ending point in the interval, must be >= to start.
    * @return A copy of the tree with the interval inserted.
    */
  def insert(from: T, to: T): IntervalTree[T] = modify(from, to, add = true)

  /**
    * Deletes an interval from the tree.
    * @param from Starting point in the interval.
    * @param to Ending point in the interval, must be >= to start.
    * @return A copy of the tree where the interval [from, to] has been removed.
    */
  def delete(from: T, to: T): IntervalTree[T] = modify(from, to, add = false)

  /** Finds every interval that intersects a given interval t.
    * @param from Starting point of t.
    * @param to Ending point of t.
    * @return Every interval in the tree that overlaps t.
    */
  def search(from: T, to: T): Set[(T, T)] = {
    val lowPoint = if (from < to) from else to
    val highPoint = if (lowPoint == from) to else from
    val lowest = ep.value(lowPoint, dimension)
    val highest = ep.value(highPoint, dimension)

    def traverse(n: RBTree[IntervalData[T]]): Set[(T, T)] = if (n.isEmpty) {
      Set()
    } else {
      val left = if (!n.left.isEmpty && n.left.value.max >= lowest) {
        traverse(n.left)
      } else {
        Set[(T, T)]()
      }
      val right = if (!n.right.isEmpty && lowPoint <= to) {
        traverse(n.right)
      } else {
        Set[(T, T)]()
      }
      val current = if (n.value.low <= highest && n.value.high >= lowest)
        findIntervals(n)
      else
        Set[(T, T)]()

      left ++ right ++ current
    }

    def findIntervals(n: RBTree[IntervalData[T]]): Set[(T, T)] = if (n.isEmpty) {
      Set()
    } else {
      n.value match {
        case isd: IntervalSegmentData[T] => isd.intervals
        case itd: IntervalTreeData[T] => itd.nextDim.search(lowPoint, highPoint) // Recurse to next dimension
      }
    }

    traverse(data)
  }

  /**
    * Retrieves all intervals in the tree in O(n) time.
    * @return Every interval in the tree.
    */
  def values: Set[(T, T)] = {
    def getAll(n: RBTree[IntervalData[T]]): Set[(T, T)] = if (n.isEmpty) {
      Set()
    } else {
      (n.value match {
        case isd: IntervalSegmentData[T] => isd.intervals
        case itd: IntervalTreeData[T] => itd.nextDim.values // Recurse to next dimension
      }) ++ getAll(n.left) ++ getAll(n.right)
    }

    getAll(data)
  }

  /** @return A copy of the tree, using the specified printer. */
  def withPrinter(p: T => String) = new IntervalTree(data, ep, dimension, Some(p))

  private def modify(from: T, to: T, add: Boolean): IntervalTree[T] = {
    if (from > to)
      modify(to, from, add)
    else {
      val fact = IntervalTree.buildIntervalFactory(ep, dimension)
      val insertData = buildInterval(from, to)

      val updatedTree = if (data.contains(insertData) /* Comparison done using low/high values for this dimension */) {
        val oldEntry: IntervalData[T] = data.subTree(insertData).value
        val updatedEntry = updateEntry(from, to, oldEntry, add)
        val removed = data.delete(oldEntry, Some(fact))
        if (!updatedEntry.isEmpty)
          removed.insert(updatedEntry, Some(fact))
        else
          removed

      } else if (add) /* No previous entry found */ {
        val updatedEntry = updateEntry(from, to, insertData, add)
        data.insert(updatedEntry, Some(fact))

      } else {
        data
      }

      new IntervalTree(updatedTree, ep, dimension, printer)
    }
  }

  private def withDimension(d: Int) = new IntervalTree(data, ep, d, None)

  private def updateEntry(low: T, high: T, entry: IntervalData[T], add: Boolean = true) = entry match {
    case isd: IntervalSegmentData[T] =>
      assert(dimension == ep.totalDimensions, "Interval set data must be stored at the highest dimension.")
      if (add) isd.add(low, high)
      else isd.remove(low, high)

    case itd: IntervalTreeData[T] =>
      assert(dimension < ep.totalDimensions, "Nested interval trees must be stored below the highest dimension.")
      if (add) itd.add(low, high)
      else itd.remove(low, high)
  }

  private def buildInterval(a: T, b: T): IntervalData[T] = {
    val aValue = ep.value(a, dimension)
    val bValue = ep.value(b, dimension)
    val low = if (aValue < bValue) aValue else bValue
    val high = if (low == aValue) bValue else aValue
    val max = high // Adjusted on insertion
    if (dimension == ep.totalDimensions) {
      new IntervalSegmentData(low, high, max, Set())
    } else {
      // If the interval already exists, the current tree will be retrieved and updated, so an empty tree is enough.
      new IntervalTreeData(low, high, max, IntervalTree.empty(ep).withDimension(dimension + 1))
    }
  }

  override def toString: String = {
    val builder = new StringBuilder()

    if (this.isEmpty) {
      builder ++= "Empty interval tree"
    } else {
      val valuesToPrint = if (printer.isDefined) values.map(v => (printer.get(v._1), printer.get(v._2)))
        else values.map(_.toString())
      builder ++= String.format("Interval tree%n")
      builder ++= valuesToPrint.mkString(", ")
    }

    builder.toString()
  }

}

object IntervalTree {

  /**
    * Creates an empty interval tree.
    * @param ep Specifies the total number of dimensions for the tree, and what value resides at each dimension for
    *           some endpoint.
    * @tparam T Endpoint type.
    * @return An empty interval tree.
    */
  def empty[T : Ordering](ep: ElementProperty[T]) = new IntervalTree[T](RBTree.empty[IntervalData[T]], ep)

  /**
    * @param low The lowest endpoint. Used as key in the tree.
    * @param high The highest endpoint in the interval.
    * @param max The highest endpoint between this node and its children max values.
    */
  private abstract class IntervalData[T : Ordering](val low: Int, val high: Int, val max: Int) extends Ordered[IntervalData[T]] {
    def compare(other: IntervalData[T]): Int = {
      if (low < other.low) -1
      else if (low > other.low) 1
      else if (high < other.high) -1
      else if (high > other.high) 1
      else 0
    }

    def setMax(newMax: Int): IntervalData[T]
    def add(from: T, to: T): IntervalData[T]
    def remove(from: T, to: T): IntervalData[T]
    def isEmpty: Boolean
  }

  /** Value container for each dimension in the nested interval tree apart from the last.
    * @param nextDim The nested interval tree containing intervals for dimension d+1, where d is the dimension of
    *                this tree.
    * @tparam T End point type.
    */
  private class IntervalTreeData[T : Ordering](l: Int, h: Int, m: Int, val nextDim: IntervalTree[T]) extends IntervalData[T](l, h, m) {
    override def setMax(newMax: Int) = new IntervalTreeData(l, h, newMax, nextDim)
    override def add(from: T, to: T) = new IntervalTreeData(l, h, m, nextDim.insert(from, to))
    override def remove(from: T, to: T) = new IntervalTreeData(l, h, m, nextDim.delete(from, to))
    override def isEmpty = nextDim.isEmpty
  }

  /** Stores the user-inputted intervals at the lowest nested interval tree.
    * @param intervals Every interval that uses spatial data (low, high) for this dimension and every one before it.
    * @tparam T Endpoint type.
    */
  private class IntervalSegmentData[T : Ordering](l: Int, h: Int, m: Int, val intervals: Set[(T, T)]) extends IntervalData[T](l, h, m) {
    override def setMax(newMax: Int) = new IntervalSegmentData(l, h, newMax, intervals)
    override def add(from: T, to: T) = new IntervalSegmentData(l, h, m, intervals + ((from, to)))
    override def remove(from: T, to: T) = new IntervalSegmentData(l, h, m, intervals - ((from, to)))
    override def isEmpty = intervals.isEmpty
  }

  /** Constructs the factory that processes interval nodes and updates their low/max values and dimensional sub trees.
    * @param ep Retrieves the data relevant for the dimension that the factory works with.
    * @param dim Dimension to use.
    * @tparam T End point type.
    * @return Factory to send to tree modification queries.
    */
  private def buildIntervalFactory[T](ep: ElementProperty[T], dim: Int) = {
    implicit val ord = new Ordering[IntervalData[T]] {
      override def compare(x: IntervalData[T], y: IntervalData[T]): Int = x compare y
    }

    def max(a: Int, b: Int): Int = if (a > b) a else b

    new ValueFactory[IntervalData[T]] {
      override def mValue(v: IntervalData[T], left: RBTree[IntervalData[T]], right: RBTree[IntervalData[T]]): IntervalData[T] = {
        val lMax = if (left.isEmpty) v.high else left.value.max
        val rMax = if (right.isEmpty) v.high else right.value.max

        if (lMax != v.max || rMax != v.max)
          v.setMax(max(lMax, rMax))
        else
          v
      }
    }
  }

}
