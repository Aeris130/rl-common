package net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree

import net.cyndeline.rlcommon.collections.BoundedPriorityQueue
import net.cyndeline.rlcommon.math.geom.spatialIndex.common.ElementProperty
import net.cyndeline.rlcommon.math.geom.{RPoint, Rectangle}
import spire.math.Rational

import Ordering.Implicits._

/**
  * A spatial index allowing for log(n) inserts, deletions and lookups, given that the tree is balanced. Creating
  * a new tree takes O(kn log n) time, where k is the number of dimensions in the data set. Balancing the tree is
  * done in O(kn log n) time using the median algorithm by Russel A. Brown.
  *
  * @tparam E The element (point) value in the tree.
  * @tparam R The values used as ranges when performing range searches.
  */
abstract class KDTree[E : Ordering, R](prop: ElementProperty[E], rangeProp: RangeProperty[E, R]) {

  /** The value stored in this tree node (i.e in the root of this tree). */
  def value: E

  /** True if this tree doesn't contain a value. */
  def isEmpty: Boolean

  def left: KDTree[E, R]
  def right: KDTree[E, R]

  /**
    * Retrieves all elements of the tree and constructs a new one, thereby balancing the data set in O(kn log n) time.
    * @return A new tree instance with its data set balanced.
    */
  def balance: KDTree[E, R] = KDTree(values, prop, rangeProp)

  /**
    * @param e Element to search for.
    * @return Tree node containing the element.
    */
  def get(e: E): KDTree[E, R] = node(e).getOrElse(throw new NoSuchElementException("Tree did not contain " + e))

  /**
    * Retrieves the values of the tree in O(n) time.
    *
    * @return Every value in the tree, without spatial order.
    */
  def values: Vector[E] = {
    def findValues(tree: KDTree[E, R]): Vector[E] = if (tree.isEmpty) {
      Vector()
    } else {
      tree.value +: (findValues(tree.left) ++ findValues(tree.right))
    }
    findValues(this)
  }

  /**
    * @param e Element to check existence for.
    * @return True if the tree contains the element, otherwise false.
    */
  def contains(e: E): Boolean = node(e).isDefined

  /**
    * Adding individual element may lead to an unbalanced tree, increasing query times.
    * @param e An element to add to the tree.
    * @return The tree resulting from adding the element. Duplicates will not be added.
    */
  def insert(e: E): KDTree[E, R] = {
    def add(tree: KDTree[E, R], depth: Int): KDTree[E, R] = if (tree.isEmpty) {
      Branch(e, KDTree.empty(prop, rangeProp), KDTree.empty(prop, rangeProp), prop, rangeProp)
    } else {
      val dimension = (depth % prop.totalDimensions) + 1
      val eValue = prop.value(e, dimension)
      val thisValue = prop.value(tree.value, dimension)

      if (eValue < thisValue)
        Branch(tree.value, add(tree.left, depth + 1), tree.right, prop, rangeProp)
      else if (eValue > thisValue)
        // Greater or equal than the current value adds the element to the right side of the tree
        Branch(tree.value, tree.left, add(tree.right, depth + 1), prop, rangeProp)
      else {
        if (tree.value == e) {
          tree // No duplicates should be inserted
        } else {

          // By default, new elements goes into the left tree if they share properties
          Branch(tree.value, add(tree.left, depth + 1), tree.right, prop, rangeProp)

        }
      }
    }

    add(this, 0)
  }

  /**
    * @param dimension A dimension in the data set.
    * @return The data in the tree for which the value mapped to the specified dimension is the lowest.
    */
  def min(dimension: Int): KDTree[E, R] = {
    minimum(this, dimension, 0)
  }

  /**
    * @param value Element to delete in the tree.
    * @return The tree that results from the deletion. If the element was not present, the original tree is returned.
    */
  def delete(value: E): KDTree[E, R] = {
    def del(e: E, tree: KDTree[E, R], depth: Int): KDTree[E, R] = if (tree.isEmpty) {
      tree
    } else {
      val dimension = (depth % prop.totalDimensions) + 1
      val nextDepth = depth + 1

      if (tree.value == e) {
        if (!tree.right.isEmpty) {
          // Replace the value to be deleted with the inorder successor
          val newValue = minimum(tree.right, dimension, nextDepth)
          Branch(newValue.value, tree.left, del(newValue.value, tree.right, nextDepth), prop, rangeProp)

        } else if (!tree.left.isEmpty) {
          /* If there's a left sub tree but no right, swap the sub trees and use min from the new right. */
          val newValue = minimum(tree.left, dimension, nextDepth)
          Branch(newValue.value, tree.right, del(newValue.value, tree.left, nextDepth), prop, rangeProp)

        } else {
          // No child nodes, delete this node by turning it into an empty node instead.
          KDTree.empty[E, R](prop, rangeProp)
        }

      } else {
        val eValue = prop.value(tree.value, dimension)
        val eToDeleteValue = prop.value(e, dimension)
        val newLeft = if (eToDeleteValue <= eValue) del(e, tree.left, nextDepth) else tree.left
        val newRight = if (eToDeleteValue >= eValue) del(e, tree.right, nextDepth) else tree.right
        Branch(tree.value, newLeft, newRight, prop, rangeProp)
      }
    }

    del(value, this, 0)
  }

  /**
    * @param range An orthogonal range of values.
    * @return Every element in the tree that lies within the range.
    */
  def rangeSearch(range: R): Vector[E] = {
    def ran(tree: KDTree[E, R], depth: Int): Vector[E] = if (tree.isEmpty) {
      Vector()
    } else {
      val dimension = (depth % prop.totalDimensions) + 1
      val newDepth = depth + 1
      val leftSubRange = if (rangeProp.hasLeftChildren(tree.value, dimension, range))
        ran(tree.left, newDepth)
      else
        Vector()

      val rightSubRange = if (rangeProp.hasRightChildren(tree.value, dimension, range))
        ran(tree.right, newDepth)
      else
        Vector()

      if (rangeProp.isInRange(tree.value, range))
        tree.value +: (leftSubRange ++ rightSubRange)
      else
        leftSubRange ++ rightSubRange
    }

    ran(this, 0)
  }

  /**
    * Finds the k nearest neighbors to a given point in O(k log n) time if the tree is balanced.
    * @param k The number of neighbors to return.
    * @param p A point in the tree.
    * @return The k points in the tree that lies closest to p, or every point if he tree has less than k elements.
    *         If p is in the tree, it will be returned along with (k-1) other points.
    */
  def nearestNeighbor(k: Int, p: E): Vector[E] = {
    require(k >= 0, "Number of neighbors sought must be >= 0.")
    knn(k, p, 1)
  }

  /**
    * An optimization of the k-nearest-neighbor algorithm, using an approximation value 'a. During the algorithms
    * traversal, any sub-tee whose bounding box lies further away than the distance r, which is the distance to
    * the current furthest away neighbor in the result. This heuristic instead uses the value (r / 'a),
    * resulting in a much faster traversal due to increased pruning. This no longer guarantees that the result will
    * contain all the nearest neighbors, but if a neighbor with distance d to the search point is returned, no
    * missed neighbor can be closer to the search point than (r / 'a). Overall, the search saves lots of time
    * at little cost to the quality of the result.
    * @param approx Denominator to use when pruning sub-trees.
    */
  def nearestNeighborApproximation(k: Int, p: E, approx: Int): Vector[E] = {
    require(approx > 1, "Approximation value must > 1, as 1 results in a regular KNN search.")
    knn(k, p, approx)
  }

  private def knn(k: Int, p: E, approximation: Rational): Vector[E] = {
    if (k == 0)
      return Vector()

    case class PrioStore(e: E, priority: Rational)
    val ordering = new Ordering[PrioStore]() {
      override def compare(x: PrioStore, y: PrioStore): Int = {
        val order = x.priority.compare(y.priority)
        if (order == 0)
          if (x.e < y.e) -1
          else if (y.e < x.e) 1
          else 0
        else
          order
      }
    }
    var queue = BoundedPriorityQueue[PrioStore](k)(ordering) // Algorithm stops when this is full

    def traverse(current: KDTree[E, R], d: Int): Unit = if (!current.isEmpty) {
      val dimension = if (d > prop.totalDimensions) 1 else d
      val v = current.value

      /* Insert the points into the queue and prioritize them according to their distance to p. When the queue is full,
       * every point with greater distance to p than the current greatest distance will not be inserted.
       */
      queue = queue.insert(PrioStore(current.value, prop.distance(v, p)))
      var left = false

      if (prop.value(p, dimension) < prop.value(v, dimension)) {
        left = true
        traverse(current.left, d + 1)
      }
      else
        traverse(current.right, d + 1)

      if (!queue.isFull || prop.axisDistance(v, p, dimension) <= (queue.max.priority / approximation)) {
        val oppositeSide = if (left) current.right else current.left
        traverse(oppositeSide, d + 1)
      }
    }

    traverse(this, 1)
    queue.values.map(_.e)
  }

  private def minimum(tree: KDTree[E, R], dimension: Int, depth: Int): KDTree[E, R] = if (tree.isEmpty) {
    throw new NoSuchElementException("Empty tree contains no minimum value.")
  } else {
    val currentDimension = (depth % prop.totalDimensions) + 1

    if (currentDimension == dimension) { // Check only left sub tree
      if (!tree.left.isEmpty)
        minimum(tree.left, dimension, depth + 1)
      else
        tree

    } else {
      // This plane was not split using the specified dimension, minimal value may be in both left and right sub tree
      val minLeft = if (tree.left.isEmpty) None else Some(minimum(tree.left, dimension, depth + 1))
      val minRight = if (tree.right.isEmpty) None else Some(minimum(tree.right, dimension, depth + 1))
      Seq(Some(tree), minLeft, minRight).filter(v => v.isDefined && !v.get.isEmpty)
        .minBy(n => prop.value(n.get.value, dimension)).get
    }
  }

  private def node(value: E): Option[KDTree[E, R]] = {
    def recFind(tree: KDTree[E, R], depth: Int): Option[KDTree[E, R]] = if (tree.isEmpty) {
      None
    } else {
      val dimension = (depth % prop.totalDimensions) + 1
      val soughtValue = prop.value(value, dimension)
      val treeValue = prop.value(tree.value, dimension)

      if (soughtValue < treeValue) {
        recFind(tree.left, depth + 1)
      } else if (soughtValue > treeValue) {
        recFind(tree.right, depth + 1)
      } else {

        if (tree.value == value) {
          Some(tree)
        } else {
          val leftResult = recFind(tree.left, depth + 1)

          if (leftResult.isDefined) {
            leftResult
          } else {
            val rightResult = recFind(tree.right, depth + 1)
            Option(rightResult.orNull)
          }
        }
      }
    }

    recFind(this, 0)
  }

}

class Branch[E : Ordering, R] private (val value: E, val left: KDTree[E, R], val right: KDTree[E, R], ep: ElementProperty[E], rp: RangeProperty[E, R]) extends KDTree[E, R](ep, rp) {
  override def isEmpty: Boolean = false

  override def equals(other: Any): Boolean = other match {
    case b: Branch[E, R] => b.value == value && b.right == right && b.left == left
    case _ => false
  }
  override def hashCode: Int = value.## ^ left.## ^ right.##

  override def toString: String = "Branch: " + value
}

object Branch {
  def apply[E : Ordering, R](v: E, l: KDTree[E, R], r: KDTree[E, R], ep: ElementProperty[E], rp: RangeProperty[E, R]): Branch[E, R] = new Branch(v, l, r, ep, rp)
}

case class Empty[E : Ordering, R](ep: ElementProperty[E], rp: RangeProperty[E, R]) extends KDTree[E, R](ep, rp) {
  override def value = error("No value specified for empty tree.")
  override def left = error("Attempted to retrieve left child of an empty tree.")
  override def right = error("Attempted to retrieve right child of an empty tree.")
  override def isEmpty = true
  private def error(str: String) = throw new Error(str)
}

/**
  * Factory object for new trees.
  */
object KDTree {

  /**
    * Creates an empty tree.
    * @param ep Element properties (see trait description).
    * @param rp Rage properties (see trait description).
    * @tparam E Element type in the tree.
    * @tparam R Range type in the tree.
    */
  def empty[E : Ordering, R](ep: ElementProperty[E], rp: RangeProperty[E, R]): KDTree[E, R] = Empty[E, R](ep, rp)

  /**
    * Builds a balanced tree from an initial set of elements.
    * @param elements Initial set of elements to put into the tree.
    * @param ep Element properties (see trait description).
    * @param rp Rage properties (see trait description).
    * @tparam E Element type in the tree.
    * @tparam R Range type in the tree.
    */
  def apply[E : Ordering, R](elements: Vector[E], ep: ElementProperty[E], rp: RangeProperty[E, R]): KDTree[E, R] = {
    if (elements.isEmpty)
      empty[E, R](ep, rp)
    else {
      val median = Median[E](elements, ep)
      kdTree(0, median, ep, rp)
    }

  }

  /**
    * @return A tree with pre-set properties for 2D integer points.
    */
  def point2DTree(elements: Vector[RPoint]): KDTree[RPoint, Rectangle] = {
    val elementAndRange = new Point2DProperty()
    apply(elements, elementAndRange, elementAndRange)
  }

  /**
    * @return A tree with pre-set properties for orthogonal rectangles.
    */
  def rectangleTree(elements: Vector[Rectangle]): KDTree[Rectangle, Rectangle] = {
    val elementAndRange = new RectangleProperty()
    apply(elements, elementAndRange, elementAndRange)
  }

  private def kdTree[E : Ordering, R](depth: Int, median: Median[E], ep: ElementProperty[E], rp: RangeProperty[E, R]): KDTree[E, R] = {
    val split = median.split((depth % ep.totalDimensions) + 1)
    val element = split._1
    val left = if (split._2.isEmpty) KDTree.empty[E, R](ep, rp) else kdTree(depth + 1, split._2, ep, rp)
    val right = if (split._3.isEmpty) KDTree.empty[E, R](ep, rp) else kdTree(depth + 1, split._3, ep, rp)
    Branch[E, R](element, left, right, ep, rp)
  }
}
