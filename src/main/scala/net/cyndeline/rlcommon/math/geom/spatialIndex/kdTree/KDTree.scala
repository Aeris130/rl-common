package net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree

import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}

/**
  * A spatial index allowing for log(n) inserts, deletions and lookups, given that the tree is balanced. Creating
  * a new tree takes O(kn log n) time, where k is the number of dimensions in the data set. Balancing the tree is
  * done in O(kn log n) time using the median algorithm by Russel A. Brown.
  */
abstract class KDTree[E, R](prop: ElementProperty[E], rangeProp: RangeProperty[E, R]) {

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
      else
        tree // Duplicate, no need to add additional entries
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
        val newLeft = if (eToDeleteValue < eValue) del(e, tree.left, nextDepth) else tree.left
        val newRight = if (eToDeleteValue > eValue) del(e, tree.right, nextDepth) else tree.right
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
        Some(tree) // Success
      }
    }

    recFind(this, 0)
  }

}

class Branch[E, R] private (val value: E, val left: KDTree[E, R], val right: KDTree[E, R], ep: ElementProperty[E], rp: RangeProperty[E, R]) extends KDTree[E, R](ep, rp) {
  override def isEmpty: Boolean = false

  override def equals(other: Any): Boolean = other match {
    case b: Branch[E, R] => b.value == value && b.right == right && b.left == left
    case _ => false
  }
  override def hashCode: Int = value.## ^ left.## ^ right.##

  override def toString: String = "Branch: " + value
}

object Branch {
  def apply[E, R](v: E, l: KDTree[E, R], r: KDTree[E, R], ep: ElementProperty[E], rp: RangeProperty[E, R]): Branch[E, R] = new Branch(v, l, r, ep, rp)
}

case class Empty[E, R](ep: ElementProperty[E], rp: RangeProperty[E, R]) extends KDTree[E, R](ep, rp) {
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
  def empty[E, R](ep: ElementProperty[E], rp: RangeProperty[E, R]): KDTree[E, R] = Empty[E, R](ep, rp)

  /**
    * Builds a balanced tree from an initial set of elements.
    * @param elements Initial set of elements to put into the tree.
    * @param ep Element properties (see trait description).
    * @param rp Rage properties (see trait description).
    * @tparam E Element type in the tree.
    * @tparam R Range type in the tree.
    */
  def apply[E, R](elements: Vector[E], ep: ElementProperty[E], rp: RangeProperty[E, R]): KDTree[E, R] = {
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
  def point2DTree(elements: Vector[Point]): KDTree[Point, Rectangle] = {
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

  private def kdTree[E, R](depth: Int, median: Median[E], ep: ElementProperty[E], rp: RangeProperty[E, R]): KDTree[E, R] = {
    val split = median.split((depth % ep.totalDimensions) + 1)
    val element = split._1
    val left = if (split._2.isEmpty) KDTree.empty[E, R](ep, rp) else kdTree(depth + 1, split._2, ep, rp)
    val right = if (split._3.isEmpty) KDTree.empty[E, R](ep, rp) else kdTree(depth + 1, split._3, ep, rp)
    Branch[E, R](element, left, right, ep, rp)
  }
}