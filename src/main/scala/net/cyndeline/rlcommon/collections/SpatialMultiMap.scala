package net.cyndeline.rlcommon.collections

import net.cyndeline.rlcommon.math.geom.{Point, RPoint, Rectangle}
import net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree.KDTree
import spire.math.Rational

/**
  * Maps geometric shapes against user-defined objects and queries in O(n log n) time. Allows the user to query
  * for all results that lie within a geometric range rather than just the results that are mapped to a geometric
  * key.
  *
  * @param m Keeps track of how many modifications has been performed since the map last updated its internal
  *          data structures in order to optimize queries.
  * @param update How many modifications should be performed before internal data structures are optimized.
  */
class SpatialMultiMap[Shape, E, Range] private(kdTree: KDTree[Shape, Range], eMap: Map[Shape, Set[E]], m: Int, update: Option[Int]) {

  /**
    * @return True if no shape key is mapped to a value, otherwise false.
    */
  def isEmpty: Boolean = kdTree.isEmpty && eMap.isEmpty

  /**
    * @return The number of keys in the map.
    */
  def keySize: Int = eMap.size

  /**
    * @param key A key shape.
    * @return True if at least one entry uses the specified key, otherwise false.
    */
  def contains(key: Shape): Boolean = kdTree.contains(key)

  def + (kv: (Shape, E)*): SpatialMultiMap[Shape, E, Range] = {
    var current = this
    val it = kv.iterator
    while (it.hasNext) {
      val entry = it.next()
      current = current + (entry._1, entry._2)
    }
    current
  }
  def + (key: Shape, value: E): SpatialMultiMap[Shape, E, Range] = addValues(key, Set(value))

  def - (kv: (Shape, E)*): SpatialMultiMap[Shape, E, Range] = {
    var current = this
    val it = kv.iterator
    while (it.hasNext) {
      val entry = it.next()
      current = current - (entry._1, entry._2)
    }
    current
  }
  def - (key: Shape, value: E): SpatialMultiMap[Shape, E, Range] = {
    if (kdTree.contains(key)) {
      val values = eMap(key)
      if (!values.contains(value))
        throw new NoSuchElementException("Could not delete value " + value + ", its key " + key + " was found, but not the value itself.")

      val valueRemoved = values - value

      // Don't actually delete the shape if it maps to multiple values
      if (valueRemoved.nonEmpty)
        createMap(kdTree, eMap + (key -> valueRemoved), false)
      else
        createMap(kdTree.delete(key), eMap - key, true)

    } else {
      throw new NoSuchElementException("The key-value pair " + key + ", " + value + " was missing and could not be removed.")
    }
  }

  /**
    * @param key A map key.
    * @return Every element mapped against the key.
    */
  def get(key: Shape): Option[Set[E]] = if (kdTree.contains(key)) {
    eMap.get(key)
  } else {
    None
  }

  /**
    * @param range A key range.
    * @return Every element mapped to a key within the range.
    */
  def getRange(range: Range): Set[E] = {
    val rangeIntersection = kdTree.rangeSearch(range)

    if (rangeIntersection.nonEmpty)
      rangeIntersection.toSet.flatMap(eMap)
    else
      Set()
  }

  /** @return Every value set in the map. */
  def values: Iterable[Set[E]] = eMap.values

  /** @return Every key in the map. */
  def keys: Iterable[Shape] = eMap.keys

  /** @return Every (key, value-set) pair in the map. */
  def iterator: Iterator[(Shape, Set[E])] = eMap.iterator

  private def addValues(key: Shape, values: Set[E]) = {
    if (kdTree.contains(key)) {
      val currentValues = eMap.getOrElse(key, Set())
      createMap(kdTree, eMap + (key -> (currentValues ++ values)), false)

    } else {
      createMap(kdTree.insert(key), eMap + (key -> values), true)
    }
  }

  private def createMap(tree: KDTree[Shape, Range], map: Map[Shape, Set[E]], treeUpdate: Boolean) = {
    var updateCount = if (treeUpdate && update.isDefined) m + 1 else m

    val updatedTree = if (update.isDefined && updateCount >= update.get) {
      updateCount = 0
      tree.balance
    } else {
      tree
    }
    new SpatialMultiMap(updatedTree, map, updateCount, update)
  }

  override def toString: String = "SMultiMap: " + eMap
  override def equals(other: Any): Boolean = other match {
    case smm: SpatialMultiMap[E, Shape, Range] =>
      val thisIt = iterator
      val otherIt = smm.iterator
      while (thisIt.hasNext && otherIt.hasNext) {
        val a = thisIt.next()
        val b = otherIt.next()
        if (a != b) return false
      }
      thisIt.isEmpty && otherIt.isEmpty
    case _ => false
  }
  override def hashCode: Int = kdTree.## ^ eMap.## ^ m.## ^ update.##

}

/**
  * Factory object for different map types.
  */
object SpatialMultiMap {

  /**
    * @param updateCount The number of key modifications to the map that should occur before the maps underlying data
    *                    structures are updated to preserve lookup and modification speed.
    * @tparam E Value type in the map.
    * @return A map where the keys are 2D points in the cartesian plane, and ranges are represented as rectangles.
    */
  def withPoint2D[E](updateCount: Int = 0): SpatialMultiMap[RPoint, E, Rectangle] = {
    val updateAt = if (updateCount > 0) Some(updateCount) else None
    new SpatialMultiMap(KDTree.point2DTree(Vector()), Map[RPoint, Set[E]](), 0, updateAt)
  }

  /**
    * @param updateCount The number of key modifications to the map that should occur before the maps underlying data
    *                    structures are updated to preserve lookup and modification speed.
    * @tparam E Value type in the map.
    * @return A map where the keys are 2D rectangles in the cartesian plane, and ranges are represented as rectangles.
    */
  def withRectangles[E](updateCount: Int = 0): SpatialMultiMap[Rectangle, E, Rectangle] = {
    val updateAt = if (updateCount > 0) Some(updateCount) else None
    new SpatialMultiMap(KDTree.rectangleTree(Vector()), Map[Rectangle, Set[E]](), 0, updateAt)
  }

}
