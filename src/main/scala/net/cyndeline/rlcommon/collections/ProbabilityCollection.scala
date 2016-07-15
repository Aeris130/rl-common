package net.cyndeline.rlcommon.collections

import net.cyndeline.rlcommon.util.RandomCollection

import scala.collection.immutable.TreeMap
import scala.util.Random

/**
 * Maps weights towards objects, making it possible to increase the rate at which some objects are
 * returned compared to others.
 *
 * To make an item more likely to be returned, assign it a greater height. The probability of a single element
 * to be selected is the elements weight divided by the total weight in the collection.
 *
 * Source: http://stackoverflow.com/a/6409791
 *
 * @constructor Creates a new probability collection.
 * @tparam E The type of object to assign weights to.
 */
class ProbabilityCollection[E] private (val map: TreeMap[Int, E], originalWeight: Map[E, Int], summedWeight: Map[E, Int], totalWeight: Int) extends RandomCollection[E] {

  /**
   * Creates an empty probability collection.
   */
  def this() = this(TreeMap[Int, E](), Map[E, Int](), Map[E, Int](), 0)

  /**
   * Add an item and assign a weight to it.
   *
   * @param weight Weight to assign to the item. Must be higher than 0.
   * @param element Item to map height to.
   */
  override def add(weight: Int, element: E): ProbabilityCollection[E] = {
    require(!originalWeight.contains(element), s"The element $element already has an associated weight.")
    require(weight > 0, s"Only positive probability weights can be used for RandomCollections, element $element has weight $weight")
    val newTotalWeight = totalWeight + weight
    new ProbabilityCollection(map + (newTotalWeight -> element), originalWeight + (element -> weight), summedWeight + (element -> newTotalWeight), newTotalWeight)
  }

  /**
   * Adds elements and weights of another collection to this one.
   * @param collection Collection to add.
   */
  override def addCollection(collection: RandomCollection[E]): ProbabilityCollection[E] = {
    var current = this
    val elements = collection.iterator
    while (elements.hasNext) {
      val e: (Int, E) = elements.next()
      current = current.add(e._1, e._2)
    }
    current
  }

  /**
   * Removes an element from the collection by creating a new map from the set of old values
   * minus the one to be removed.
   * @param element Element to remove.
   */
  override def remove(element: E): ProbabilityCollection[E] = if (originalWeight.contains(element)) {
    val weightToSubtract = originalWeight(element)
    val eWeight = summedWeight(element)

    def updateRefWeight(e: E, w: Int): Int = if (w > eWeight) w - weightToSubtract else w

    /* Since every weight is greater than 0, it means that every element added after x gets the weight of x added
     * to its own weight, and will thus be greater. Any element added before x must have a lower weight.By subtracting
     * the weight of x from all elements with weight greater than x, its affect on the collection is nullified.
     */
    val updatedTree = (map - summedWeight(element))
      .map(entry => (updateRefWeight(entry._2, entry._1), entry._2))
    val updatedSummedWeights = (summedWeight - element)
      .map(entry => (entry._1, updateRefWeight(entry._1, entry._2)))

    new ProbabilityCollection(updatedTree, originalWeight - element, updatedSummedWeights, totalWeight - weightToSubtract)
  } else {
    this
  }

  /**
   * Fetches an item from the collection at random. items with higher
   * weight will be more likely to be returned.
   *
   * @return A random item with weighted probability that skews towards
   * 			greater weights.
   */
  override def next(r: Random): E = {
    if (map.isEmpty)
      throw new NoSuchElementException("next on an empty random collection")

    val value = Math.ceil(r.nextDouble() * totalWeight).toInt
    map.from(value).headOption.map(_._2).getOrElse(map.maxBy(_._1)._2)
  }

  /**
   * @return the size of the collection.
   */
  override def size: Int = map.size

  /**
   * @return True if the collection contains no elements, otherwise false.
   */
  override def isEmpty: Boolean = size == 0

  /**
   * Returns each element stored without weights.
   * @return A list containing one copy of each element in no
   * 			particular order.
   */
  override def allElements: Vector[E] = map.values.toVector

  /**
   * Iterates over every weight and element in the collection.
   * @return An iterator over the collection.
   */
  override def iterator: Iterator[(Int, E)] = originalWeight.map(_.swap).iterator

  /**
   * Used for testing to inspect values inside the tree map.
   * @return Returns a list of tuples where the weight is the combined weight as found in the map. Example: Three
   *         elements with weighted 1, 2 and 3 will have weights 1, 3 and 6 inside the map.
   */
  def combinedWeights: Vector[(Int, E)] = summedWeight.map(_.swap).iterator.toVector.sortBy(_._1)

  override def toString: String = s"Probability collection: ${iterator.mkString(", ")}"
  override def hashCode: Int = map.## ^ summedWeight.## ^ originalWeight.##
  override def equals(other: Any): Boolean = other match {
    case pc: ProbabilityCollection[E] => pc.map == map
    case _ => false
  }

}

object ProbabilityCollection {

  /**
    * Creates a new collection with an initial set of elements and probabilities.
    * @param values All initial elements and probability weights.
    */
  def from[E](values: (Int, E)*): ProbabilityCollection[E] = {
    val allvalues = values.map(_._2)
    require(allvalues.distinct.size == allvalues.size, "Duplicate values found in probability collection.")

    var pc = new ProbabilityCollection[E]()
    for (v <- values)
      pc = pc.add(v._1, v._2)

    pc
  }
}
