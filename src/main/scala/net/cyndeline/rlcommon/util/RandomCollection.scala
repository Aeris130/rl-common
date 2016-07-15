package net.cyndeline.rlcommon.util

import scala.util.Random


/**
 * Returns elements randomly and maps weights towards objects, making it possible to increase the rate at which some
 * objects are returned compared to others.
 *
 * To make an item more likely to be returned, assign it a greater weight. The odds of a single element being returned
 * is the weight of the element divided by the sum of all weights.
 *
 * @tparam E The type of object to assign weights to.
 */
trait RandomCollection[E] {

  /**
   * Add an item and assign a weight to it.
   *
   * @param weight Weight to assign to the item. Must be higher than 0.
   * @param element Item to map height to.
   */
  def add(weight: Int, element: E): RandomCollection[E]

  /**
   * Adds elements and weights of another collection to this one.
   * @param collection Collection to add.
   */
  def addCollection(collection: RandomCollection[E]): RandomCollection[E]

  /**
   * Removes an element from the collection.
   * @param element Element to remove.
   */
  def remove(element: E): RandomCollection[E]

  /**
   * Fetches an item from the collection at random. Items with higher
   * weight will be more likely to be returned.
   *
   * @param r Random object used in selection.
   * @return A random item with weighted probability that skews towards greater weights.
   */
  def next(r: Random): E

  /**
   * @return the size of the collection.
   */
  def size: Int

  /**
   * @return True if the collection contains no elements, otherwise false.
   */
  def isEmpty: Boolean

  /**
   * Returns each element stored without weights.
   * @return A list containing one copy of each element in no particular order.
   */
  def allElements: Vector[E]

  /**
   * Iteratoes over every weight and element in the collection.
   * @return An iterator over the collection.
   */
  def iterator: Iterator[(Int, E)]

}
