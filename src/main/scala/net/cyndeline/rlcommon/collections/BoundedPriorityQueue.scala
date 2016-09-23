package net.cyndeline.rlcommon.collections

import Ordering.Implicits._

/**
  * A priority queue that has an upper bound on the number of elements it can store. Once the bound is reached, any
  * element greater than the current greatest element will be discarded on insertion. Any element smaller than the
  * current greatest element will cause the greatest element to be removed from the queue upon insertion.
  *
  * @param bound The maximum number of elements the queue can contain.
  * @param size The current amount of elements in the queue.
  * @tparam E Element type in the queue. Its ordering decides the priority.
  */
class BoundedPriorityQueue[E : Ordering] private (val bound: Int, val size: Int, elements: PriorityQueue[E], greatest: Option[E]) {

  /** @return True if there are no elements in the queue, otherwise false. */
  def isEmpty: Boolean = size <= 0

  /**
    * Inserts a value into the queue if the queue is non-full or the new value is less than some current value
    * in the queue.
    * @param e Value to insert.
    * @return A copy of this queue with the value inserted if it has less priority than some value in the queue, or
    *         if the queue's size hasn't reached the bound.
    */
  def insert(e: E, es: E*): BoundedPriorityQueue[E] = {
    var current = this
    val elements = (Seq(e) ++ es).toIterator
    while (elements.hasNext) {
      val element = elements.next()
      current = current.ins(element)
    }
    current
  }

  /** Takes O(n) time.
    * @return Every value in the queue.
    */
  def values: Vector[E] = elements.values

  /** @return True if the queue cannot add additional elements without discarding some, otherwise false. */
  def isFull: Boolean = size >= bound

  /**
    * @return The element in the queue with the highest priority (i.e the next element to be removed if the queue is
    *         full and an element with lower priority is added.
    */
  def max: E = greatest.getOrElse(throw new Error("Cannot retrieve greatest element from empty queue."))

  private def ins(e: E): BoundedPriorityQueue[E] = {
    if (!isFull) {
      val newElements = elements.insert(e)
      val newHighest = if (greatest.isEmpty || greatest.get < e) Some(e) else greatest
      new BoundedPriorityQueue(bound, size + 1, newElements, newHighest)

    } else if (greatest.get > e) {
      val newElements = elements.delete(greatest.get).insert(e)
      val newHighest = Some(newElements.max)
      new BoundedPriorityQueue(bound, size, newElements, newHighest)

    } else {
      this
    }
  }

}

object BoundedPriorityQueue {

  /**
    * @param bound The maximum number of elements the queue can contain. Must be > 0.
    * @tparam E Value type in the queue.
    * @return An empty priority queue using the bound.
    */
  def apply[E : Ordering](bound: Int): BoundedPriorityQueue[E] = {
    require(bound > 0, "Priority queue bound must be > 0.")
    new BoundedPriorityQueue[E](bound, 0, new PriorityQueue[E](), None)
  }

  /**
    *
    * @param bound The maximum number of elements the queue can contain. Must be > 0.
    * @param elements The elements to insert in the queue.
    * @tparam E Value type in the queue.
    * @return A priority queue using the bound, containing the elements.
    */
  def apply[E : Ordering](bound: Int, elements: E*): BoundedPriorityQueue[E] = {
    val queue = apply[E](bound)
    if (elements.isEmpty) {
      queue
    } else {
      queue.insert(elements.head, elements.drop(1):_*)
    }
  }
}
