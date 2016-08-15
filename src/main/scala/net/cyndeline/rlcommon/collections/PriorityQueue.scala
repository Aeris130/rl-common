package net.cyndeline.rlcommon.collections

import scala.collection.mutable.ArrayBuffer

/**
  * Immutable priority queue implemented using red-black trees, giving it O(log n) performance on its operations.
  * Priority in the queue is handled by the Ordering supplied by the user (implicitly if none is specified explicitly).
  */
class PriorityQueue[E] private (private val rbTree: RBTree[E])(implicit ord: Ordering[E]) {

  /**
    * @param ord An ordering that determines the priority of all elements in the queue. Given elements A and B, A has
    *            higher priority if this ordering considers A > B.
    */
  def this()(implicit ord: Ordering[E]) = this(RBTree.empty[E])

  /**
    * @param e Element to insert into the queue.
    * @return An instance of the priority queue with the element inserted.
    */
  def insert(e: E, es: E*): PriorityQueue[E] = {
    var t = rbTree.insert(e)
    for (element <- es)
      t = t.insert(element)
    make(t)
  }

  /**
    * Retrieves the element with the highest priority without removing it from the queue.
    *
    * @return The element with the highest priority.
    */
  def peek: E = rbTree.max

  /**
    * Retrieves and removes the element with the highest priority.
    *
    * @return The element with the highest priority and an instance of the queue with the element removed.
    */
  def poll: (E, PriorityQueue[E]) = {
    val max = rbTree.max
    (max, new PriorityQueue(rbTree.delete(max)))
  }

  /**
    * Deletes an entry from the queue.
    *
    * @param e Entry to delete.
    * @return An instance of the priority queue with the element deleted.
    */
  def delete(e: E): PriorityQueue[E] = make(rbTree.delete(e))

  /** @return True if there are no elements in the queue, otherwise false. */
  def isEmpty: Boolean = rbTree.isEmpty
  def nonEmpty: Boolean = !isEmpty

  /** Collects all elements in this queue in O(n) time. */
  def values: Vector[E] = {
    val elements = new ArrayBuffer[E]()
    def revInorder(t: RBTree[E]): Unit = if (!t.isEmpty) {
      revInorder(t.right)
      elements += t.value
      revInorder(t.left)
    }
    revInorder(rbTree)
    elements.toVector
  }

  private def make(tree: RBTree[E]): PriorityQueue[E] = new PriorityQueue(tree)

  override def toString: String = if (isEmpty) {
    "Empty Priority Queue"
  } else {
    val nl = System.getProperty("line.separator")
    "Priority Queue: [" ++ values.mkString(s", $nl") + "]"
  }

}
