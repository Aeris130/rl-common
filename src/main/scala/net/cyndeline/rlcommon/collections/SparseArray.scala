package net.cyndeline.rlcommon.collections

import scala.collection.mutable

/**
  * Stores its content using key-value pairs rather than vectors, and returns a default value for any index that
  * doesn't correspond to a key. Mutable.
  */
trait SparseArray[T] {

  def apply(index: Int): T

  def set(index: Int, value: T): Unit

  def length: Int

  /** @return True if every value in the array is the default, otherwise false.*/
  def isDefault: Boolean

  def isEmpty: Boolean
  final def nonEmpty: Boolean = !isEmpty

}

class SparseKeyArray[T] private (val length: Int, default: T, map: mutable.HashMap[Int, T]) extends SparseArray[T] {

  def this(length: Int, default: T) = this(length, default, new mutable.HashMap[Int, T]())

  override def apply(index: Int): T = map.getOrElse(index, default)
  override def set(index: Int, value: T): Unit = {
    validateIndex(index)
    map.put(index, value)
  }
  override def isDefault: Boolean = map.isEmpty
  override def isEmpty: Boolean = length == 0

  private def validateIndex(index: Int): Unit = {
    if (index < 0 || index >= length)
      throw new IndexOutOfBoundsException(index.toString)
  }
}

class SparseArrayWrapper[T](array: Array[T]) extends SparseArray[T] {
  override def apply(index: Int): T = array(index)
  override def set(index: Int, value: T): Unit = array(index) = value
  override def length: Int = array.length
  override def isDefault: Boolean = false
  override def isEmpty: Boolean = array.isEmpty
}
