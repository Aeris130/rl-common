package net.cyndeline.rlcommon.sorting

/**
  * Heapsort, modified to allow arbitrary data objects using an implicit ordering.
  */
object HeapSort {

  /**
    * Uses the heapsort algorithm to sort a vector of elements.
    * @param elements Elements to sort.
    * @param ordering Determines gt/lt ordering for the element data type.
    * @tparam E Element data type.
    * @return A vector of all elements, sorted according to the ordering.
    */
  def apply[E](elements: Vector[E])(implicit ordering: Ordering[E]): Vector[E] = {
    val values: Array[Int] = elements.zipWithIndex.map(_._2).toArray
    val sorted = heapSort(values, elements, ordering)
    for (s <- sorted.toVector) yield elements(s)
  }

  private def heapSort[E](arr: Array[Int], original: Vector[E], order: Ordering[E]) : Array[Int] = {
    buildHeap(arr, original, order)
    (arr.length-1 until 0 by -1).foreach( i => {
      swap(arr, 0, i)
      heapify(arr, 0, i, original, order)
    })
    arr
  }

  private def buildHeap[E](arr: Array[Int], original: Vector[E], order: Ordering[E]) {
    ((arr.length/2.0D).floor.toInt-1 until -1 by -1).foreach( i => heapify(arr, i, arr.length, original, order) )
  }

  private def heapify[E](arr: Array[Int], idx: Int, max: Int, original: Vector[E], order: Ordering[E]) {
    val l = left(idx)
    val r = right(idx)
    var largest = if (l < max && order.gt(original(arr(l)), original(arr(idx)))) l else idx
    largest = if (r < max && order.gt(original(arr(r)), original(arr(largest)))) r else largest
    if (largest != idx) {
      swap(arr, idx, largest)
      heapify(arr, largest, max, original, order)
    }
  }

  private def left(idx: Int): Int = 2*idx+1
  private def right(idx: Int): Int = (2*idx)+2

  def swap(s: Array[Int], i: Int, j: Int): Unit = {
    val v = s(i)
    s(i) = s(j)
    s(j) = v
  }
}
