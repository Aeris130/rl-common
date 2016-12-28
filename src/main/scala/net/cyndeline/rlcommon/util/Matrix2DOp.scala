package net.cyndeline.rlcommon.util

/**
  * Operations on 2D matrix elements.
  */
object Matrix2DOp {

  /**
    * @param matrix A matrix.
    * @param f A function that takes a matrix value as input along with the (x,y) coordinate of the value, and produces
    *          another value as output. Will be called once for every (x,y) index in the matrix.
    * @tparam E Value type in the matrix.
    */
  def modify[E](matrix: Array[Array[E]], f: (E, Int, Int) => E): Unit = {
    if (matrix.length == 0)
      return

    val w = matrix.length
    val h = matrix(0).length
    var i = 0
    while (i < w) {
      var j = 0
      while (j < h) {
        matrix(i)(j) = f(matrix(i)(j), i, j)
        j += 1
      }
      i += 1
    }

  }

  /**
    * Modifies a matrix by mapping a function onto every value.
    * @param matrix A matrix.
    * @param f Function that takes a value and returns another value.
    * @tparam E Value type.
    */
  def modify[E](matrix: Array[Array[E]], f: E => E): Unit = {
    def fun(e: E, x: Int, y: Int) = f(e)
    modify(matrix, fun _)
  }


}
