package net.cyndeline.rlcommon.math

import net.cyndeline.rlcommon.collections.{SparseArray, SparseArrayWrapper}

/**
  * Iterative algorithm to solve the equation Ax=b, where A is a diagonally dominant nxn matrix.
  *
  * Ported from java at: http://www.sanfoundry.com/java-program-implement-gauss-seidel-method/
  */
class GaussSeidelSolver(iterations: Int) {
  require(iterations > 0, "Number of iterations must be > 0.")

  // Decides how close each equations approximation should converge towards a solution before the algorithm terminates.
  val epsilon = 1e-10

  /**
    * Solves a series of linear equations.
    * @param variables An NxN matrix, where V(1)(2) contains the constant of the third variable in the second equation.
    * @param constant The right-hand-side of the equation.
    * @return Given N variables, a vector of size N containing the solution for each variable.
    */
  def solve(variables: Vector[Vector[Int]], constant: Vector[Double]): Vector[Double] = {
    solve(variables.map(_.toArray).toArray, constant.toArray)
  }

  /**
    * Solves a series of linear equations.
    * @param variables An NxN matrix, where V(1)(2) contains the constant of the third variable in the second equation.
    * @param constant The right-hand-side of the equation.
    * @return Given N variables, a vector of size N containing the solution for each variable.
    */
  def solve(variables: Array[Array[Int]], constant: Array[Double]): Vector[Double] = {
    val n = variables.length
    val sparse = Array.fill[SparseArray[Int]](n)(null)
    var i = 0
    while (i < n) {
      sparse(i) = new SparseArrayWrapper(variables(i))
      i += 1
    }
    solve(new SparseArrayWrapper(sparse), constant)
  }

  /**
    * Solves a series of linear equations.
    * @param variables An NxN matrix, where V(1)(2) contains the constant of the third variable in the second equation.
    * @param constant The right-hand-side of the equation.
    * @return Given N variables, a vector of size N containing the solution for each variable.
    */
  def solve(variables: SparseArray[SparseArray[Int]], constant: Array[Double]): Vector[Double] = {
    checkDiagonalDominate(variables)
    val n = variables.length
    val approximations = Array.fill(n)(0d)
    var previous = Array.fill(n)(0d)

    /**
      * Takes the target constant for all equations (0 -> (n-1)) and sums the values found so far into the
      * approximation for that equation. Each equation E's approximation is updated before (E+1) starts,
      * causing it to converge towards the result faster due to being able to use E's updated approximation
      * rather than the one found during the last iteration.
      */
    def updateApproximations(): Unit = {
      var i = 0
      while (i < n) {
        var sum = constant(i)

        var j = 0
        while (j < n) {
          if (j != i)
            sum -= variables(i)(j) * approximations(j)
          j += 1
        }

        // Update approximation for equation (i) before starting on i+1.
        approximations(i) = (1d / variables(i)(i)) * sum
        i += 1
      }
    }

    /**
      * Checks if every current approximation lies within the epsilon margin of the previous approximation.
      * @return False if every equations current approximation lies within the margin, otherwise true.
      */
    def nonConvergingEntryExists: Boolean = {
      var i = 0
      while (i < n) {
        if (Math.abs(approximations(i) - previous(i)) > epsilon)
          return true
        i += 1
      }
      false
    }

    var currentIteration = 0
    var resultFound = false

    // Main algorithm
    do {
      updateApproximations()

      if (iterations >= 1) {
        if (nonConvergingEntryExists) {
          previous = approximations.clone()
          currentIteration += 1
        } else {
          resultFound = true
        }
      }

    } while (!resultFound && currentIteration < iterations)

    approximations.toVector
  }

  private def checkDiagonalDominate(a: SparseArray[SparseArray[Int]]) = {
    val n = a.length
    var i = 0
    while (i < n) {
      var j = 0
      var sum = 0
      while (j < n) {
        if (i != j)
          sum += a(i)(j)
        j += 1
      }
      require(Math.abs(a(i)(i)) >= sum, "Variable matrix was not diagonally dominant.")
      i += 1
    }
  }

}
