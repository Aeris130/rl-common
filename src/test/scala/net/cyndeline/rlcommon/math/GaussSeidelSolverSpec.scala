package net.cyndeline.rlcommon.math

import net.cyndeline.rlcommon.SpecImports

class GaussSeidelSolverSpec extends SpecImports {
  private val solver = new GaussSeidelSolver(100)

  private def solutionWithinMargin(s: Double, expected: Double): Unit = {
    assert(Math.abs(s - expected) <= solver.epsilon, s"$s did not match expected value $expected")
  }

  describe("GaussSeidelSolver") {

    // Careful to only use diagonally dominant variable constants
    // No 0's allowed in the diagonal!

    it ("should compute the value for a single variable") {

      Given("the equation 2x=3")
      val lhs = Vector.fill(1, 1)(2)
      val rhs = Vector(3d)

      When("computing a result")
      val result = solver.solve(lhs, rhs)

      Then("the result should be 1.5")
      solutionWithinMargin(result(0), 1.5)

    }

    it ("should compute the value for multiple variables") {

      Given("the equation 2x1 + x2 = 5")
      val lhs = Vector(Vector(2, 2), Vector(2, 2))
      val rhs = Vector(5d, 5d)

      When("computing a result")
      val result = solver.solve(lhs, rhs)

      Then("2x1 + x2 should be 5")
      solutionWithinMargin(2 * result(0) + result(1), 5)

    }

    it ("should compute the value for an equation with 0 on both sides") {

      Given("the equation 2x1 + 0x2 = 0 and 0x1 + 2x2 = 0")
      val lhs = Vector(Vector(2, 0), Vector(0, 2))
      val rhs = Vector(0d, 0d)

      When("computing a result")
      val result = solver.solve(lhs, rhs)

      Then("2x1 + 0x2 should be 0")
      solutionWithinMargin(2 * result(0) + 0 * result(1), 0)

      Then("0x1 + 2x2 should be 0")
      solutionWithinMargin(0 * result(0) + 2 * result(1), 0)

    }

    it ("should compute the value for multiple variables and multiple equations") {

      Given("the equations [1] 10x1 - x2 + 2x3 + 0x4 = 6, [2] -1x1 - 11x2 - x3 + 3x4 = 25, [3] 2x1 - 1x2 + 10x3 - x4 = 11, " +
        "[4] 0x1 + 3x2 - x3 + 8x4 = 15")
      val lhs = Vector(Vector(10, -1, 2, 0), Vector(-1, -11, -1, 3), Vector(2, -1, 10, -1), Vector(0, 3, -1, 8))
      val rhs = Vector(6d, 25d, -11d, 15d)

      When("computing a result")
      val result = solver.solve(lhs, rhs)

      Then("equation 1 should hold")
      solutionWithinMargin(lhs(0)(0) * result(0) + lhs(0)(1) * result(1) + lhs(0)(2) * result(2) + lhs(0)(3) * result(3), rhs(0))

      And("equation 2 should hold")
      solutionWithinMargin(lhs(1)(0) * result(0) + lhs(1)(1) * result(1) + lhs(1)(2) * result(2) + lhs(1)(3) * result(3), rhs(1))

      And("equation 3 should hold")
      solutionWithinMargin(lhs(2)(0) * result(0) + lhs(2)(1) * result(1) + lhs(2)(2) * result(2) + lhs(2)(3) * result(3), rhs(2))

      And("equation 4 should hold")
      solutionWithinMargin(lhs(3)(0) * result(0) + lhs(3)(1) * result(1) + lhs(3)(2) * result(2) + lhs(3)(3) * result(3), rhs(3))

    }

    it ("should throw an exception for variable matrices that are not diagonally dominant") {

      Given("a matrix of size two that is not diagonally dominant")
      val lhs = Vector(Vector(1, 2), Vector(2, 1))
      val rhs = Vector(5d, 5d)

      When("computing a result")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        solver.solve(lhs, rhs)
      }

    }

  }


}
