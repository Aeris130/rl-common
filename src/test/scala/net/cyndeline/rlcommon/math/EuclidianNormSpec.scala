package net.cyndeline.rlcommon.math

import net.cyndeline.rlcommon.SpecImports

class EuclidianNormSpec extends SpecImports {
  private val eps = 1.0E-6

  describe("EuclidianNorm") {

    it ("should compute the norm of a vector with size 3") {

      Given("the vector (1, 3, 7)")
      val v = Vector(1d, 3d, 7d)

      When("computing the norm")
      val norm = EuclidianNorm(v)

      Then("the norm should be sqrt(59)")
      assert(norm - Math.sqrt(59) < eps)

    }

  }
}
