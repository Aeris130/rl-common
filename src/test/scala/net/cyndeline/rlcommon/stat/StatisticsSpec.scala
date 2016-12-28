package net.cyndeline.rlcommon.stat

import net.cyndeline.rlcommon.SpecImports

class StatisticsSpec extends SpecImports {

  private val eps = 1e-6

  describe("Statistics") {

    it ("should compute the mean") {

      Given("values 0 to 10")
      val values = Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      When("computing the mean")
      val mean = Statistics.mean(values)

      Then("the mean should be 5")
      mean should be (5.0)

    }

    it ("should compute the variance") {

      Given("values 3, 4, 4, 5, 6, 8")
      val values = Vector(3, 4, 4, 5, 6, 8)

      When("computing the variance")
      val variance = Statistics.variance(values)

      Then("the variance should be 8/3 (2.666...)")
      assert(variance - (8.0/3) < eps)

    }

    it ("should compute the standard variation") {

      Given("values with a variance")
      val values = Vector(1, 2, 3, 4, 5)
      val variance = Statistics.variance(values)

      When("computing the standard deviation")
      val std = Statistics.stdDeviation(values)

      Then("the deviation should be the square root of the variance")
      std should equal (Math.sqrt(variance.toDouble))

    }

  }

}
