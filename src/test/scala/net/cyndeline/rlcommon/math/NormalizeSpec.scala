package net.cyndeline.rlcommon.math

import net.cyndeline.rlcommon.SpecImports

class NormalizeSpec extends SpecImports {

  describe("Normalize") {

    it ("should normalize a value to the range [0,1]") {

      Given("the integer 3 with max = 10 and min = 0")
      val i = 3
      val max = 10
      val min = 0

      When("normalizing the value")
      val normalized = Normalize(i, min, max)

      Then("the normalized value should be 0.3")
      normalized should be (0.3)

    }

    it ("should normalize the value 0 to 0") {

      Given("the integer 0 with max = 0 and min = 0")
      val i = 0
      val max = 0
      val min = 0

      When("normalizing the value")
      val normalized = Normalize(i, min, max)

      Then("the normalized value should be 0")
      normalized should be (0)

    }

    it ("should throw an exception when normalizing a value outside of its min/max bound") {

      Given("the integer 5/-1 with max = 4 and min = 1")
      val i1 = 5
      val i2 = -1
      val max = 4
      val min = 1

      When("normalizing the values")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        Normalize(i1, min, max)
      }
      intercept[IllegalArgumentException] {
        Normalize(i2, min, max)
      }

    }

  }


}
