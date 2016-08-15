package net.cyndeline.rlcommon.math

import net.cyndeline.rlcommon.SpecImports

class RationalSpec extends SpecImports {

  describe("Rational") {

    it ("should use the lowest possible numerator/denominator") {

      Given("the num/denom 21/28")
      val num = 21
      val denom = 28

      When("constructing the rational")
      val rational = Rational(num, denom)

      Then("the num/denom should be 3/4")
      rational.numer should be (3)
      rational.denom should be (4)

    }

    it ("should compute its negative") {

      Given("a rational 2/3")
      val rational = Rational(2, 3)

      When("computing its negative")
      val nr = -rational

      Then("the result should be -2/3")
      nr.numer should be (-2)
      nr.denom should be (3)

    }

    it ("should add") {

      Given("the rational 2/3")
      val rational = Rational(2, 3)

      When("adding 5/7")
      val added = rational + Rational(5, 7)

      Then("the result should be 29/21")
      added should be (Rational(29, 21))

    }

    it ("should subtract") {

      Given("the rational 3/4")
      val rational = Rational(3, 4)

      When("subtracting 1/2")
      val subtracted = rational - Rational(1, 2)

      Then("the result should be 1/4")
      subtracted should be (Rational(1, 4))

    }

    it ("should multiply") {

      Given("the rationals 23/21 and 7/11")
      val r1 = Rational(23, 21)
      val r2 = Rational(7, 11)

      When("multiplying them")
      val multiplied = r1 * r2

      Then("the result should be 161/231")
      multiplied should be (Rational(161, 231))

    }

    it ("should divide") {

      Given("the rationals 3/4 and 2/5")
      val r1 = Rational(3, 4)
      val r2 = Rational(2, 5)

      When("dividing them")
      val multiplied = r1 / r2

      Then("the result should be 8/15")
      multiplied should be (Rational(15, 8))

    }

    it ("should order rationals") {

      Given("the rationals 1/2 and 44/-3")
      val r1 = Rational(1, 2)
      val r2 = Rational(44, -3)

      When("checking order")
      val c1 = r1 < r2
      val c2 = r2 < r1

      Then("r2 should be less than r1")
      c1 should be (false)
      c2 should be (true)

    }

  }



}
