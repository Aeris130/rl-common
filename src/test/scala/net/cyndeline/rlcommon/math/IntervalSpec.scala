package net.cyndeline.rlcommon.math

import net.cyndeline.rlcommon.SpecImports

class IntervalSpec extends SpecImports {

  describe("Interval") {

    /*
     * Addition
     */

    it ("should add a partially overlapping interval") {

      Given("the interval 2 to 6")
      val interval = Interval(2, 6)

      When("adding 4 to 7")
      val added = interval + Interval(4, 7)

      Then("the result should be [2, 7]")
      added should be (Interval(2, 7))

    }

    it ("should an entirely overlapping interval") {

      Given("the interval 2 to 6")
      val interval = Interval(2, 6)

      When("adding -3 to 8")
      val added = interval + Interval(-3, 8)

      Then("the result should be [-3, 8]")
      added should be (Interval(-3, 8))

    }

    it ("should throw an exception when adding disjoint intervals") {

      Given("the interval 2 to 6")
      val interval = Interval(2, 6)

      When("adding -3 to 1")
      Then("an error should be thrown")
      intercept[IllegalArgumentException] {
        interval + Interval(-3, 1)
      }

    }

    it ("should add an interval to an empty interval") {

      Given("an empty interval")
      val empty = Interval.empty[Int]

      When("adding 2 to 6")
      val toAdd = Interval(2, 6)
      val added = empty + toAdd

      Then("the result should be 2 to 6")
      added should be (toAdd)

    }

    /*
     * Subtraction
     */

    it ("should do nothing when subtracting a disjoint interval") {

      Given("the interval 2 to 6")
      val interval = Interval(2, 6)

      When("subtracting -3 to 1")
      val subtracted = interval - Interval(-3, 1)

      Then("the result should be the initial interval")
      subtracted should equal (Left(interval))

    }

    it ("should subtract an entirely overlapping interval") {

      Given("the interval 2 to 6")
      val interval = Interval(2, 6)

      When("subtracting -3 to 8")
      val subtracted = interval - Interval(-3, 8)

      Then("the result should be empty")
      subtracted match {
        case Left(sub) => sub should be ('empty)
        case _ => fail(subtracted + " was not empty")
      }

    }

    it ("should subtract an interval that overlaps in the middle") {

      Given("an interval 0 to 10")
      val interval = Interval(0, 10)

      When("subtracting 3 to 7")
      val subtracted = interval - Interval(3, 7)

      Then("the result should be [0, 3] and [7, 10]")
      subtracted match {
        case Right((left, right)) =>
          left should be (Interval(0, 3))
          right should be (Interval(7, 10))

        case _ => fail("Interval was not split")
      }
    }

    it ("should subtract an interval the overlaps partially from the left") {

      Given("the interval 2, 6")
      val interval = Interval(2, 6)

      When("subtracting 2, 3")
      val subtracted = interval - Interval(2, 3)

      Then("the result should be 3 to 6")
      subtracted should be (Left(Interval(3, 6)))

    }

    it ("should subtract an interval the overlaps partially from the right") {

      Given("the interval 2, 6")
      val interval = Interval(2, 6)

      When("subtracting 4, 6")
      val subtracted = interval - Interval(4, 6)

      Then("the result should be 2 to 4")
      subtracted should be (Left(Interval(2, 4)))

    }

    it ("should subtract a point") {

      Given("the interval 2, 6")
      val interval = Interval(2, 6)

      When("subtracting 4")
      val subtracted = interval - Interval.point(4)

      Then("the result should be 2 to 4 and 4 to 6")
      subtracted should be (Right((Interval(2, 4), Interval(4, 6))))

    }

    /*
     * Difference
     */

    it ("should diff partially overlapping intervals to the sides") {

      Given("the interval 0 to 10")
      val interval = Interval(0, 10)

      When("differing the intervals -2 to 4 and 8 to 12")
      val diff = interval diff (Interval(-2, 4), Interval(8, 12))

      Then("the result should be 4 to 8")
      diff should be (Vector(Interval(4, 8)))

    }

    it ("should diff entirely overlapping intervals") {

      Given("the interval 0 to 10")
      val interval = Interval(0, 10)

      When("differing the intervals -2 to 4 and -5 to 12")
      val diff = interval diff (Interval(-2, 4), Interval(-5, 12))

      Then("the result should be empty")
      diff should be ('empty)

    }

    it ("should return the left splits when a subsequent split removes the remaining interval") {

      Given("the interval 0 to 10")
      val interval = Interval(0, 10)

      When("splitting the interval at 4 and then removing the rest")
      val diff = interval diff (Interval(4, 5), Interval(5, 12))

      Then("the result should be 0 to 4")
      diff should be (Vector(Interval(0, 4)))

    }

    it ("should split an interval at multiple points") {

      Given("the interval 0 to 10")
      val interval = Interval(0, 10)

      When("differing the intervals [2,3], [4,5] and [8,9]")
      val diff = interval diff (Interval(2, 3), Interval(4, 5), Interval(8, 9))

      Then("the result should be [0,2], [3,4], [5,8], [9,10]")
      diff should be (Vector(Interval(0, 2), Interval(3, 4), Interval(5, 8), Interval(9, 10)))

    }

    it ("should split an interval using intervals that begin at the same point") {

      Given("the interval 0 to 10")
      val interval = Interval(0, 10)

      When("differing the intervals [5,6], [5,7] and [5,9]")
      val diff = interval diff (Interval(5, 6), Interval(5, 7), Interval(5, 9))

      Then("the result should be [0,5] and [9,10]")
      diff should be (Vector(Interval(0, 5), Interval(9, 10)))

    }

    it ("should split an interval using intervals in non-ascending order") {

      val interval = Interval(0, 10)

      When("differing the intervals [7,9], [5,6] and [-3,2]")
      val diff = interval diff (Interval(7, 9), Interval(5, 6), Interval(-3, 2))

      Then("the result should be [2,5], [6,7] and [9,10]")
      diff should be (Vector(Interval(2, 5), Interval(6, 7), Interval(9, 10)))

    }

  }

}
