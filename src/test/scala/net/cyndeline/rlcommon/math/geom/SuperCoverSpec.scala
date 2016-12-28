package net.cyndeline.rlcommon.math.geom

import net.cyndeline.rlcommon.SpecImports

import scala.language.implicitConversions

class SuperCoverSpec extends SpecImports {
  private val dummySize = 9999

  describe("SuperCover") {

    it ("should detect points of a horizontal line") {

      Given("the line (3,4)->(0,4)")
      val line = Line((3, 4), (0, 4))

      When("computing the supercover")
      val cover = SuperCover.cover(dummySize, line)

      Then("the cover should be 3, 2, 1, 0")
      cover should be (Vector(Point(3, 4), Point(2, 4), Point(1, 4), Point(0, 4)))

    }

    it ("should detect points of a vertical line") {

      Given("the line (3,0)->(3,4)")
      val line = Line((3, 0), (3, 4))

      When("computing the supercover")
      val cover = SuperCover.cover(dummySize, line)

      Then("the cover should be 0, 1, 2, 3, 4")
      cover should be (Vector(Point(3, 0), Point(3, 1), Point(3, 2), Point(3, 3), Point(3, 4)))

    }

    it ("should detect points of a diagonal line") {

      Given("a line from (1,1) to (3,4)")
      val line = Line((1, 1), (3, 4))

      When("computing the supercover")
      val cover = SuperCover.cover(dummySize, line)

      Then("6 grid cells should be intercepted")
      cover should have size 6

      And("those cells should be (1,1), (1,2), (2,2), (2,3), (3,3) and (3,4)")
      cover.toSet should be (Set(Point(1, 1), Point(1, 2), Point(2, 2), Point(2, 3), Point(3, 3), Point(3, 4)))

    }

    it ("should detect points of a line with size 1") {

      Given("a line beginning and ending at the same coordinate")
      val p = Point(22, 22)
      val line = Line(p, p)

      When("computing the supercover")
      val cover = SuperCover.cover(dummySize, line)

      Then("the coordinate should be returned")
      cover should be (Vector(p))

    }

    it ("should detect both cells on the side of an intersection") {

      Given("the line (1,1) to (3,3)")
      val line = Line((1, 1), (3, 3))

      When("computing the supercover")
      val coverAB = SuperCover.cover(dummySize, line)
      val coverBA = SuperCover.cover(dummySize, Line(line.stop, line.start))

      Then("4 grid cells should be intercepted")
      coverAB should have size 7
      coverBA should have size 7

      And("those cells should be (1,1), (1,2), (2,1), (2,2), (2,3), (3,2) and (3,3)")
      coverAB.toSet should be (coverBA.toSet)
      coverAB.toSet should be (Set(Point(1, 1), Point(1, 2), Point(2, 1), Point(2, 2), Point(2, 3), Point(3, 2), Point(3, 3)))

    }

    it ("should return the first valid point") {

      Given("a line that covers (2,4) and (1,7) in that order")
      val line = Line((3, 3), (1, 8))

      When("finding a point using a function that returns true for (2,4) and (1,7)")
      val a = Point(2, 4)
      val b = Point(1, 7)
      val found = SuperCover.find(dummySize, line, (p: Point) => p == a || p == b)

      Then("the result should be (2,4)")
      found should be (Some(a))

    }

  }

}
