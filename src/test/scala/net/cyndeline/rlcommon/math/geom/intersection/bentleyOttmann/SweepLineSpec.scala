package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.{Line, Point}
import net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann.SweepLine.LineEntry

class SweepLineSpec extends SpecImports {

  private def threeSegments = new {
    val lowest = Point(1, 8)
    val l1 = Segment(0, Line(lowest, Point(9, 9))) // Top segment
    val l2 = Segment(1, Line(Point(4, 6), Point(8, 5)))
    val l3 = Segment(2, Line(Point(2, 5), Point(7, 4))) // Bottom segment

    // l3's x coordinate comes before l2, so it must be inserted first
    val line = SweepLine(lowest, 3).insert(l1.source.x, l1).insert(l3.source.x, l3).insert(l2.source.x, l2)
  }

  describe("SweepLine") {

    it ("should retrieve the closest neighbor above a segment") {

      Given("a line with three segments")
      val f = threeSegments
      import f._

      When("retrieving the neighbor above the lowest segment")
      val neighbor = line.above(l3)

      Then("l2 should be found")
      neighbor should be (Some(l2))

    }

    it ("should retrieve the closest neighbor below a segment") {

      Given("a line with three segments")
      val f = threeSegments
      import f._

      When("retrieving the neighbor below the highest segment")
      val neighbor = line.below(l1)

      Then("l2 should be found")
      neighbor should be (Some(l2))

    }

    it ("should retrieve the neighbors above and below a segment with neighbors in both directions") {

      Given("a line with three segments")
      val f = threeSegments
      import f._

      When("retrieving neighbors for the middle segment")
      val neighbors = line.aboveAndBelow(l2)

      Then("l1 and l3 should be found")
      neighbors should be ((Some(l1), Some(l3)))

    }

    it ("should retrieve the neighbor above a segment if no neighbor exists below it") {

      Given("a line with three segments")
      val f = threeSegments
      import f._

      When("retrieving neighbors for the lower segment")
      val neighbors = line.aboveAndBelow(l3)

      Then("the neighbor above should be l2")
      neighbors._1 should be (Some(l2))

      And("the neighbor below should be empty")
      neighbors._2 should be ('empty)

    }

    it ("should retrieve the neighbor below a segment if no neighbor exists above it") {

      Given("a line with three segments")
      val f = threeSegments
      import f._

      When("retrieving neighbors for the upper segment")
      val neighbors = line.aboveAndBelow(l1)

      Then("the neighbor below should be l2")
      neighbors._2 should be (Some(l2))

      And("the neighbor above should be empty")
      neighbors._1 should be ('empty)

    }

  }

}
