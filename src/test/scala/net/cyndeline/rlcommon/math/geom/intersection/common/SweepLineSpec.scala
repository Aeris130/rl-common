package net.cyndeline.rlcommon.math.geom.intersection.common

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.intersection.common.SweepLine.LineEntry
import net.cyndeline.rlcommon.math.geom.{Line, Point, RPoint}

class SweepLineSpec extends SpecImports {
  private val ordering = SweepLine.buildOrdering[Line]

  private def threeSegments = new {
    val lowest = RPoint(1, 8)
    val l1 = Segment(0, Line(lowest, RPoint(9, 9))) // Top segment
    val l2 = Segment(1, Line(Point(4, 6), Point(8, 5)))
    val l3 = Segment(2, Line(Point(2, 5), Point(7, 4))) // Bottom segment

    // l3's x coordinate comes before l2, so it must be inserted first
    val line = SweepLine(lowest, 3).insert(l1.source, l1).insert(l3.source, l3).insert(l2.source, l2)
  }

  /*
   * A segment ending in the middle of a vertical segment. This case triggers a lot of non-slope-related
   * checks, since neither segment has its source in (4,4).
   */
  private def verticalTarget(beforeFlip: Boolean = true) = new {
    val l1 = LineEntry(RPoint(4, 2), Segment(1, Line(Point(2, 2), Point(4, 4))), beforeFlip)
    val l2 = LineEntry(RPoint(4, 2), Segment(0, Line(Point(4, 2), Point(4, 7))), beforeFlip)
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
      val above = line.above(l2)
      val below = line.below(l2)

      Then("l1 and l3 should be found")
      (above, below) should be ((Some(l1), Some(l3)))

    }

    it ("should retrieve the neighbor above a segment if no neighbor exists below it") {

      Given("a line with three segments")
      val f = threeSegments
      import f._

      When("retrieving neighbors for the lower segment")
      val above = line.above(l3)
      val below = line.below(l3)

      Then("the neighbor above should be l2")
      above should be (Some(l2))

      And("the neighbor below should be empty")
      below should be ('empty)

    }

    it ("should retrieve the neighbor below a segment if no neighbor exists above it") {

      Given("a line with three segments")
      val f = threeSegments
      import f._

      When("retrieving neighbors for the upper segment")
      val above = line.above(l1)
      val below = line.below(l1)

      Then("the neighbor below should be l2")
      below should be (Some(l2))

      And("the neighbor above should be empty")
      above should be ('empty)

    }

    /*
     * Swapping
     */

    it ("should swap a pencil of segments") {

      Given("three segments l:1-3 intersecting at point (2,2) with differing slopes")
      val l1 = Segment(0, Line(Point(1, 1), Point(3, 3)))
      val l2 = Segment(1, Line(Point(1, 2), Point(3, 2)))
      val l3 = Segment(2, Line(Point(1, 3), Point(3, 1)))
      val line = sweepLine(l1, l3, l2)
      assert(line.values == Seq(l3, l2, l1))

      When("swapping at (2,2)")
      val swapped = line.swap(RPoint(2, 2), l2, l3, l1)

      Then("the segments should be ordered l1, l2, l3")
      assert(swapped.values == Seq(l1, l2, l3))

    }

    it ("should swap vertical segments with other segments having their source on it") {

      Given("a vertical segment from (1,0) and up, with intersecting segments beginning at (1,1), (1,2) and (1,3)")
      val v = Segment(0, Line(Point(1, 0), Point(1, 3)))
      val l1 = Segment(1, Line(Point(1, 0), Point(2, 0)))
      val l2 = Segment(2, Line(Point(1, 1), Point(2, 1)))
      val l3 = Segment(3, Line(Point(1, 2), Point(2, 2)))
      val line = sweepLine(v, l1, l3, l2)
      assert(line.values == Seq(l3, l2, l1, v))

      When("swapping the intersection points (v,l1), (v,l2), (v,l3) in order")
      val swap1 = line.swap(l1.start, l1, v)
      val swap2 = swap1.swap(l2.start, l2, v)
      val swap3 = swap2.swap(l3.start, v, l3)

      Then("after the first swap, the vertical segment should lie between l2 and l1")
      assert(swap1.values == Seq(l3, l2, v, l1))

      And("after the second swap, the vertical segment should lie between l3 and l2")
      assert(swap2.values == Seq(l3, v, l2, l1))

      And("after the third swap, the vertical segment should be on top of the sweep line")
      assert(swap3.values == Seq(v, l3, l2, l1))

    }

    it ("should swap collinear segments") {

      Given("two identical segments intersected by a third segment at (2,2)")
      val l1 = Segment(0, Line(Point(1, 1), Point(3, 3)))
      val l2 = Segment(1, Line(Point(1, 1), Point(3, 3)))
      val l3 = Segment(2, Line(Point(1, 2), Point(3, 2)))
      val line = sweepLine(l1, l2, l3)
      assert(line.values == Seq(l3, l2, l1))

      When("swapping the segments at (2,2)")
      val swapped = line.swap(RPoint(2,2), l2, l1, l3)

      Then("the resulting line should store its segments in the reversed order")
      swapped.values should be (line.values.reverse)

    }

    it ("should swap multiple vertical segments") {

      Given("two vertical overlapping segments intersected by a third at (1,2)")
      val l1 = Segment(0, Line(Point(1, 1), Point(1, 3)))
      val l2 = Segment(1, Line(Point(1, 1), Point(1, 3)))
      val l3 = Segment(2, Line(Point(1, 2), Point(2, 2)))
      val line = sweepLine(l1, l2, l3)
      assert(line.values == Seq(l3, l2, l1))

      When("swapping the segments at (1,2)")
      val swapped = line.swap(RPoint(1,2), l2, l1, l3)

      Then("the resulting line should store its segments in the reversed order")
      swapped.values should be (line.values.reverse)

    }

    it ("should swap two collinear overlapping segments multiple times against each other") {

      Given("two collinear overlapping segments l1 and l2, intersected at two points by other segments at (2,1) and (5,1)")
      val l1 = Segment(0, Line(Point(1, 1), Point(7, 1)))
      val l2 = Segment(1, Line(Point(2, 1), Point(9, 1)))
      val l3 = Segment(2, Line(Point(2, 0), Point(2, 3)))
      val l4 = Segment(3, Line(Point(5, 0), Point(5, 3)))
      val line = SweepLine(RPoint(1, 1), 4).insert(l1.source, l1).insert(l2.source, l2).insert(l3.source, l3)
      assert(line.values == Seq(l2, l1, l3))

      When("swapping l1+l2 and l3 and then l1+l2 and l4")
      val swap1 = line.swap(RPoint(2, 1), l3, l2, l1)
      val nextPoint = swap1.remove(l3).insert(l4.source, l4) // l3 and l4 can't be cut at the same time
      assert(nextPoint.values == Seq(l1, l2, l4))
      val swap2 = nextPoint.swap(RPoint(5, 1), l4, l2, l1)

      Then("the first swap should result in l3 being on top, and l1 and l2 changing places")
      swap1.values should be (line.values.reverse)

      And("the second swap should place l4 on top, with l1 and l2 changing places")
      swap2.values should be (Seq(l4, l1, l2))

    }

    it ("should swap single-coordinate segments") {
      // Needs multi-coordinate as well, or no swappable single-coordinate point.
      Given("two lines intersecting a single-coordinate line at (5,6)")
      val l1 = Segment(0, Line(Point(1, 1), Point(10, 10)))
      val l2 = Segment(1, Line(Point(3, 9), Point(9, 3)))
      val single = Segment(2, Line(Point(6, 6), Point(6, 6)))
      val line = sweepLine(l1, l2, single)
      assert(line.values == Seq(l2, l1, single))

      When("swapping the lines at (5,6)")
      val swapped = line.swap(single.start, single, l2, l1)

      Then("the segments should reverse order")
      swapped.values should be (line.values.reverse)

    }

    /*
     *
     * Ordering tests
     *
     * This is done to skip having to set up a sweep line with a correct internal data structure just to trigger
     * bugs related to the ordering used in the RB tree.
     *
     */

    it ("should order two non-flipped segments according to slope at the target of a segment") {

      Given("a segment 1 that ends in a vertical segment 2 at (4,4), outside of 2's source and target")
      val f = verticalTarget(true)
      import f._

      When("checking ordering status")
      val l1LessThan2 = ordering.lt(l1, l2)
      val l2LessThan1 = ordering.lt(l2, l1)

      Then("2 should be less than 1")
      l1LessThan2 should be (false)
      l2LessThan1 should be (true)

    }

    it ("should order two flipped segments according to slope at the target of a segment") {

      Given("a segment 1 that ends in a vertical segment 2 at (4,4) to the right of the point, outside of 2's source and target")
      val f = verticalTarget(false)
      import f._

      When("checking ordering status")
      val l1LessThan2 = ordering.lt(l1, l2)
      val l2LessThan1 = ordering.lt(l2, l1)

      Then("1 should be less than 2")
      l1LessThan2 should be (true)
      l2LessThan1 should be (false)

    }

    it ("should order two swapped segments according to their slopes if they intersect at a point where they haven't been swapped yet") {

      Given("a vertical segment l1 (3,2)->(3,4) that has been swapped at (3,2) and a horizontal segment l2 (1,4)->(3,4) that has been swapped at (1,4)")
      val l1 = LineEntry(RPoint(3, 2), Segment(1, Line(Point(3, 2), Point(3, 4))), false)
      val l2 = LineEntry(RPoint(1, 4), Segment(2, Line(Point(1, 4), Point(3, 4))), false)

      When("checking ordering status")
      val l1LessThan2 = ordering.lt(l1, l2)
      val l2LessThan1 = ordering.lt(l2, l1)

      Then("l1 should be less than l2")
      l1LessThan2 should be (true)
      l2LessThan1 should be (false)

    }

    it ("should reverse the order according if a segment has its source inside another segment and both segments were swapped at the source") {

      Given("a segment l1 (1,0)->(1,3) being swapped at (1,2) with segment l2 (1,2)->(2,2)")
      val l1 = LineEntry(RPoint(1, 2), Segment(1, Line(Point(1, 0), Point(1, 3))), false)
      val l2 = LineEntry(RPoint(1, 2), Segment(2, Line(Point(1, 2), Point(2, 2))), false)

      When("checking ordering status")
      val l1LessThan2 = ordering.lt(l1, l2)
      val l2LessThan1 = ordering.lt(l2, l1)

      Then("l2 should be less than l1")
      l1LessThan2 should be (false)
      l2LessThan1 should be (true)

    }

    it ("should reverse the order if one segments has its swapping point before the other") {

      Given("a segment l1 (1,0)->(1,3) being swapped at (1,2), and a segment l2 (1,1)->(2,1) swapped at (1,1)")
      val l1 = LineEntry(RPoint(1, 2), Segment(1, Line(Point(1, 0), Point(1, 3))), false)
      val l2 = LineEntry(RPoint(1, 1), Segment(2, Line(Point(1, 1), Point(2, 1))), false)

      When("checking ordering status")
      val l1LessThan2 = ordering.lt(l1, l2)
      val l2LessThan1 = ordering.lt(l2, l1)

      Then("l2 should be less than l1")
      l1LessThan2 should be (false)
      l2LessThan1 should be (true)

    }

    it ("should not consider two collinear segments as swapped if one of them has been swapped at a point not present on the other") {

      Given("a segment (1,2)->(1,6) swapped at (1,2), and a segment (1,5)->(1,7)")
      val l1 = LineEntry(RPoint(1, 2), Segment(1, Line(Point(1, 2), Point(1, 6))), false)
      val l2 = LineEntry(RPoint(1, 5), Segment(2, Line(Point(1, 5), Point(1, 7))), true)

      When("checking ordering status")
      val l1LessThan2 = ordering.lt(l1, l2)
      val l2LessThan1 = ordering.lt(l2, l1)

      Then("l1 should be less than l2")
      l1LessThan2 should be (true)
      l2LessThan1 should be (false)

    }

    it ("should not consider two collinear segments as swapped if both segments share each others swap points, but only one has been swapped") {

      Given("two segments (1,2)->(2,6), with one (l1) being before (1,2) and the other (l2) after (2,6)")
      val id1 = 1
      val id2 = 2
      val l1 = LineEntry(RPoint(1, 2), Segment(id1, Line(Point(1, 2), Point(2, 6))), true)
      val l2 = LineEntry(RPoint(2, 6), Segment(id2, Line(Point(1, 2), Point(2, 6))), false)

      When("checking ordering status")
      val l1LessThan2 = ordering.lt(l1, l2)
      val l2LessThan1 = ordering.lt(l2, l1)

      Then("id's should determine below status")
      l1LessThan2 should be (true)
      l2LessThan1 should be (false)

    }

  }

  private def sweepLine(segments: Segment[Line]*): SweepLine[Line] = {
    require(segments.nonEmpty, "Tried to instantiate empty sweep line")
    val coordinates: Seq[Point] = segments.flatMap(s => Set(s.start, s.stop))
    val minX = coordinates.minBy(_.x).x
    val minY = coordinates.minBy(_.y).x
    var line = SweepLine[Line](RPoint(minX, minY), segments.size)
    for (s <- segments)
      line = line.insert(s.start, s)
    line
  }

}
