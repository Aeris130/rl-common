package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.{Line, Point}

/**
  * Created by Tobias Edin on 2016-07-03.
  */
class SegmentSpec extends SpecImports {

  describe("Segment") {

    it ("should define a segment that has its source point on another segment as above it") {

      Given("a segment s1 that begins below s2, and a segment s2 beginning on s1 at (4,8)")
      val s1 = Segment(0, Point(2, 6), Point(7, 11))
      val s2 = Segment(1, Point(4, 8), Point(7, 7))

      When("checking below-status")
      Then("s1 should be below s2 at x-coordinate 4")
      s1.below(4, s2) should be (true)

      And("s2 should not be below s1 at x-coordinate 4")
      s2.below(4, s1) should be (false)

    }

    it ("should order a vertical segment above a non-vertical if they do not intersect at an x-coordinate and the " +
      "vertical segments lowest y-coordinate is greater than the highest of the non-vertical") {

      Given("a vertical segment above a non-vertical")
      val v = Segment(0, Line(Point(4, 4), Point(4, 9)))
      val nv = Segment(1, Line(Point(4, 0), Point(5, 3)))

      When("checking v's and nv's below status relative to each other")
      val r1 = nv.below(4, v)
      val r2 = v.below(4, nv)

      Then("the v should be above nv")
      r1 should be (true)
      r2 should be (false)

    }

    it ("should order vertical non-overlapping segments  according to their y-coordinates") {

      Given("two vertical segments, with v1.start.y > v2.stop.y")
      val v1 = Segment(1, Line(Point(4, 4), Point(4, 9)))
      val v2 = Segment(0, Line(Point(4, 0), Point(4, 3)))

      When("checking v1's and v2's below status relative to each other")
      val v1BelowV2 = v1.below(4, v2)
      val v2BelowV1 = v2.below(4, v1)

      Then("v1 should be above v2")
      v1BelowV2 should be (false)
      v2BelowV1 should be (true)

    }

    it ("should order a single-point segments below another segment despite that the other segments begins at a lower x-value elsewhere") {

      Given("a single-point segment and a diagonal segment beginning below x")
      val single = Segment(0, Line(Point(4, 4), Point(4, 4)))
      val diagonal = Segment(1, Line(Point(2, 2), Point(6, 6)))

      When("checking below status relative to each other")
      val sBelowOther = single.below(4, diagonal)
      val otherBelowS = diagonal.below(4, single)

      Then("the single-segment should be below the other")
      sBelowOther should be (true)
      otherBelowS should be (false)

    }

  }

}
