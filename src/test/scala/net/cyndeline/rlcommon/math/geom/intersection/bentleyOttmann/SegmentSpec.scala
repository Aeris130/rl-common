package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.Point

/**
  * Created by Tobias Edin on 2016-07-03.
  */
class SegmentSpec extends SpecImports {

  describe("Segment") {

    it ("should define a segment that has its source point on another segment as above it") {

      Given("a segment s1 that begins below s2, and a segment s2 beginning on s1 at (4,8)")
      val s1 = Segment(Point(2, 6), Point(7, 11), 0)
      val s2 = Segment(Point(4, 8), Point(7, 7), 1)

      When("checking below-status")
      Then("s1 should be below s2 at x-coordinate 4")
      s1.below(4, s2) should be (true)

      And("s2 should not be below s1 at x-coordinate 4")
      s2.below(4, s1) should be (false)

    }

  }

}
