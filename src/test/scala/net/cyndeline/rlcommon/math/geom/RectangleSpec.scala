package net.cyndeline.rlcommon.math.geom

import net.cyndeline.rlcommon.SpecImports

class RectangleSpec extends SpecImports {

  describe("Rectangle") {

    it ("it should consider rectangles on its border as overlapping") {

      Given("a rectangle between (1,1) and (2,2)")
      val r = Rectangle(Point(1, 1), 2, 2)

      When("checking overlap for rectangles on its four sides")
      val west = Rectangle(Point(0, 1), 2, 2)
      val north = Rectangle(Point(1, 2), 2, 2)
      val east = Rectangle(Point(2, 1), 2, 2)
      val south = Rectangle(Point(1, 0), 2, 2)

      Then("all four should be overlapping")
      r.overlaps(west) should be (true)
      r.overlaps(north) should be (true)
      r.overlaps(east) should be (true)
      r.overlaps(south) should be (true)

    }

    it ("should consider a rectangle sharing a corner as overlapping") {

      Given("a rectangle between (1,1) and (2,2)")
      val r1 = Rectangle(Point(1, 1), 2, 2)

      When("checking overlap with a rectangle (0,0) to (1,1)")
      val r2 = Rectangle(Point(0, 0), 2, 2)

      Then("they should overlap")
      r1.overlaps(r2) should be (true)

    }

    it ("should not overlap a rectangle outside of it") {

      Given("a rectangle between (2,4) and (8,7)")
      val r1 = Rectangle(Point(2, 4), 7, 4)

      When("checking overlap with a rectangle above it")
      val r2 = Rectangle(Point(12, 14), 7, 4)

      Then("they should not overlap")
      r1.overlaps(r2) should be (false)

    }

    it ("should overlap a rectangle partly inside of it") {

      Given("a rectangle between (3,3) and (7,7)")
      val r1 = Rectangle(Point(3, 3), 5, 5)

      When("checking overlap with a rectangle between (0,0) and (4,4)")
      val r2 = Rectangle(Point(0, 0), 5, 5)

      Then("they should overlap")
      r1.overlaps(r2) should be (true)

    }

    it ("should overlap a rectangle inside of it") {

      Given("a rectangle between (0,0) and (7,7)")
      val r1 = Rectangle(Point(0, 0), 8, 8)

      When("checking overlap with a rectangle between (1,1) and (4,4)")
      val r2 = Rectangle(Point(1, 1), 5, 5)

      Then("they should overlap")
      r1.overlaps(r2) should be (true)

    }

  }
}
