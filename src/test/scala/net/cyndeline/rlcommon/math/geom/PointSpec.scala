package net.cyndeline.rlcommon.math.geom

import net.cyndeline.rlcommon.SpecImports

class PointSpec extends SpecImports {

  describe("Point") {

    it ("should compute the angle to other points") {

      Given("a point P and 4 orthogonal neighbors")
      val p = Point(3, 3)
      val wn = Point(2, 3)
      val en = Point(4, 3)
      val nn = Point(3, 4)
      val sn = Point(3, 2)

      When("computing enighbor angles")
      val west = p.angleTo(wn)
      val east = p.angleTo(en)
      val north = p.angleTo(nn)
      val south = p.angleTo(sn)

      Then("the correct angles should be computed")
      west should be (180)
      east should be (0)
      north should be (90)
      south should be (270)

    }

  }

}
