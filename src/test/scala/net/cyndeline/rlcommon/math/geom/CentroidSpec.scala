package net.cyndeline.rlcommon.math.geom

import net.cyndeline.rlcommon.SpecImports

class CentroidSpec extends SpecImports {

  describe("Centroid") {

    it ("should compute the centroid of a set of points") {

      Given("the points (2,1), (2,3), (4,1), (4,3)")
      val p1 = Point(2, 1)
      val p2 = Point(2, 3)
      val p3 = Point(4, 1)
      val p4 = Point(4, 3)

      When("computing their centroids")
      val centroid = Centroid.fromPoints(p1, p2, p3, p4)

      Then("the centroid should be (3,2)")
      centroid should be (Point(3, 2))

    }

  }

}
