package net.cyndeline.rlcommon.math.geom

import net.cyndeline.rlcommon.SpecImports

class CentroidSpec extends SpecImports {

  describe("Centroid") {

    it ("should compute the centroid of a set of points") {

      Given("the points (2,1), (2,3), (4,1), (4,3)")
      val p1 = RPoint(2, 1)
      val p2 = RPoint(2, 3)
      val p3 = RPoint(4, 1)
      val p4 = RPoint(4, 3)

      When("computing their centroids")
      val centroid = Centroid.fromPoints(p1, p2, p3, p4)

      Then("the centroid should be (3,2)")
      centroid should be (RPoint(3, 2))

    }

    it ("should compute the centroid of a set of identical points") {

      Given("5 identical points")
      val p1 = RPoint(3, 2)
      //...

      When("computing their centroids")
      val centroid = Centroid.fromPoints(p1, p1, p1, p1, p1)

      Then("the centroid should be equal to the points")
      centroid should be (p1)

    }

    it ("should throw an exception when computing centroids from a single point") {

      Given("a single point")
      val p = RPoint(0, 0)

      When("computing its centroids")

      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        Centroid.fromPoints(p)
      }

    }

  }

}
