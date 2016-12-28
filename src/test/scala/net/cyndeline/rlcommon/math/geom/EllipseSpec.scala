package net.cyndeline.rlcommon.math.geom

import net.cyndeline.rlcommon.SpecImports

class EllipseSpec extends SpecImports {

  private def makeEllipse(center: (Int, Int), rh: Int, rv: Int) = new Ellipse(Point(center), rh, rv, 0)

  describe("Ellipse") {

    /*
     *
     *  Line intersections
     *
     */

    it ("should not return coordinates for a line that lies outside the ellipse") {

      Given("an ellipse with center (3,4) and vertical radius 2")
      val ellipse = makeEllipse((3, 4), 1, 2)

      When("checking if the line (1,6) -> (9,8) intersects it")
      val intersection = ellipse.intersectsLine(RPoint(1, 6), RPoint(9, 8))

      Then("no result should be given")
      intersection should be ('empty)

    }

    it ("should not return coordinates for a line that lies inside the ellipse") {

      Given("an ellipse with center (4,4) and vertical radius 7")
      val ellipse = makeEllipse((4, 4), 7, 7)

      When("checking if the line (2,3) -> (5,3) intersects it")
      val intersection = ellipse.intersectsLine(RPoint(2, 3), RPoint(5, 3))

      Then("no result should be given")
      intersection should be ('empty)

    }

    it ("should return a single coordinate for a line that begins outside and ends inside of it") {

      Given("an ellipse with center (3,4) and horizontal radius 2")
      val ellipse = makeEllipse((3, 4), 2, 1)

      When("checking if the line (0,4) -> (3,4) intersects it")
      val intersection = ellipse.intersectsLine(RPoint(0, 4), RPoint(3, 4)).get

      Then("the point (1, 4) should be returned")
      intersection should have size 1
      intersection.head should be (DPoint(1, 4))

    }

    it ("should return a single coordinate for a line that begins inside and ends outside of it") {

      Given("an ellipse with center (3,4) and horizontal radius 2")
      val ellipse = makeEllipse((3, 4), 2, 1)

      When("checking if the line (3,4) -> (0,4) intersects it")
      val intersection = ellipse.intersectsLine(RPoint(3, 4), RPoint(0, 4)).get

      Then("the point (1, 4) should be returned")
      intersection should have size 1
      intersection.head should be (DPoint(1, 4))

    }

    it ("should return two coordinates for a line that intersects the ellipse twice") {

      Given("an ellipse with center (4,4) and vertical radius 3")
      val ellipse = makeEllipse((4, 4), 1, 3)

      When("checking if the line (4,10) -> (4,0) intersects it")
      val intersection = ellipse.intersectsLine(RPoint(4, 10), RPoint(4, 0)).get

      Then("points (4,7) and (4,1) should be returned")
      intersection should have size 2
      val approxDoubleValues = intersection.map(p => (p.x.doubleValue(), p.y.doubleValue()))
      val eps = 0.1E-7
      def fuzzy(d: Double, r: Double) = Math.abs(r - d) < eps
      assert(approxDoubleValues.exists(t => fuzzy(4.0, t._1) && fuzzy(7.0, t._2)))
      assert(approxDoubleValues.exists(t => fuzzy(4.0, t._1) && fuzzy(1.0, t._2)))

    }

    it ("should return a single coordinate for a line that tangents its shape at a starting coordinate") {

      Given("an ellipse with center (4,4) and vertical radius 3")
      val ellipse = makeEllipse((4, 4), 1, 3)

      When("checking if the line (1,7) -> (8,7) intersects it")
      val intersection = ellipse.intersectsLine(RPoint(1, 7), RPoint(8, 7)).get

      Then("the point (4, 7) should be returned")
      intersection should have size 1
      intersection.head should be (DPoint(4, 7))

    }

    it ("should return a single coordinate for a line that tangents its shape at a stop coordinate") {

      Given("an ellipse with center (4,4) and vertical radius 3")
      val ellipse = makeEllipse((4, 4), 1, 3)

      When("checking if the line (1,1) -> (8,1) intersects it")
      val intersection = ellipse.intersectsLine(RPoint(1, 1), RPoint(8, 1)).get

      Then("the point (4, 1) should be returned")
      intersection should have size 1
      intersection.head should be (DPoint(4, 1))

    }

    /*
     *
     *  Point intersections
     *
     */

    it ("should report that it does not contain a coordinate outside its circumference") {

      Given("an ellipse with center (4,4), horizontal radius 1 and vertical radius 3")
      val ellipse = makeEllipse((4, 4), 1, 3)

      When("checking if the point (2, 4) lies within the ellipse")
      val liesWithin = ellipse.containsPoint(RPoint(2, 4))

      Then("the result should be false")
      liesWithin should be (false)

    }

    it ("should report that it does contain a coordinate inside its circumference") {

      Given("an ellipse with center (4,4), horizontal radius 1 and vertical radius 3")
      val ellipse = makeEllipse((4, 4), 1, 3)

      When("checking if the point (4, 2) lies within the ellipse")
      val liesWithin = ellipse.containsPoint(RPoint(4, 2))

      Then("the result should be true")
      liesWithin should be (true)

    }

    it ("should report that it does contain a coordinate that lies on its circumference") {

      Given("an ellipse with center (4,4), horizontal radius 1 and vertical radius 3")
      val ellipse = makeEllipse((4, 4), 1, 3)

      When("checking if the point (4,1) lies within the ellipse")
      val liesWithin = ellipse.containsPoint(RPoint(4, 1))

      Then("the result should be true")
      liesWithin should be (true)

    }


    /*
     *
     * Rectangle intersections
     *
     */

    it ("should report that a rectangle lies outside its circumference") {

      Given("an ellipse with center (4,4) and radius 3")
      val ellipse = makeEllipse((4, 4), 3, 3)

      When("checking if a rectangle between (9, 9) and (12, 12) intersects it")
      val status = ellipse.intersectsRectangle(RPoint(9, 9), RPoint(12, 12))

      Then("the rectangle should be outside")
      status should be (Outside)

    }

    it ("should report that a rectangle intersects it if the rectangle has coordinates inside it") {

      Given("an ellipse with center (4,4) and radius 2")
      val ellipse = makeEllipse((4, 4), 2, 2)

      When("checking if a rectangle between (0, 0) and (4, 4) intersects it")
      val status = ellipse.intersectsRectangle(RPoint(0, 0), RPoint(4, 4))

      Then("the rectangle should be intersecting")
      status should be (Intersects)

    }

    it ("should report that a rectangle that tangents it intersects it") {

      Given("an ellipse with center (4,4) and radius 2")
      val ellipse = makeEllipse((4, 4), 2, 2)

      When("checking if a rectangle with its corner in (4, 2) intersects it")
      val status = ellipse.intersectsRectangle(RPoint(0, 0), RPoint(4, 2))

      Then("the rectangle should be intersecting")
      status should be (Intersects)

    }

    it ("should report that a rectangle lies inside of it") {

      Given("an ellipse with center (4,4) and radius 7")
      val ellipse = makeEllipse((4, 4), 7, 7)

      When("checking if a rectangle between (5, 5) and (6, 6) intersects it")
      val status = ellipse.intersectsRectangle(RPoint(5, 5), RPoint(6, 6))

      Then("the rectangle should be inside")
      status should be (Inside)

    }

  }
}
