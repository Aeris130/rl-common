package net.cyndeline.rlcommon.math.geom

import net.cyndeline.rlcommon.SpecImports

class LineSpec extends SpecImports {

  private def line(from: (Double, Double), to: (Double, Double)): Line = Line(DPoint(from._1, from._2), DPoint(to._1, to._2))

  describe("Line") {

    /*
     *
     *  Line intersections
     *
     */

    it ("should report two disjoint parallel lines as non-intersecting") {

      Given("Two lines from (1,1)->(4,1) and (5,1)->(9,1)")
      val l1 = line((1, 1), (4, 1))
      val l2 = line((5, 1), (9, 1))

      When("checking them for an intersection point")
      val intersection = l1 intersectionPoint l2

      Then("no such point should be found")
      intersection should be ('empty)

    }

    it ("should report two overlapping parallel lines as non-intersecting") {

      Given("Two lines from (1,1)->(4,1) and (3,1)->(9,1) that overlaps from (3,1) to (4,1)")
      val l1 = line((1, 1), (4, 1))
      val l2 = line((3, 1), (9, 1))

      When("checking them for an intersection point")
      val intersection = l1 intersectionPoint l2

      Then("no such point should be found")
      intersection should be ('empty)

    }

    it ("should not report two non-parallel lines that intersects at a corner as intersecting") {

      Given("a line (0,0)->(3,3) and a line (3,3)->(5,3)")
      val l1 = line((0, 0), (3, 3))
      val l2 = line((3, 3), (5, 3))

      When("checking them for an intersection point")
      val intersection = l1 intersectionPoint l2

      Then("no such point should be found")
      intersection should be ('empty)

    }

    it ("should not report to non-parallel lines as intersecting if one of them intersect at its corner") {

      Given("a line (0,0)->(3,3) and a line (3,0)->(3,7)")
      val l1 = line((0, 0), (3, 3))
      val l2 = line((3, 0), (3, 7))

      When("checking them for an intersection point")
      val intersection = l1 intersectionPoint l2

      Then("no such point should be found")
      intersection should be ('empty)

    }

    it ("should report to intersecting lines as such") {

      Given("a line from (2,1) to (2,7) and a line (1,4) to (5,4)")
      val l1 = line((2, 1), (2, 7))
      val l2 = line((1, 4), (5, 4))

      When("checking them for an intersection point")
      val intersection = l1 intersectionPoint l2

      Then("they should overlap at (2, 4)")
      intersection should be ('defined)
      intersection.get should be (DPoint(2, 4))

    }

    /*
     *
     *  Point intersection
     *
     */

    it ("should not consider a point outside the line as intersecting") {

      Given("a line (1,1) to (5,1) and a point (3,4)")
      val l = line((1, 1), (5, 1))
      val point = DPoint(3, 4)

      When("checking if the line contains the point")
      val contains = l.containsPoint(point)

      Then("the result should be false")
      contains should be (false)

    }

    it ("should not consider a point that lies parallel to the line, but not within its start and stop coordinates, as intersecting") {

      Given("a line (1,1) to (5,1) and a point (6,1)")
      val l = line((1, 1), (5, 1))
      val point = DPoint(6, 1)

      When("checking if the line contains the point")
      val contains = l.containsPoint(point)

      Then("the result should be false")
      contains should be (false)

    }

    it ("should consider a point that lies on the end coordinates as intersecting") {

      Given("a line (1,1) to (5,1) and two points (1,1) and (5,1)")
      val l = line((1, 1), (5, 1))
      val point1 = DPoint(1, 1)
      val point2 = DPoint(5, 1)

      When("checking if the line contains the point")
      val containsStart = l.containsPoint(point1)
      val containsStop = l.containsPoint(point2)

      Then("the result should be true")
      containsStart should be (true)
      containsStop should be (true)

    }

    it ("should consider a point that lies inside the lines end coordinates as intersecting") {

      Given("a line (0,0) to (4,4) and a point (3,3)")
      val l = line((0, 0), (4, 4))
      val point = DPoint(3, 3)

      When("checking if the line contains the point")
      val contains = l.containsPoint(point)

      Then("the result should be true")
      contains should be (true)

    }

  }

}
