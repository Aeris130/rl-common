package net.cyndeline.rlcommon.math.geom

import net.cyndeline.rlcommon.SpecImports

class LineSpec extends SpecImports {

  private def line(from: (Int, Int), to: (Int, Int)): Line = Line(Point(from._1, from._2), Point(to._1, to._2))

  describe("Line") {

    /*
     *
     *  Misc
     *
     */

    it ("should not be collinear with a segment using a different slope") {

      Given("two segments with different slopes")
      val line1 = line((1, 1), (4, 4))
      val line2 = line((1, 1), (12, 4))

      When("checking if the lines are collinear")
      val collinear = line1.collinearWith(line2)

      Then("the result should be false")
      collinear should be (false)

    }

    it ("should report collinear lines as such") {

      Given("two collinear lines")
      val line1 = line((1, 1), (4, 4))
      val line2 = line((5, 5), (8, 8))

      When("checking if the lines are collinear")
      val collinear1 = line1.collinearWith(line2)
      val collinear2 = line2.collinearWith(line1)

      Then("the result should be true")
      collinear1 should be (true)
      collinear2 should be (true)

    }

    it ("should not report lines as collinear if they have the same angle but doesn't lie on the same line") {

      Given("two collinear lines")
      val line1 = line((1, 1), (4, 4))
      val line2 = line((2, 1), (5, 4))

      When("checking if the lines are collinear")
      val collinear1 = line1.collinearWith(line2)
      val collinear2 = line2.collinearWith(line1)

      Then("the result should be false")
      collinear1 should be (false)
      collinear2 should be (false)

    }

    it ("should report single-point lines as collinear with other single-point lines") {

      Given("two lines with coordinate (2,2)")
      val line1 = line((2, 2), (2, 2))
      val line2 = line((2, 2), (2, 2))

      When("checking if the lines are collinear")
      val collinear1 = line1.collinearWith(line2)
      val collinear2 = line2.collinearWith(line1)

      Then("the result should be true")
      collinear1 should be (true)
      collinear2 should be (true)

    }

    it ("should report single-point lines as collinear with multi-point lines") {

      Given("a single-point line intersecting with another line")
      val line1 = line((2, 2), (2, 2))
      val line2 = line((1, 1), (3, 3))

      When("checking if the lines are collinear")
      val collinear1 = line1.collinearWith(line2)
      val collinear2 = line2.collinearWith(line1)

      Then("the result should be true")
      collinear1 should be (true)
      collinear2 should be (true)

    }

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
      val intersection = l1 intersection l2

      Then("no such point should be found")
      intersection should be ('empty)

    }

    it ("should report two non-parallel lines that intersects at a corner as intersecting") {

      Given("a line (0,0)->(3,3) and a line (3,3)->(5,3)")
      val l1 = line((0, 0), (3, 3))
      val l2 = line((3, 3), (5, 3))

      When("checking them for an intersection point")
      val intersection = l1 intersection l2

      Then("the point (3,3) should be the intersection between the lines")
      intersection should be ('defined)
      intersection.get.pointIntersection should be (DPoint(3, 3))

    }

    it ("should report two intersecting lines as such") {

      Given("a line from (2,1) to (2,7) and a line (1,4) to (5,4)")
      val l1 = line((2, 1), (2, 7))
      val l2 = line((1, 4), (5, 4))

      When("checking them for an intersection point")
      val intersection = l1 intersection l2

      Then("they should overlap at (2, 4)")
      intersection should be ('defined)
      intersection.get.isSinglePoint should be (true)
      intersection.get.pointIntersection should be (DPoint(2, 4))

    }

    it ("should report two lines with the same coordinates as overlapping") {

      Given("two lines a-b and b-a")
      val a = (2, 4)
      val b = (15, 89)
      val l1 = line(a, b)
      val l2 = line(b, a)

      When("checking them for intersections")
      val intersection = l1 intersection l2

      Then("an overlapping segment should be found")
      intersection should be ('defined)
      intersection.get.isInterval should be (true)

      And("the interval should go from the lower point to the larger")
      intersection.get.overlap should be ((Point(a), Point(b)))

    }

    it ("should not report two parallel lines as overlapping if they share the same slope, but not coordinates") {

      Given("a line from (0,0) to (3,3) and a line from (1,0) to (4,3)")
      val l1 = line((0, 0), (3, 3))
      val l2 = line((1, 0), (4, 3))

      When("checking them for intersections")
      val intersection = l1 intersection l2

      Then("no overlap should be found")
      intersection should be ('empty)

    }

    it ("should not report two collinear-but-disjoint segments as overlapping") {

      Given("two segments from (0,0) to (3,3) and (4,4) to (6,6)")
      val l1 = line((0, 0), (3, 3))
      val l2 = line((4, 4), (6, 6))

      When("checking them for intersections")
      val intersection = l1 intersection l2

      Then("no overlap should be found")
      intersection should be ('empty)

    }

    it ("should report segment overlap for vertical segments") {

      Given("two overlapping segments with the same x coordinate in all points")
      val l1 = line((0, 0), (0, 10))
      val l2 = line((0, 3), (0, 7))

      When("checking them for intersections")
      val intersection = l1 intersection l2

      Then("the overlap should be line 2")
      intersection should be ('defined)
      intersection.get.isInterval should be (true)
      intersection.get.overlap should be ((l2.start, l2.stop))

    }

    it ("should report a segment to be overlapping if it is the subset of another segment") {

      Given("a segment (0,0) to (10,10) and a subset (3,3) to (5,5)")
      val l1 = line((0, 0), (10, 10))
      val l2 = line((3, 3), (5, 5))

      When("checking if the subset overlaps the superset")
      val intersection = l2 intersection l1

      Then("the overlap should be (3,3) to (5,5)")
      intersection should be ('defined)
      intersection.get.isInterval should be (true)
      intersection.get.overlap should be ((l2.start, l2.stop))

    }

    it ("should report a segment to be overlapping if it is the superset of another segment") {

      Given("a segment (0,0) to (10,10) and a subset (3,3) to (5,5)")
      val l1 = line((0, 0), (10, 10))
      val l2 = line((3, 3), (5, 5))

      When("checking if the superset overlaps the superset")
      val intersection = l1 intersection l2

      Then("the overlap should be (3,3) to (5,5)")
      intersection should be ('defined)
      intersection.get.isInterval should be (true)
      intersection.get.overlap should be ((l2.start, l2.stop))

    }

    it ("should report a segment as overlapping when only the start point lies within another segment") {

      Given("a segment (1,2) to (1,7) and another segment with start (1,5) that ends beyond (1,7)")
      val l1 = line((1, 2), (1, 7))
      val l2 = line((1, 5), (1, 10))

      When("checking if the superset overlaps the superset")
      val intersection = l1 intersection l2

      Then("the overlap should be (1,5) to (1,7)")
      intersection should be ('defined)
      intersection.get.isInterval should be (true)
      intersection.get.overlap should be ((l2.start, l1.stop))

    }

    it ("should report a segment as overlapping when only the stop point lies within another segment") {

      Given("a segment (1,2) to (1,7) and another segment with start before (1,2) that ends at (1,5)")
      val l1 = line((1, 2), (1, 7))
      val l2 = line((1, 1), (1, 5))

      When("checking if the superset overlaps the superset")
      val intersection = l1 intersection l2

      Then("the overlap should be (1,2) to (1,5)")
      intersection should be ('defined)
      intersection.get.isInterval should be (true)
      intersection.get.overlap should be ((l1.start, l2.stop))

    }

    it ("should detect intersection between two segments when one segments source point lies within the other segment") {

      Given("two perpendicular segments intersecting at (4,9), the source of s1")
      val s1 = line((4, 9), (7, 7))
      val s2 = line((2, 7), (5, 10))

      When("checking if the segments intersect")
      val s1IntersectsS2 = s1.intersection(s2)
      val s2IntersectsS1 = s2.intersection(s1)

      Then("the point (4,9) should be detected as an intersection")
      s1IntersectsS2.get.pointIntersection should be (DPoint(4, 9))
      s2IntersectsS1 should equal (s1IntersectsS2)

    }

    it ("should detect intersection between two segments when one segments target point lies within the other segment") {

      Given("two perpendicular segments intersecting at (4,9), the target of s1")
      val s1 = line((7, 7), (4, 9))
      val s2 = line((2, 7), (5, 10))

      When("checking if the segments intersect")
      val s1IntersectsS2 = s1.intersection(s2)
      val s2IntersectsS1 = s2.intersection(s1)

      Then("the point (4,9) should be detected as an intersection")
      s1IntersectsS2.get.pointIntersection should be (DPoint(4, 9))
      s2IntersectsS1 should equal (s1IntersectsS2)

    }

    it ("should detect an intersection between two segments when one of them occupies a single coordinate") {

      Given("a single-point segment and one that encompasses multiple coordinates")
      val s1 = line((2, 2), (2, 2))
      val s2 = line((0, 4), (4, 0))

      When("checking if the segments intersect")
      val s1IntersectsS2 = s1.intersection(s2)
      val s2IntersectsS1 = s2.intersection(s1)

      Then("the point (2,2) should be detected as an intersection")
      s1IntersectsS2.get.overlap should be ((Point(2, 2), Point(2, 2)))
      s2IntersectsS1 should equal (s1IntersectsS2)

    }

    it ("should not detect an intersection between two segments when one of them occupies a single coordinate and they don't overlap") {

      Given("a single-point segment and one that encompasses multiple coordinates")
      val s1 = line((2, 2), (2, 2))
      val s2 = line((0, 3), (3, 0))

      When("checking if the segments intersect")
      val s1IntersectsS2 = s1.intersection(s2)
      val s2IntersectsS1 = s2.intersection(s1)

      Then("no overlap should be found")
      s1IntersectsS2 should be ('empty)
      s2IntersectsS1 should be ('empty)

    }

    it ("should detect a single-point intersection between collinear segments that share end-points") {

      Given("two collinear segments sharing (4,4)")
      val l1 = line((2, 2), (4, 4))
      val l2 = line((4, 4), (6, 6))

      When("checking if the segments intersect")
      val s1IntersectsS2 = l1.intersection(l2)
      val s2IntersectsS1 = l2.intersection(l1)

      Then("a single-coordinate point at (4,4) between l1 and l2 should be found")
      val intersection = s1IntersectsS2.get
      intersection.isSinglePoint should be (true)
      intersection.pointIntersection should be (DPoint(4, 4))
      s2IntersectsS1.get should equal (s1IntersectsS2.get)

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

    it ("should not consider a point that lies outside a single-coordinate line as intersecting") {

      Given("a single-point line and a point")
      val l = line((2, 2), (2, 2))
      val point = DPoint(0, 3)

      When("checking if the line contains the point")
      val contains = l.containsPoint(point)

      Then("the result should be false")
      contains should be (false)

    }

    /*
     * Splits
     */

    it ("should split a line an even amount of times") {

      Given("a line (7,3) -> (4,6)")
      val line = Line(Point(7, 3), Point(4, 6))

      When("splitting the line into 4 parts")
      val split = line.split(4)

      Then("the splits should be (7,3), (6,4), (5,5), (4,6)")
      split should be (Seq(DPoint(7, 3), DPoint(6, 4), DPoint(5, 5), DPoint(4, 6)))

    }

    it ("should split a line an odd amount of times") {

      Given("a line (7,3) -> (4,6)")
      val line = Line(Point(7, 3), Point(4, 6))

      When("splitting the line into 3 parts")
      val split = line.split(3)

      Then("the splits should be (7,3), (5.5, 4.5), (4,6)")
      split should be (Seq(DPoint(7, 3), DPoint(5.5, 4.5), DPoint(4, 6)))

    }

    it ("should 'split' a line into two parts") {

      Given("a line")
      val start = Point(234, 99743)
      val stop = Point(1334, 754)
      val line = Line(start, stop)

      When("splitting the line into 2 parts")
      val split = line.split(2)

      Then("the splits should be start and stop")
      split should be (Seq(start.toDouble, stop.toDouble))

    }

    it ("should split a single-point segment") {

      Given("a single-point line")
      val p = Point(2, 2)
      val line = Line(p, p)

      When("splitting the line 4 times")
      val split = line.split(4)

      Then("only the lines start and stop coordinate should be returned")
      split should be (Vector(p.toDouble))

    }

    /*
     * Slopes
     */

    it ("should compute slopes") {

      Given("two lines between (3,6) and (13, 12) with different start and stop points")
      val l1 = Line(Point(3, 6), Point(13, 12))
      val l2 = Line(Point(13, 12), Point(3, 6))

      When("computing the slope for the lines")
      val slope1 = l1.slope
      val slope2 = l2.slope

      Then("both slopes should be 6 / 10")
      slope1 should be (6.0 / 10)
      slope2 should be (6.0 / 10)

    }

    it ("should not define slopes for vertical segments") {

      Given("a segments with the same x-value in both coordinates")
      val segment = Line(Point(4, 1), Point(4, 3))

      When("computing the slope")
      Then("an exception should be thrown")
      intercept[Error] {
        segment.slope
      }

    }

  }

}
