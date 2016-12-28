package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.{Line, Point, RPoint}

class BentleyOttmannIntersectionSpec extends SpecImports {
  private val algorithm = BentleyOttmannIntersection.withoutSinglePointIntersections
  private val algWithSinglePoints = BentleyOttmannIntersection.withSinglePointIntersections

  private def r(a: Point, b: Point) = (RPoint(a), RPoint(b))

  // The regular line class considers lines with the same coordinate identical.
  class IDSegment(val id: Int, start: RPoint, stop: RPoint) extends Line(start, stop) {

    def this(id: Int) = this(id, RPoint(3, 3), RPoint(5, 3))

    override def toString: String = s"Seg[$id][${start.asTuple} to ${stop.asTuple}"
    override def equals(other: Any): Boolean = other match {
      case i: IDSegment => id == i.id
      case _ => false
    }
    override def hashCode: Int = id
  }

  describe("BentleyOttmannIntersection") {

    // Note: These tests are divided into 3 sections (source, target, intersection), handling the general case as well
    // as the degenerate cases as described in the article mentioned in the class description.

    /*
     * Misc
     */

    it ("should not report intersections in an empty segment set") {

      Given("an empty set of segments")
      val s = Vector[Line]()

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s)

      Then("no intersections should be found")
      intersections should be ('empty)

    }

    it ("should not report intersections for a set of non-intersecting segments") {

      Given("two non-intersecting segments")
      val s1 = Line((1, 1), (2, 2))
      val s2 = Line((5, 5), (7, 7))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2)

      Then("no intersections should be found")
      intersections should be ('empty)

    }

    it ("should detect overlap with more than 3 collinear segments that has different source and targets") {

      Given("5 collinear segments")
      val a = Line((-1, 5), (7, 5))
      val b = Line((0, 5), (8, 5))
      val c = Line((1, 5), (9, 5))
      val d = Line((2, 5), (10, 5))
      val e = Line((3, 5), (11, 5))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(a, b, c, d, e)

      Then("10 overlaps should be detected between a-e")
      intersections should have size 10
      assert(!intersections.exists(_._1.isSinglePoint))

      assert(intersections.exists(o => o._1.overlap == r(b.start, a.stop) && segSet(o._2) == Set(a, b)))
      assert(intersections.exists(o => o._1.overlap == r(c.start, a.stop) && segSet(o._2) == Set(a, c)))
      assert(intersections.exists(o => o._1.overlap == r(d.start, a.stop) && segSet(o._2) == Set(a, d)))
      assert(intersections.exists(o => o._1.overlap == r(e.start, a.stop) && segSet(o._2) == Set(a, e)))
      assert(intersections.exists(o => o._1.overlap == r(c.start, b.stop) && segSet(o._2) == Set(b, c)))
      assert(intersections.exists(o => o._1.overlap == r(d.start, b.stop) && segSet(o._2) == Set(b, d)))
      assert(intersections.exists(o => o._1.overlap == r(e.start, b.stop) && segSet(o._2) == Set(b, e)))
      assert(intersections.exists(o => o._1.overlap == r(d.start, c.stop) && segSet(o._2) == Set(c, d)))
      assert(intersections.exists(o => o._1.overlap == r(e.start, c.stop) && segSet(o._2) == Set(c, e)))
      assert(intersections.exists(o => o._1.overlap == r(e.start, d.stop) && segSet(o._2) == Set(d, e)))

    }

    it ("should detect overlap with more than 3 collinear segments that has the same source and targets") {

      Given("5 distinguishable segments with identical coordinates")
      val a = new IDSegment(1)
      val b = new IDSegment(2)
      val c = new IDSegment(3)
      val d = new IDSegment(4)
      val e = new IDSegment(5)

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(a, b, c, d, e)

      Then("1 overlap should be detected having all segments a-e")
      intersections should have size 1
      assert(!intersections.exists(_._1.isSinglePoint))
      val start = a.start
      val stop = a.stop
      assert(intersections.exists(o => o._1.overlap == (RPoint(start), RPoint(stop)) && segSet(o._2) == Set(a, b, c, d, e)))

    }

    it ("should store every overlapping segment if the same segment is inserted multiple times") {

      Given("three identical segments")
      val s1 = Line((0, 0), (4, 5))
      val s2 = s1
      val s3 = s1

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2, s3)

      Then("the resulting overlap should contain three instances of the line in its vector")
      intersections should have size 1
      assert(!intersections.exists(_._1.isSinglePoint))
      assert(intersections.exists(o => o._1.overlap == r(s1.start, s1.stop) && o._2 == Vector(s1, s2, s3)))

    }

    it ("should add every overlap with the same coordinates to the same intersection even if the segment source/target coordinates differ") {

      Given("two collinear segments with the same source/target, and a third collinear segment with differing coordinates")
      val s1 = new IDSegment(1, RPoint(3, 3), RPoint(5, 3))
      val s2 = new IDSegment(2, RPoint(3, 3), RPoint(5, 3))
      val differs = new IDSegment(3, RPoint(0, 3), RPoint(7, 3))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2, differs)

      Then("1 overlap should be detected having all three segments")
      intersections should have size 1
      assert(!intersections.exists(_._1.isSinglePoint))
      val start = s1.start
      val stop = s2.stop
      assert(intersections.exists(o => o._1.overlap == r(start, stop) && segSet(o._2) == Set(s1, s2, differs)))

    }

    it ("should handle single-point intersections for vertical segments") {

      Given("a vertex segment s1 and an intersecting segment s2")
      val s1 = Line((4, 2), (4, 6))
      val s2 = Line((2, 3), (6, 5))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2)

      Then("an intersection should be found between s1 and s2")
      intersections should have size 1
      val intersection = intersections.head
      intersection._1.isSinglePoint should be (true)
      intersection._1.pointIntersection should be (s1.intersection(s2).get.pointIntersection)
      segSet(intersection._2) should be (Set(s1, s2))

    }

    it ("should handle overlaps for vertical segments") {

      Given("three vertical collinear segments")
      val s1 = Line((0, 0), (0, 10))
      val s2 = Line((0, 3), (0, 7))
      val s3 = Line((0, 5), (0, 6))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2, s3)

      Then("two intersections for s1+s2 and s1+s2+s3 should be found")
      intersections should have size 2
      assert(!intersections.exists(_._1.isSinglePoint))

      assert(intersections.exists(o => o._1.overlap == r(s2.start, s2.stop) && segSet(o._2) == Set(s1, s2)))
      assert(intersections.exists(o => o._1.overlap == r(s3.start, s3.stop) && segSet(o._2) == Set(s1, s2, s3)))

    }

    it ("should handle segments where the target is less than the source") {

      Given("a vertical segment going from top to bottom and a horizontal segment going from right to left")
      val s1 = Line((0, 10), (0, 0))
      val s2 = Line((5, 5), (-6, 5))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2)

      Then("an intersection should be found between s1 and s2")
      intersections should have size 1
      val intersection = intersections.head
      intersection._1.isSinglePoint should be (true)
      intersection._1.pointIntersection should be (s1.intersection(s2).get.pointIntersection)
      segSet(intersection._2) should be (Set(s1, s2))

    }

    it ("should not detect intersections in a set with separate single-point segments") {

      Given("three different single-point segments")
      val s1 = Line((0, 0), (0, 0))
      val s2 = Line((1, 1), (1, 1))
      val s3 = Line((2, 2), (2, 2))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2, s3)

      Then("no intersections should be found")
      intersections should be ('empty)

    }

    it ("should detect intersections between overlapping single-point segments") {

      Given("multiple single.point segments overlapping")
      val p = RPoint(5, 5)
      val s1 = new IDSegment(0, p, p)
      val s2 = new IDSegment(1, p, p)
      val s3 = new IDSegment(2, p, p)
      val s4 = new IDSegment(3, p, p)
      val s5 = new IDSegment(4, p, p)

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2, s3, s4, s5)

      Then("an intersection should be found between the segments")
      intersections should have size 1
      assert(intersections.head._1.isInterval)
      assert(intersections.exists(o => o._1.overlap == (p, p) && segSet(o._2) == Set(s1, s2, s3, s4, s5)))

    }

    it ("should detect intersections between a point and a segment") {

      Given("a point and a segment that overlaps")
      val s1 = Line((0, 0), (3, 3))
      val s2 = Line((2, 2), (2, 2))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2)

      Then("an intersection should be found between s1 and s2")
      intersections should have size 1
      assert(!intersections.exists(_._1.isSinglePoint))
      assert(intersections.exists(o => o._1.overlap == r(s2.start, s2.stop) && segSet(o._2) == Set(s1, s2)))

    }

    it ("should detect separate intersections between a point and multiple segments") {

      Given("a point and two segments that overlaps")
      val s1 = Line((0, 0), (4, 4))
      val s2 = Line((2, 2), (2, 2))
      val s3 = Line((0, 4), (4, 0))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2, s3)

      Then("an intersection interval should be found between s1, s2 and s3")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 1
      assert(overlaps.exists(o => o._1.overlap == r(s2.start, s2.stop) && segSet(o._2) == Set(s1, s2, s3)))

      And("an intersection point should be found between s1 and s3")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      assert(points.exists(o => o._1.pointIntersection == RPoint(s2.start) && segSet(o._2) == Set(s1, s3)))

    }

    it ("should detect separate intersections between two overlapping points and multiple segments") {

      Given("two points and two segments that overlaps")
      val s1 = new IDSegment(1, RPoint(0, 0), RPoint(4, 4))
      val s2_1 = new IDSegment(2, RPoint(2, 2), RPoint(2, 2))
      val s2_2 = new IDSegment(22, RPoint(2, 2), RPoint(2, 2))
      val s3 = new IDSegment(3, RPoint(0, 4), RPoint(4, 0))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2_1, s2_2, s3)

      Then("an intersection should be found between s1, s2 and s3")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 1
      assert(overlaps.exists(o => o._1.overlap == r(s2_1.start, s2_1.stop) && segSet(o._2) == Set(s1, s2_1, s2_2, s3)))

      And("an intersection should be found between s1 and s3")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      assert(points.exists(o => o._1.pointIntersection == RPoint(s2_1.start) && segSet(o._2) == Set(s1, s3)))

    }

    it ("should detect a common intersection between a point and multiple collinear segments") {

      Given("two collinear segments and a single-point segment")
      val s1 = new IDSegment(1, RPoint(0, 0), RPoint(4, 4))
      val s2 = new IDSegment(2, RPoint(0, 0), RPoint(4, 4))
      val s3 = new IDSegment(3, RPoint(2, 2), RPoint(2, 2))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2, s3)

      Then("an intersection should be found between s1, s2 and s3, and between s1 and s2")
      intersections should have size 2
      assert(intersections.exists(o => o._1.overlap == r(s3.start, s3.stop) && segSet(o._2) == Set(s1, s2, s3)))
      assert(intersections.exists(o => o._1.overlap == r(s1.start, s1.stop) && segSet(o._2) == Set(s1, s2)))

    }

    it ("should detect intersections between segments intersecting at both sources and targets") {

      Given("four segments that intersect at (4,4) using their sources and targets")
      val l1 = Line((2, 2), (4, 4))
      val l2 = Line((2, 6), (4, 4))
      val l3 = Line((6, 6), (4, 4))
      val l4 = Line((6, 2), (4, 4))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(l1, l2, l3, l4)

      Then("an intersection at (4,4) should be found")
      intersections should have size 1
      intersections.head._1.isSinglePoint should be (true)
      intersections.head._1.pointIntersection should be (RPoint(4, 4))
      segSet(intersections.head._2) should be (Set(l1, l2, l3, l4))

    }

    it ("should detect corner intersections") {

      Given("four segments making up a rectangle from (2,4) to (4,6)")
      val l1 = Line((2, 4), (2, 6))
      val l2 = Line((2, 4), (4, 4))
      val l3 = Line((2, 6), (4, 6))
      val l4 = Line((4, 4), (4, 6))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(l1, l2, l3, l4)

      Then("four corner intersections should be found")
      intersections.filter(_._1.isInterval) should be ('empty)
      intersections should have size 4
      assert(intersections.exists(o => o._1.pointIntersection == RPoint(2, 4) && segSet(o._2) == Set(l1, l2)))
      assert(intersections.exists(o => o._1.pointIntersection == RPoint(4, 4) && segSet(o._2) == Set(l2, l4)))
      assert(intersections.exists(o => o._1.pointIntersection == RPoint(2, 6) && segSet(o._2) == Set(l1, l3)))
      assert(intersections.exists(o => o._1.pointIntersection == RPoint(4, 6) && segSet(o._2) == Set(l3, l4)))

    }

    it ("should handle single-point segments at the target of another segment") {

      Given("a segment 1 ending in segment 2 at (4,4), with 4 other single-point segments")
      val l1 = new IDSegment(0, RPoint(2, 2), RPoint(4, 4))
      val l2 = new IDSegment(1, RPoint(4, 2), RPoint(4, 7))
      val l3 = new IDSegment(2, RPoint(4, 4), RPoint(4, 4))
      val l4 = new IDSegment(3, RPoint(4, 4), RPoint(4, 4))
      val l5 = new IDSegment(4, RPoint(4, 4), RPoint(4, 4))
      val l6 = new IDSegment(5, RPoint(4, 4), RPoint(4, 4))

      When("computing the intersections of the set")
      val intersections = algWithSinglePoints.computeIntersections(l1, l2, l3, l4, l5, l6)

      Then("an intersection between all 6 segments should be found at (4,4)")
      val point = intersections.filter(_._1.isSinglePoint)
      point should have size 1
      segSet(point.head._2) should equal (Set(l1, l2, l3, l4, l5, l6))

      And("an overlap between all 6 segments should be found at (4,4)")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      segSet(overlap.head._2) should equal (Set(l1, l2, l3, l4, l5, l6))

    }

    it ("should handle multiple single-point segments at the source of another segment") {

      Given("a segment from (2,3) to (5,4) and two single-point segments at (2,3)")
      val l1 = new IDSegment(0, RPoint(2, 3), RPoint(5, 4))
      val l2 = new IDSegment(1, RPoint(2, 3), RPoint(2, 3))
      val l3 = new IDSegment(2, RPoint(2, 3), RPoint(2, 3))

      When("computing the intersections of the set")
      val intersections = algWithSinglePoints.computeIntersections(l1, l2, l3)

      Then("an overlap between the three segments should be found")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      segSet(overlap.head._2) should equal (Set(l1, l2, l3))

    }

    it ("should handle a swapped segment intersecting an intersection point between two other segments") {

      Given("an initial intersection between l1 and l2 (causing l2 to be swapped) and an intersection at (1,5) between l3, l4 and l1")
      val l1 = Line((1, 2), (1, 6))
      val l2 = Line((1, 2), (3, 2))
      val l3 = Line((1, 5), (3, 5))
      val l4 = Line((1, 5), (1, 7))

      When("computing the intersections of the set")
      val intersections = algWithSinglePoints.computeIntersections(l1, l2, l3, l4)

      Then("an intersection between l1 and l2 should be found at (1,2)")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 2
      assert(points.exists(o => o._1.pointIntersection == RPoint(l1.start) && segSet(o._2) == Set(l1, l2)))

      And("an intersection between l1, l3 and l4 should be found at (1,5)")
      assert(points.exists(o => o._1.pointIntersection == RPoint(l3.start) && segSet(o._2) == Set(l1, l3, l4)))

      And("an overlap between l1 and l4 should be found at (1,5)->(1,6)")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      segSet(overlap.head._2) should equal (Set(l1, l4))

    }

    it ("should detect a point-intersection for two collinear segments that intersect at a point") {

      Given("two collinear segments intersecting at (7,7)")
      val l1 = Line((4, 4), (7, 7))
      val l2 = Line((7, 7), (10, 10))

      When("computing the intersections of the set")
      val intersections = algWithSinglePoints.computeIntersections(l1, l2)

      Then("a single intersection should be found at (7,7)")
      intersections should have size 1
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      assert(points.exists(o => o._1.pointIntersection == RPoint(l2.start) && segSet(o._2) == Set(l1, l2)))

    }

    it ("should detect an intersection between two collinear segment when a third collinear segment has the same start as the first, and ends after the second") {

      Given("two collinear segments l1 and l2 intersecting at (7,7), and a third segment starting at l1's source and ending after (7,7)")
      val l1 = new IDSegment(1, RPoint(4, 4), RPoint(7, 7))
      val l2 = new IDSegment(2, RPoint(7, 7), RPoint(10, 10))
      val l3 = new IDSegment(3, RPoint(4, 4), RPoint(9, 9))

      When("computing the intersections of the set")
      val intersections = algWithSinglePoints.computeIntersections(l1, l2, l3)

      Then("an intersection between l1, l2 and l3 should be found at (7,7)")
      intersections should have size 3
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      assert(points.exists(o => o._1.pointIntersection == RPoint(l2.start) && segSet(o._2) == Set(l1, l2, l3)))

      And("an overlap between l1 and l3 should exist between (4,4)->(7,7)")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 2
      assert(overlap.exists(o => o._1.overlap == r(l3.start, l1.stop) && segSet(o._2) == Set(l1, l3)))

      And("an overlap between l2 and l3 should exist between (7,7)->(9,9)")
      assert(overlap.exists(o => o._1.overlap == r(l2.start, l3.stop) && segSet(o._2) == Set(l2, l3)))

    }

    it ("should detect an intersection between two collinear segment pairs, intersecting at their endpoints") {

      Given("two segments l1 and l2: (2,2)->(5,5) intersecting at (5,5) with l3 and l4")
      val l1 = new IDSegment(2, RPoint(2, 2), RPoint(5, 5))
      val l2 = new IDSegment(3, RPoint(2, 2), RPoint(5, 5))
      val l3 = new IDSegment(0, RPoint(5, 5), RPoint(8, 8))
      val l4 = new IDSegment(1, RPoint(5, 5), RPoint(10, 10))

      When("computing the intersections of the set")
      val intersections = algWithSinglePoints.computeIntersections(l1, l2, l3, l4)

      Then("an intersection between l1, l2, l3 and l4 should be found")
      intersections should have size 3
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      assert(points.exists(o => o._1.pointIntersection == RPoint(l1.stop) && segSet(o._2) == Set(l1, l2, l3, l4)))

      And("an overlap between l1 and l2 should exist between (2,2)->(5,5)")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 2
      assert(overlap.exists(o => o._1.overlap == r(l1.start, l1.stop) && segSet(o._2) == Set(l1, l2)))

      And("an overlap between l3 and l4 should exist between (5,5)->(8,8)")
      assert(overlap.exists(o => o._1.overlap == r(l3.start, l3.stop) && segSet(o._2) == Set(l3, l4)))

    }

    it ("should detect an intersection between two collinear endpoints covered by a collinear neighbor starting after the first, and ending at the target of the last") {

      Given("an initial segment l3 intersecting l2 at (3,7), and a segment l1 that starts within l3 and ends at the target of l2")
      val l1 = new IDSegment(1, RPoint(1, 5), RPoint(6, 10))
      val l2 = new IDSegment(2, RPoint(3, 7), RPoint(6, 10))
      val l3 = new IDSegment(3, RPoint(0, 4), RPoint(3, 7))

      When("computing the intersections of the set")
      val intersections = algWithSinglePoints.computeIntersections(l1, l2, l3)

      Then("an intersection between l1, l2 and l3 should be found")
      intersections should have size 3
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      assert(points.exists(o => o._1.pointIntersection == RPoint(l3.stop) && segSet(o._2) == Set(l1, l2, l3)))

      And("an overlap between l1 and l3 should exist between (1,5)->(3,7)")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 2
      assert(overlap.exists(o => o._1.overlap == r(l1.start, l3.stop) && segSet(o._2) == Set(l3, l1)))

      And("an overlap between l1 and l2 should exist between (3,7)->(6,10)")
      assert(overlap.exists(o => o._1.overlap == r(l2.start, l1.stop) && segSet(o._2) == Set(l2, l1)))

    }

    it ("should detect an intersection between collinear pairs of segments ending at different targets") {

      Given("a segment 5 and two collinear pairs of segments (2,4 and 1,3) beginning at the target of 4")
      val l1 = new IDSegment(0, RPoint(5, 5), RPoint(8, 8))
      val l2 = new IDSegment(1, RPoint(5, 5), RPoint(10, 10))
      val l3 = new IDSegment(2, RPoint(5, 5), RPoint(8, 8))
      val l4 = new IDSegment(3, RPoint(5, 5), RPoint(10, 10))
      val l5 = new IDSegment(4, RPoint(0, 0), RPoint(5, 5))

      When("computing the intersections of the set")
      val intersections = algWithSinglePoints.computeIntersections(l1, l2, l3, l4, l5)

      Then("an intersection between all five segments should be found")
      intersections should have size 3
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      assert(points.exists(o => o._1.pointIntersection == RPoint(l5.stop) && segSet(o._2) == Set(l1, l2, l3, l4, l5)))

      And("an overlap between l1-4 should exist between (5,5)->(8,8)")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 2
      assert(overlap.exists(o => o._1.overlap == r(l1.start, l1.stop) && segSet(o._2) == Set(l1, l2, l3, l4)))

      And("an overlap between l2 and l4 should exist between (5,5)->(10,10)")
      assert(overlap.exists(o => o._1.overlap == r(l2.start, l2.stop) && segSet(o._2) == Set(l2, l4)))

    }

    it ("should detect an intersection overlapped by multiple collinear segments") {

      Given("six collinear segments overlapping or intersecting at (5,5)")
      val l0 = new IDSegment(0, RPoint(4, 4), RPoint(7, 7))
      val l1 = new IDSegment(1, RPoint(4, 4), RPoint(9, 9))
      val l2 = new IDSegment(2, RPoint(5, 5), RPoint(8, 8))
      val l3 = new IDSegment(3, RPoint(5, 5), RPoint(10, 10))
      val l4 = new IDSegment(4, RPoint(2, 2), RPoint(5, 5))
      val l5 = new IDSegment(5, RPoint(2, 2), RPoint(7, 7))

      When("computing the intersections of the set")
      val intersections = algWithSinglePoints.computeIntersections(l0, l1, l2, l3, l4, l5)

      Then("an intersection between all six segments should be found at (5,5)")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      assert(points.exists(o => o._1.pointIntersection == RPoint(5, 5) && segSet(o._2) == Set(l0, l1, l2, l3, l4, l5)))

    }

    it ("should resolve upper and lower neighbors in an intersection where the upper/lower-most segments have neighbors that intersect them, but not in the pencil") {

      Given("a segment from (2,4) to (8,8) that is intersected at two points by segments 0 (left) and 1 (right)")
      val l0 = new IDSegment(0, RPoint(2, 2), RPoint(6, 8))
      val l1 = new IDSegment(1, RPoint(4, 4), RPoint(6, 8))
      val l2 = new IDSegment(2, RPoint(8, 8), RPoint(2, 4))

      When("swapping segments at the intersection of 0 and 2")
      val intersections = algWithSinglePoints.computeIntersections(l0, l1, l2)

      Then("segment two should find the intersection between 2 and 1")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 3
      val l1_2_isct = l1.intersection(l2).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == l1_2_isct && segSet(o._2) == Set(l1, l2)))

      And("an intersection between 0 and 2 should be found")
      val l0_2_isct = l0.intersection(l2).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == l0_2_isct && segSet(o._2) == Set(l0, l2)))

      And("an intersection between 0 and 1 should be found")
      assert(points.exists(o => o._1.pointIntersection == RPoint(6, 8) && segSet(o._2) == Set(l0, l1)))

    }

    it ("should add both overlaps and intersections when removing a segment") {

      Given("a segment 0 that overlaps segment 1, who in turn intersects segment 2")
      val l0 = new IDSegment(0, RPoint(5, 3), RPoint(1, 0))
      val l1 = new IDSegment(1, RPoint(9, 5), RPoint(2, 6))
      val l2 = new IDSegment(2, RPoint(9, 6), RPoint(1, 0))

      When("removing segment 0 from the sweep line")
      val intersections = algWithSinglePoints.computeIntersections(Vector(l0, l1, l2))

      Then("segments 1 and 2 should be found as non-collinear and have their intersection point added")
      intersections should have size 2
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      val l1_2_isct = l1.intersection(l2).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == l1_2_isct && segSet(o._2) == Set(l1, l2)))

      And("an overlap between s0 and s1 should be found")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      assert(overlap.exists(o => o._1.overlap == r(l0.stop, l0.start) && segSet(o._2) == Set(l0, l2)))

    }

    it ("should not add collinear neighbors on the sweep line to an intersection if they don't intersect the current segment") {

      Given("a segment s0 that intersects s1 when it is added to the sweep line with x = 2, and a collinear neighbor s2 that doesn't intersect s0")
      val s0 = new IDSegment(0, RPoint(2, 6), RPoint(8, 3))
      val s1 = new IDSegment(1, RPoint(2, 4), RPoint(7, 9))
      val s2 = new IDSegment(2, RPoint(0, 2), RPoint(3, 5))

      When("computing the intersections of the set")
      val intersections = algWithSinglePoints.computeIntersections(Vector(s0, s1, s2))

      Then("the intersection between s0 and s1 should not contain 2")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      val s0_1_isct = s1.intersection(s0).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == s0_1_isct && segSet(o._2) == Set(s0, s1)))

      And("an overlap between s1 and s2 should be found")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      assert(overlap.exists(o => o._1.overlap == r(s1.start, s2.stop) && segSet(o._2) == Set(s1, s2)))

    }

    it ("should register intersection points for collinear segments inside a pencil") {

      Given("two segments s2 and s3 that are intersected by the collinear segments s1 and s1")
      val s0 = new IDSegment(0, RPoint(3, 1), RPoint(5, 5))
      val s1 = new IDSegment(1, RPoint(4, 3), RPoint(6, 7))
      val s2 = new IDSegment(2, RPoint(2, 9), RPoint(5, 0))
      val s3 = new IDSegment(3, RPoint(1, 9), RPoint(8, 2))

      When("the lowest segment (s0) registers an intersection in s2/3 and is subsequently swapped around s2")
      val intersections = algWithSinglePoints.computeIntersections(Vector(s0, s1, s2, s3))

      Then("segment s1 should also register its intersection with s3 before its target is reached")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 3
      val s0_3_isct = s0.intersection(s3).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == s0_3_isct && segSet(o._2) == Set(s0, s1, s3)))

      And("an intersection between s0, s1 and s2 should be found")
      val s0_2_isct = s0.intersection(s2).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == s0_2_isct && segSet(o._2) == Set(s0, s1, s2)))

      And("an intersection between s2 and s3 should be found")
      val s2_3_isct = s2.intersection(s3).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == s2_3_isct && segSet(o._2) == Set(s2, s3)))

      And("an overlap between s0 and s1 should be found")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      assert(overlap.exists(o => o._1.overlap == r(s1.start, s0.stop) && segSet(o._2) == Set(s0, s1)))

    }

    it ("should add the collinear neighbors of a neighbor that is found during the target case") {

      Given("a segment s0 that has neighbors s2 and s1 at the time of removal")
      val s0 = new IDSegment(0, RPoint(0, 5), RPoint(2, 2))
      val s1 = new IDSegment(1, RPoint(1, 0), RPoint(8, 0))
      val s2 = new IDSegment(2, RPoint(1, 8), RPoint(7, 0))
      val s3 = new IDSegment(3, RPoint(1, 0), RPoint(9, 0))

      When("segment 0's neighbors s1 and s2 have their intersection added")
      val intersections = algWithSinglePoints.computeIntersections(Vector(s0, s1, s2, s3))

      Then("segment s3 that is collinear with s1 should be added as well")
      val points = intersections.filter(_._1.isSinglePoint)
      assert(points.exists(o => o._1.pointIntersection == RPoint(7, 0) && segSet(o._2) == Set(s1, s2, s3)))

      And("there should be an overlap between s1 and s3")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      assert(overlap.exists(o => o._1.overlap == r(s1.start, s1.stop) && segSet(o._2) == Set(s1, s3)))

    }

    it ("should register intersection points for collinear neighbors of the upper/lower neighbors found during the intersection case " +
      "when the collinear neighbor is above the upper/lower neighbor") {

      Given("a segment s2 intersecting below s1, with two collinear segments s3 and s0 above s1 on the sweep line")
      val s0 = new IDSegment(0, RPoint(3, 2), RPoint(4, 9))
      val s1 = new IDSegment(1, RPoint(2, 6), RPoint(8, 0))
      val s2 = new IDSegment(2, RPoint(4, 1), RPoint(4, 9))
      val s3 = new IDSegment(3, RPoint(3, 2), RPoint(4, 9))

      When("swapping segments s1 and s2")
      val intersections = algWithSinglePoints.computeIntersections(Vector(s0, s1, s2, s3))
      intersections should have size 4

      Then("the intersection between s2 and s3 with its neighbor s0 should be detected and added to the event queue")
      val points = intersections.filter(_._1.isSinglePoint)
      assert(points.exists(o => o._1.pointIntersection == RPoint(4, 9) && segSet(o._2) == Set(s0, s2, s3)))

      And("an intersection between s1, s0 and s3 should be found")
      val s1_3_isct = s1.intersection(s3).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == s1_3_isct && segSet(o._2) == Set(s0, s1, s3)))

      And("an intersection between s1 and s2 should be found")
      assert(points.exists(o => o._1.pointIntersection == RPoint(4, 4) && segSet(o._2) == Set(s1, s2)))

    }

    it ("should register intersection points for collinear neighbors of the upper/lower neighbors found during the intersection case " +
      "when the collinear neighbor is below the upper/lower neighbor") {

      Given("segments s1 and s2 being collinear and intersecting s0, such that s2 is below s0 in the intersection case" +
        " preceeding the s1,2,3 intersection")
      val s0 = new IDSegment(0, RPoint(0, 9), RPoint(8, 0))
      val s1 = new IDSegment(1, RPoint(2, 2), RPoint(7, 2))
      val s2 = new IDSegment(2, RPoint(2, 2), RPoint(9, 2))
      val s3 = new IDSegment(3, RPoint(0, 2), RPoint(6, 3))

      When("swapping segments s0 and s3")
      val intersections = algWithSinglePoints.computeIntersections(Vector(s0, s1, s2, s3))
      intersections should have size 3

      Then("an intersection between s1, s2 and s0 should be found")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 2
      val s0_1_isct = s0.intersection(s1).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == s0_1_isct && segSet(o._2) == Set(s0, s1, s2)))

      And("an intersection between s0 and s3 should be found")
      val s0_3_isct = s0.intersection(s3).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == s0_3_isct && segSet(o._2) == Set(s0, s3)))

      And("an overlap between s1 and s2 should be found")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      assert(overlap.exists(o => o._1.overlap == r(s1.start, s1.stop) && segSet(o._2) == Set(s1, s2)))

    }

    it ("should register overlaps betwen a single-point segment s and its neighbors when the neighbors are not collinear " +
      "and s does not neighbor them all on the sweep line") {

      Given("a single-point segment s0 that ends up on one side of both segments s1 and s2 on the sweep line")
      val s0 = new IDSegment(0, RPoint(9, 2), RPoint(9, 2))
      val s1 = new IDSegment(1, RPoint(0, 3), RPoint(9, 2))
      val s2 = new IDSegment(2, RPoint(0, 4), RPoint(9, 2))

      When("computing intersections using the set")
      val intersections = algWithSinglePoints.computeIntersections(Vector(s2, s1, s0))

      Then("an intersection point should be found between s0, s1 and s2")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      assert(points.exists(o => o._1.pointIntersection == RPoint(9, 2) && segSet(o._2) == Set(s0, s1, s2)))

      And("an overlap should be found between s0, s1 and s2")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      assert(overlap.exists(o => o._1.overlap == r(s0.start, s0.stop) && segSet(o._2) == Set(s0, s1, s2)))

    }

    it ("should detect intersections in the target case when the upper neighbor has collinear neighbors") {

      Given("segments s1 and s3 being collinear and intersecting s2, with s0 between them on the sweep line")
      val s0 = new IDSegment(0, RPoint(4, 8), RPoint(6, 6))
      val s1 = new IDSegment(1, RPoint(4, 9), RPoint(8, 5))
      val s2 = new IDSegment(2, RPoint(5, 1), RPoint(9, 9))
      val s3 = new IDSegment(3, RPoint(4, 9), RPoint(8, 5))

      When("s0's target case is reached")
      val intersections = algWithSinglePoints.computeIntersections(Vector(s0, s1, s2, s3))

      Then("an intersection between s1, s2 and s3 should be found")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      val s1_2_isct = s1.intersection(s2).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == s1_2_isct && segSet(o._2) == Set(s1, s2, s3)))

      And("an overlap should be found between s1 and s3")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      assert(overlap.exists(o => o._1.overlap == r(s1.start, s1.stop) && segSet(o._2) == Set(s1, s3)))

    }

    it ("should add s2 as lower neighbor when adding intersections in the collinearity case") {

      Given("segment s3 intersected first by s1 then by s2 from below")
      val s0 = new IDSegment(0, RPoint(3, 8), RPoint(4, 5))
      val s1 = new IDSegment(1, RPoint(2, 5), RPoint(6, 3))
      val s2 = new IDSegment(2, RPoint(4, 1), RPoint(8, 9))
      val s3 = new IDSegment(3, RPoint(3, 8), RPoint(5, 2))

      When("computing intersections using the set")
      val intersections = algWithSinglePoints.computeIntersections(Vector(s0, s1, s2, s3))
      intersections should have size 4

      Then("the intersection between s3 and s2 should be found when swapping the first intersection point between s1 and s3")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 3
      val s2_3_isct = s2.intersection(s3).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == s2_3_isct && segSet(o._2) == Set(s2, s3)))

      And("an intersection between s1 and s3 should be found")
      val s1_3_isct = s1.intersection(s3).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == s1_3_isct && segSet(o._2) == Set(s1, s3)))

      And("an intersection between s1 and s2 should be found")
      val s1_2_isct = s1.intersection(s2).get.pointIntersection
      assert(points.exists(o => o._1.pointIntersection == s1_2_isct && segSet(o._2) == Set(s1, s2)))

      And("an overlap between s3 and s0 should be found")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      assert(overlap.exists(o => o._1.overlap == r(s0.start, s0.stop) && segSet(o._2) == Set(s0, s3)))

    }

    it ("should add more than 3 segments to a single-point overlap at the same time") {

      Given("segments s0, s2, s3 intersecting with single-point segment s1 at (5,2)")
      val s0 = new IDSegment(0, RPoint(0, 5), RPoint(5, 2))
      val s1 = new IDSegment(1, RPoint(5, 2), RPoint(5, 2))
      val s2 = new IDSegment(2, RPoint(2, 5), RPoint(5, 2))
      val s3 = new IDSegment(3, RPoint(1, 4), RPoint(9, 0))

      When("computing intersections using the set")
      val intersections = algWithSinglePoints.computeIntersections(Vector(s0, s1, s2, s3))
      intersections should have size 2

      Then("an overlap should be found at (5,2) with all four segments")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 1
      assert(overlap.exists(o => o._1.overlap == r(s1.start, s1.stop) && segSet(o._2) == Set(s0, s1, s2, s3)))

      And("an intersection should be found at (5,2) with all four segments")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      assert(points.exists(o => o._1.pointIntersection == RPoint(5, 2) && segSet(o._2) == Set(s0, s1, s2, s3)))

    }

    it ("should return intersection results deterministically") {

      Given("segments resulting in both point and segment intersections")
      val s0 = new IDSegment(0, RPoint(0, 9), RPoint(8, 0))
      val s1 = new IDSegment(1, RPoint(2, 2), RPoint(7, 2))
      val s2 = new IDSegment(2, RPoint(2, 2), RPoint(9, 2))
      val s3 = new IDSegment(3, RPoint(0, 2), RPoint(6, 3))

      When("computing intersections using the set multiple times using the same input")
      val input = Vector(s0, s1, s2, s3)
      val first = algWithSinglePoints.computeIntersections(input)

      Then("the result should be the same each time")
      for (i <- 0 to 100) {
        val intersections = algWithSinglePoints.computeIntersections(input)
        intersections should equal (first)
      }

    }

    /*
     * Source vertex case
     */

    it ("should handle the general Source case") {

      Given("three non-intersecting segments")
      val s1 = Line((1, 8), (9, 9))
      val s = Line((4, 6), (8, 5))
      val s2 = Line((2, 5), (7, 4))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s, s2)

      Then("no intersections should be found")
      intersections should be ('empty)

    }

    it ("should handle degenerate Source case (b)") {

      Given("an upper segment s1, a lower segment s2 and a middle segment s that intersects the middle of s1 " +
            "using its source point")
      val s1 = Line((2, 6), (7, 11))
      val s = Line((4, 8), (7, 7))
      val s2 = Line((3, 2), (9, 1))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s, s2)

      Then("the source of s should be an intersection between s ans s1")
      intersections should have size 1
      val intersection = intersections.head
      intersection._1.isSinglePoint should be (true)
      intersection._1.pointIntersection should be (RPoint(s.start))
      segSet(intersection._2) should be (Set(s, s1))

    }

    it ("should handle degenerate Source case (c)") {

      Given("an upper segment 1 starting at the same coordinate as middle segment s")
      val s1 = Line((3, 5), (7, 7))
      val s = Line((3, 5), (8, 4))
      val s2 = Line((2, 3), (9, 1))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s, s2)

      Then("the source of s should be an intersection between s ans s1")
      intersections should have size 1
      val intersection = intersections.head
      intersection._1.isSinglePoint should be (true)
      intersection._1.pointIntersection should be (RPoint(s.start))
      segSet(intersection._2) should be (Set(s, s1))

    }

    it ("should handle degenerate Source case (d)") {

      Given("an upper segment s', a lower segment s2, a middle segment s and a segment s1 that starts in s and intersects s'")
      val sPrime = Line((3, 10), (10, 9))
      val s1 = Line((2, 8), (8, 6))
      val s = Line((5, 7), (9, 10))
      val s2 = Line((1, 5), (9, 4))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(sPrime, s1, s, s2)

      Then("two intersections should be found")
      intersections should have size 2

      And("one intersection should be between s1 and s, in the source point of s")
      assert(intersections.exists(i => i._1.isSinglePoint &&
        i._1.pointIntersection == RPoint(s.start) &&
        segSet(i._2) == Set(s1, s)), "No intersection beginning in s found.")

      And("one intersection should be between s' and s")
      assert(intersections.exists(i => i._1.isSinglePoint &&
        i._1.pointIntersection == s.intersection(sPrime).get.pointIntersection &&
        segSet(i._2) == Set(sPrime, s)), "No intersection between s' and s found.")

    }

    it ("should handle degenerate Source case (e)") {

      Given("an upper segment s1', a lower segment s2, and two segments s1 and s beginning in the same coordinate")
      val s1Prime = Line((1, 8), (9, 10))
      val s = Line((4, 7), (8, 6))
      val s1 = Line((4, 7), (7, 5))
      val s2 = Line((1, 3), (9, 1))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1Prime, s1, s, s2)

      Then("the source of s should be an intersection between s ans s1")
      intersections should have size 1
      val intersection = intersections.head
      intersection._1.isSinglePoint should be (true)
      intersection._1.pointIntersection should be (RPoint(s.start))
      segSet(intersection._2) should be (Set(s, s1))

    }

    it ("should handle degenerate Source case (f)") {

      Given("three collinear segments s1, a segment s starting in s1 and a lower segment s2")
      val s1_1 = Line((2, 7), (5, 10))
      val s1_2 = Line((1, 6), (6, 11))
      val s1_3 = Line((3, 8), (7, 12))
      val s = Line((4, 9), (7, 7))
      val s2 = Line((0, 4), (10, 3))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1_1, s1_2, s1_3, s, s2)

      Then("there should be a single-point intersections between s and the three s1-segments")
      val singlePoints = intersections.filter(_._1.isSinglePoint)
      singlePoints should have size 1
      assert(singlePoints.exists(i => i._1.pointIntersection == RPoint(s.start) && segSet(i._2) == Set(s1_1, s1_2, s1_3, s)))

      And("there should be three overlapping segments between s1_1 + s1_2, s1_1 + s1_3 and s1_2 + s1_3")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 3
      assert(overlap.exists(o => o._1.overlap == r(s1_3.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_3)))
      assert(overlap.exists(o => o._1.overlap == r(s1_3.start, s1_2.stop) && segSet(o._2) == Set(s1_2, s1_3)))
      assert(overlap.exists(o => o._1.overlap == r(s1_1.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_2)))

    }

    it ("should handle degenerate Source case (g)") {

      Given("a top segment s1', a segment s with its source point intersecting a set of collinear segments s1, " +
            "and a bottom segment s2")
      val s1P = Line((0, 10), (11, 11))
      val s1_1 = Line((4, 7), (7, 4))
      val s1_2 = Line((2, 9), (8, 3))
      val s1_3 = Line((5, 6), (9, 2))
      val s = Line((6, 5), (10, 7))
      val s2 = Line((1, 1), (12, 0))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1_1, s1_2, s1_3, s, s2, s1P)

      Then("there should be three single-point intersections between s and the three s1-segments")
      val singlePoints = intersections.filter(_._1.isSinglePoint)
      singlePoints should have size 1
      assert(singlePoints.exists(i => i._1.pointIntersection == RPoint(s.start) && segSet(i._2) == Set(s1_1, s1_2, s1_3, s)))

      And("there should be three overlapping segments between s1_1 + s1_2, s1_1 + s1_3 and s1_2 + s1_3")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 3
      assert(overlap.exists(o => o._1.overlap == r(s1_1.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_2)))
      assert(overlap.exists(o => o._1.overlap == r(s1_3.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_3)))
      assert(overlap.exists(o => o._1.overlap == r(s1_3.start, s1_2.stop) && segSet(o._2) == Set(s1_2, s1_3)))

    }

    it ("should handle degenerate Source case (h)") {

      /* This test is a bit weird, in the sense that the original article case doesn't contain any intersections
       * that are caught by the collinear sub-algorithm. Due to this, s1P has been realigned to intersect with s despite
       * having s1:1-3 between it and s.
       */

      Given("a top segment s1', three segments s1 that are collinear with a segment s, and a bottom segment s2")
      val s1P = Line((2, 11), (9, 2))
      val s1_1 = Line((3, 8), (6, 5))
      val s1_2 = Line((2, 9), (7, 4))
      val s1_3 = Line((4, 7), (8, 3))
      val s = Line((5, 6), (10, 1))
      val s2 = Line((0, 1), (10, 0))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1_1, s1_2, s1_3, s, s2, s1P)

      Then("there should be a single point intersection between s and s'")
      intersections.count(_._1.isSinglePoint) should be (1)
      val single = intersections.find(_._1.isSinglePoint).get
      single._1.pointIntersection should be (RPoint(s1P.stop))
      segSet(single._2) should be (Set(s1P, s))

      And("there should be 6 segment overlaps between s1:1-3 and s.")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 6

      assert(overlaps.exists(o => o._1.overlap == r(s1_1.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_2)))
      assert(overlaps.exists(o => o._1.overlap == r(s1_3.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_3)))
      assert(overlaps.exists(o => o._1.overlap == r(s.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s)))
      assert(overlaps.exists(o => o._1.overlap == r(s1_3.start, s1_2.stop) && segSet(o._2) == Set(s1_2, s1_3)))
      assert(overlaps.exists(o => o._1.overlap == r(s.start, s1_2.stop) && segSet(o._2) == Set(s1_2, s)))
      assert(overlaps.exists(o => o._1.overlap == r(s.start, s1_3.stop) && segSet(o._2) == Set(s1_3, s)))

    }

    it ("should handle degenerate Source case (i)") {

      /* Same as (h), this test has re-aligned s1' such that it intersects s. */

      Given("an upper segment s1', a lower segment s2 and a segment s beginning at the same coordinate as the collinear segment s1 intersecting s1'.")
      val s1P = Line((1, 11), (7, 2))
      val s1 = Line((2, 8), (8, 2))
      val s = Line((2, 8), (6, 4))
      val s2 = Line((0, 1), (10, 0))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s, s2, s1P)

      Then("a single-coordinate intersection should be found between s1' + s and s1' + s1")
      val pointIntersections = intersections.filter(_._1.isSinglePoint)
      pointIntersections should have size 1
      assert(pointIntersections.exists(i => i._1.pointIntersection == s.intersection(s1P).get.pointIntersection && segSet(i._2) == Set(s1P, s, s1)))

      And("an overlap between s1 and s should be detected")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 1
      assert(overlaps.exists(o => o._1.overlap == r(s.start, s.stop) && segSet(o._2) == Set(s, s1)))

    }

    /*
     * Target case
     */

    // Unsure of what the figure in a) means, but nothing in the general case needs to be tested since it is covered by other cases

    it ("should handle the degenerate Target case (b)") {

      Given("an upper segment s1, a lower segment s2, and a segment s that ends in s1 from above")
      val s1 = Line((2, 5), (11, 8))
      val s = Line((0, 8), (5, 6))
      val s2 = Line((1, 4), (10, 1))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s, s2)

      Then("an intersection between s and s1 should be detected")
      intersections should have size 1
      val intersection = intersections.head
      intersection._1.isSinglePoint should be (true)
      intersection._1.pointIntersection should be (s.intersection(s1).get.pointIntersection)
      segSet(intersection._2) should be (Set(s, s1))

    }

    it ("should handle the degenerate Target case (c)") {

      Given("an upper segment s1 and a lower segment s2, and a segment s that ends in s2 from below")
      val s1 = Line((2, 5), (11, 8))
      val s = Line((0, 1), (4, 3))
      val s2 = Line((1, 4), (10, 1))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s, s2)

      Then("an intersection between s and s2 should be detected")
      intersections should have size 1
      val intersection = intersections.head
      intersection._1.isSinglePoint should be (true)
      intersection._1.pointIntersection should be (s.intersection(s2).get.pointIntersection)
      segSet(intersection._2) should be (Set(s, s2))

    }

    it ("should handle the degenerate Target case (d)") {

      Given("upper segment s1, lower segment s2 and middle segment s forming a pencil ending at the same point")
      val s1 = Line((2, 9), (8, 5))
      val s = Line((1, 7), (8, 5))
      val s2 = Line((3, 3), (8, 5))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s, s2)

      Then("an intersection between s1, s and s2 should be detected")
      intersections should have size 1
      val intersection = intersections.head
      intersection._1.isSinglePoint should be (true)
      intersection._1.pointIntersection should be (s.intersection(s1).get.pointIntersection)
      intersection._1.pointIntersection should be (s.intersection(s2).get.pointIntersection)
      intersection._1.pointIntersection should be (s1.intersection(s2).get.pointIntersection)
      segSet(intersection._2) should be (Set(s, s1, s2))

    }

    it ("should handle the degenerate Target case (e)") {

      Given("an upper segment s1', a lower segment s2', three collinear segments s2 between s1' and s2' and " +
            "a segment s ending in s2 from below")
      val s1P = Line((1, 11), (11, 10))
      val s2P = Line((0, 0), (10, 2))
      val s = Line((2, 3), (6, 6))
      val s2_1 = Line((3, 9), (7, 5))
      val s2_2 = Line((4, 8), (8, 4))
      val s2_3 = Line((5, 7), (9, 3))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1P, s2P, s, s2_1, s2_2, s2_3)

      Then("a point intersection between s2:1-3 and s should be found")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      points.head._1.pointIntersection should be (RPoint(s.stop))
      points.head._2.toSet should be (Set(s, s2_1, s2_2, s2_3))

      And("three interval overlaps should be found between s2:1-3")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 3
      assert(overlaps.exists(o => o._1.overlap == r(s2_2.start, s2_1.stop) && segSet(o._2) == Set(s2_1, s2_2)))
      assert(overlaps.exists(o => o._1.overlap == r(s2_3.start, s2_1.stop) && segSet(o._2) == Set(s2_1, s2_3)))
      assert(overlaps.exists(o => o._1.overlap == r(s2_3.start, s2_2.stop) && segSet(o._2) == Set(s2_2, s2_3)))

    }

    it ("should handle the degenerate Target case (f)") {

      Given("an upper segment s1', a lower segment s2', three collinear segments s2 between s1' and s2' and " +
            "a segment s ending in s2 from above")
      val s1P = Line((1, 11), (11, 10))
      val s2P = Line((0, 0), (10, 2))
      val s = Line((2, 8), (5, 5))
      val s2_1 = Line((1, 1), (6, 6))
      val s2_2 = Line((2, 2), (7, 7))
      val s2_3 = Line((4, 4), (9, 9))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1P, s2P, s, s2_1, s2_2, s2_3)

      Then("a point intersection between s2:1-3 and s should be found")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      points.head._1.pointIntersection should be (RPoint(s.stop))
      points.head._2.toSet should be (Set(s, s2_1, s2_2, s2_3))

      And("three interval overlaps should be found between s2:1-3")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 3
      assert(overlaps.exists(o => o._1.overlap == r(s2_2.start, s2_1.stop) && segSet(o._2) == Set(s2_1, s2_2)))
      assert(overlaps.exists(o => o._1.overlap == r(s2_3.start, s2_1.stop) && segSet(o._2) == Set(s2_1, s2_3)))
      assert(overlaps.exists(o => o._1.overlap == r(s2_3.start, s2_2.stop) && segSet(o._2) == Set(s2_2, s2_3)))

    }

    it ("should handle the degenerate Target case (g)") {

      Given("an upper segment s1', a lower segment s2' and three collinear segments s, s1, s2")
      val s1P = Line((2, 10), (6, 11))
      val s2P = Line((0, 1), (11, 0))
      val s = Line((1, 8), (4, 5))
      val s1 = Line((2, 7), (6, 3))
      val s2 = Line((3, 6), (8, 1))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1P, s2P, s, s1, s2)

      And("three interval overlaps should be found between s, s1 and s2")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 3
      assert(overlaps.exists(o => o._1.overlap == r(s1.start, s.stop) && segSet(o._2) == Set(s, s1)))
      assert(overlaps.exists(o => o._1.overlap == r(s2.start, s.stop) && segSet(o._2) == Set(s, s2)))
      assert(overlaps.exists(o => o._1.overlap == r(s2.start, s1.stop) && segSet(o._2) == Set(s2, s1)))

    }

    it ("should handle the degenerate Target case (h)") {

      Given("an upper segment s1', a lower segment s2', and three collinear segments s:1-3 cutting s1' from above, where s1 has its target in s1'")
      val s1P = Line((1, 2), (10, 5))
      val s2P = Line((2, 0), (11, 1))
      val s1 = Line((2, 9), (7, 4))
      val s2 = Line((3, 8), (8, 3))
      val s3 = Line((4, 7), (8, 3))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1P, s2P, s1, s2, s3)

      Then("a point intersection between s1-3 and s1' should be found")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      points.head._1.pointIntersection should be (RPoint(s1.stop))
      points.head._2.toSet should be (Set(s1, s2, s3, s1P))

      And("three interval overlaps should be found between s1, s2 and s3")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 3
      assert(overlaps.exists(o => o._1.overlap == r(s2.start, s1.stop) && segSet(o._2) == Set(s1, s2)))
      assert(overlaps.exists(o => o._1.overlap == r(s3.start, s1.stop) && segSet(o._2) == Set(s1, s3)))
      assert(overlaps.exists(o => o._1.overlap == r(s3.start, s3.stop) && segSet(o._2) == Set(s2, s3)))

    }

    it ("should handle the degenerate Target case (i)") {

      Given("two non-collinear segments s2 and s, where s2's target point is the source of s")
      val s2 = Line((2, 9), (5, 6))
      val s = Line((5, 6), (9, 5))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s, s2)

      Then("a point intersection between s and s2 should be found")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      points.head._1.pointIntersection should be (RPoint(s2.stop))
      segSet(points.head._2) should be (Set(s, s2))

    }

    it ("should handle the general Intersection case (a)") {

      Given("an upper segment v', a lower segment u', a middle segment v and a segment u that intersects v and u'.")
      val vP = Line((1, 9), (10, 11))
      val uP = Line((2, 2), (11, 1))
      val v = Line((4, 5), (8, 6))
      val u = Line((5, 7), (10, 0))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(vP, uP, v, u)

      Then("two intersections should be found between u + v and u + u'")
      intersections should have size 2
      intersections.map(_._1.isSinglePoint) should have size 2
      assert(intersections.exists(i => i._1.pointIntersection == u.intersection(v).get.pointIntersection && segSet(i._2) == Set(u, v)))
      assert(intersections.exists(i => i._1.pointIntersection == u.intersection(uP).get.pointIntersection && segSet(i._2) == Set(u, uP)))

    }

    it ("should handle the degenerate Intersection case (b)") {

      Given("an upper segment v', a lower segment u' and a pencil of three segments p1-3")
      val vP = Line((1, 9), (10, 11))
      val uP = Line((2, 2), (11, 1))
      val p1 = Line((4, 8), (8, 4))
      val p2 = Line((4, 6), (7, 6))
      val p3 = Line((3, 4), (9, 8))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(vP, uP, p1, p2, p3)

      Then("a single intersection between p1-3 should be found")
      intersections should have size 1
      assert(intersections.head._1.isSinglePoint)
      assert(intersections.head._1.pointIntersection == RPoint(6, 6))
      assert(segSet(intersections.head._2) == Set(p1, p2, p3))

    }

    it ("should handle the degenerate Intersection case (c)") {

      Given("an upper segment v', a middle segment u, and a lower segment v that intersects u from below")
      val vP = Line((2, 8), (9, 10))
      val u = Line((1, 8), (10, 1))
      val v = Line((1, 2), (5, 5))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(vP, u, v)

      Then("a single intersection between u and v should be found")
      intersections should have size 1
      assert(intersections.head._1.isSinglePoint)
      assert(intersections.head._1.pointIntersection == u.intersection(v).get.pointIntersection)
      assert(segSet(intersections.head._2) == Set(u, v))

    }

    // Intersection case (d) is covered by Target case (d)

    it ("should handle the degenerate Intersection case (e)") {

      Given("an upper segment v', a lower segment u', a middle segment v and three collinear segments u1-3 intersecting v")
      val vP = Line((1, 10), (10, 9))
      val uP = Line((1, 1), (10, 2))
      val v = Line((2, 4), (8, 6))
      val u1 = Line((2, 8), (6, 4))
      val u2 = Line((3, 7), (8, 2))
      val u3 = Line((4, 6), (7, 3))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(vP, uP, v, u1, u2, u3)

      Then("a single intersection between v and u1-3 should be found")
      val points = intersections.filter(_._1.isSinglePoint)
      points should have size 1
      assert(intersections.head._1.isSinglePoint)
      assert(intersections.head._1.pointIntersection == RPoint(5, 5))
      assert(segSet(intersections.head._2) == Set(v, u1, u2, u3))

      And("three overlaps between u1-3 should be found")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 3
      assert(overlaps.exists(o => o._1.overlap == r(u2.start, u1.stop) && segSet(o._2) == Set(u1, u2)))
      assert(overlaps.exists(o => o._1.overlap == r(u3.start, u1.stop) && segSet(o._2) == Set(u1, u3)))
      assert(overlaps.exists(o => o._1.overlap == r(u3.start, u3.stop) && segSet(o._2) == Set(u2, u3)))

    }

    // Intersection case (f) is covered by Target case (f)

  }

  /* Detects if a vector contains duplicates before converting it to a set. */
  private def segSet[L <: Line](vs: Vector[L]): Set[L] = {
    val unique = vs.distinct
    assert(unique.size == vs.size, "Duplicate entries found in segment intersection vector: " + vs.filter(n => vs.count(_ == n) > 1))
    unique.toSet
  }

}
