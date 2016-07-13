package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.{DPoint, Line, Point}

class BentleyOttmannIntersectionSpec extends SpecImports {
  private val algorithm = new BentleyOttmannIntersection()

  // The regular line class considers lines with the same coordinate identical.
  class IDSegment(val id: Int, start: Point, stop: Point) extends Line(start, stop) {

    def this(id: Int) = this(id, Point(3, 3), Point(5, 3))

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

      assert(intersections.exists(o => o._1.overlap == (b.start, a.stop) && segSet(o._2) == Set(a, b)))
      assert(intersections.exists(o => o._1.overlap == (c.start, a.stop) && segSet(o._2) == Set(a, c)))
      assert(intersections.exists(o => o._1.overlap == (d.start, a.stop) && segSet(o._2) == Set(a, d)))
      assert(intersections.exists(o => o._1.overlap == (e.start, a.stop) && segSet(o._2) == Set(a, e)))
      assert(intersections.exists(o => o._1.overlap == (c.start, b.stop) && segSet(o._2) == Set(b, c)))
      assert(intersections.exists(o => o._1.overlap == (d.start, b.stop) && segSet(o._2) == Set(b, d)))
      assert(intersections.exists(o => o._1.overlap == (e.start, b.stop) && segSet(o._2) == Set(b, e)))
      assert(intersections.exists(o => o._1.overlap == (d.start, c.stop) && segSet(o._2) == Set(c, d)))
      assert(intersections.exists(o => o._1.overlap == (e.start, c.stop) && segSet(o._2) == Set(c, e)))
      assert(intersections.exists(o => o._1.overlap == (e.start, d.stop) && segSet(o._2) == Set(d, e)))

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
      assert(intersections.exists(o => o._1.overlap == (start, stop) && segSet(o._2) == Set(a, b, c, d, e)))

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
      assert(intersections.exists(o => o._1.overlap == (s1.start, s1.stop) && o._2 == Vector(s1, s2, s3)))

    }

    it ("should add every overlap with the same coordinates to the same intersection even if the segment source/target coordinates differ") {

      Given("two collinear segments with the same source/target, and a third collinear segment with differing coordinates")
      val s1 = new IDSegment(1, Point(3, 3), Point(5, 3))
      val s2 = new IDSegment(2, Point(3, 3), Point(5, 3))
      val differs = new IDSegment(3, Point(0, 3), Point(7, 3))

      When("computing the intersections of the set")
      val intersections = algorithm.computeIntersections(s1, s2, differs)

      Then("1 overlap should be detected having all three segments")
      intersections should have size 1
      assert(!intersections.exists(_._1.isSinglePoint))
      val start = s1.start
      val stop = s2.stop
      assert(intersections.exists(o => o._1.overlap == (start, stop) && segSet(o._2) == Set(s1, s2, differs)))

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

      assert(intersections.exists(o => o._1.overlap == (s2.start, s2.stop) && segSet(o._2) == Set(s1, s2)))
      assert(intersections.exists(o => o._1.overlap == (s3.start, s3.stop) && segSet(o._2) == Set(s1, s2, s3)))

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
      intersection._1.pointIntersection should be (s.start.toDouble)
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
      intersection._1.pointIntersection should be (s.start.toDouble)
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
        i._1.pointIntersection == s.start.toDouble &&
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
      intersection._1.pointIntersection should be (s.start.toDouble)
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
      assert(singlePoints.exists(i => i._1.pointIntersection == s.start.toDouble && segSet(i._2) == Set(s1_1, s1_2, s1_3, s)))

      And("there should be three overlapping segments between s1_1 + s1_2, s1_1 + s1_3 and s1_2 + s1_3")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 3
      assert(overlap.exists(o => o._1.overlap == (s1_3.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_3)))
      assert(overlap.exists(o => o._1.overlap == (s1_3.start, s1_2.stop) && segSet(o._2) == Set(s1_2, s1_3)))
      assert(overlap.exists(o => o._1.overlap == (s1_1.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_2)))

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
      assert(singlePoints.exists(i => i._1.pointIntersection == s.start.toDouble && segSet(i._2) == Set(s1_1, s1_2, s1_3, s)))

      And("there should be three overlapping segments between s1_1 + s1_2, s1_1 + s1_3 and s1_2 + s1_3")
      val overlap = intersections.filter(_._1.isInterval)
      overlap should have size 3
      assert(overlap.exists(o => o._1.overlap == (s1_1.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_2)))
      assert(overlap.exists(o => o._1.overlap == (s1_3.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_3)))
      assert(overlap.exists(o => o._1.overlap == (s1_3.start, s1_2.stop) && segSet(o._2) == Set(s1_2, s1_3)))

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
      single._1.pointIntersection should be (s1P.stop.toDouble)
      segSet(single._2) should be (Set(s1P, s))

      And("there should be 6 segment overlaps between s1:1-3 and s.")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 6

      assert(overlaps.exists(o => o._1.overlap == (s1_1.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_2)))
      assert(overlaps.exists(o => o._1.overlap == (s1_3.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s1_3)))
      assert(overlaps.exists(o => o._1.overlap == (s.start, s1_1.stop) && segSet(o._2) == Set(s1_1, s)))
      assert(overlaps.exists(o => o._1.overlap == (s1_3.start, s1_2.stop) && segSet(o._2) == Set(s1_2, s1_3)))
      assert(overlaps.exists(o => o._1.overlap == (s.start, s1_2.stop) && segSet(o._2) == Set(s1_2, s)))
      assert(overlaps.exists(o => o._1.overlap == (s.start, s1_3.stop) && segSet(o._2) == Set(s1_3, s)))

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
      assert(overlaps.exists(o => o._1.overlap == (s.start, s.stop) && segSet(o._2) == Set(s, s1)))

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
      points.head._1.pointIntersection should be (s.stop.toDouble)
      points.head._2.toSet should be (Set(s, s2_1, s2_2, s2_3))

      And("three interval overlaps should be found between s2:1-3")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 3
      assert(overlaps.exists(o => o._1.overlap == (s2_2.start, s2_1.stop) && segSet(o._2) == Set(s2_1, s2_2)))
      assert(overlaps.exists(o => o._1.overlap == (s2_3.start, s2_1.stop) && segSet(o._2) == Set(s2_1, s2_3)))
      assert(overlaps.exists(o => o._1.overlap == (s2_3.start, s2_2.stop) && segSet(o._2) == Set(s2_2, s2_3)))

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
      points.head._1.pointIntersection should be (s.stop.toDouble)
      points.head._2.toSet should be (Set(s, s2_1, s2_2, s2_3))

      And("three interval overlaps should be found between s2:1-3")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 3
      assert(overlaps.exists(o => o._1.overlap == (s2_2.start, s2_1.stop) && segSet(o._2) == Set(s2_1, s2_2)))
      assert(overlaps.exists(o => o._1.overlap == (s2_3.start, s2_1.stop) && segSet(o._2) == Set(s2_1, s2_3)))
      assert(overlaps.exists(o => o._1.overlap == (s2_3.start, s2_2.stop) && segSet(o._2) == Set(s2_2, s2_3)))

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
      assert(overlaps.exists(o => o._1.overlap == (s1.start, s.stop) && segSet(o._2) == Set(s, s1)))
      assert(overlaps.exists(o => o._1.overlap == (s2.start, s.stop) && segSet(o._2) == Set(s, s2)))
      assert(overlaps.exists(o => o._1.overlap == (s2.start, s1.stop) && segSet(o._2) == Set(s2, s1)))

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
      points.head._1.pointIntersection should be (s1.stop.toDouble)
      points.head._2.toSet should be (Set(s1, s2, s3, s1P))

      And("three interval overlaps should be found between s1, s2 and s3")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 3
      assert(overlaps.exists(o => o._1.overlap == (s2.start, s1.stop) && segSet(o._2) == Set(s1, s2)))
      assert(overlaps.exists(o => o._1.overlap == (s3.start, s1.stop) && segSet(o._2) == Set(s1, s3)))
      assert(overlaps.exists(o => o._1.overlap == (s3.start, s3.stop) && segSet(o._2) == Set(s2, s3)))

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
      points.head._1.pointIntersection should be (s2.stop.toDouble)
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
      assert(intersections.head._1.pointIntersection == DPoint(6, 6))
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
      assert(intersections.head._1.pointIntersection == DPoint(5, 5))
      assert(segSet(intersections.head._2) == Set(v, u1, u2, u3))

      And("three overlaps between u1-3 should be found")
      val overlaps = intersections.filter(_._1.isInterval)
      overlaps should have size 3
      assert(overlaps.exists(o => o._1.overlap == (u2.start, u1.stop) && segSet(o._2) == Set(u1, u2)))
      assert(overlaps.exists(o => o._1.overlap == (u3.start, u1.stop) && segSet(o._2) == Set(u1, u3)))
      assert(overlaps.exists(o => o._1.overlap == (u3.start, u3.stop) && segSet(o._2) == Set(u2, u3)))

    }

    // Intersection case (f) is covered by Target case (f)

  }

  /* Detects if a vector contains duplicates before converting it to a set. */
  private def segSet(vs: Vector[Line]): Set[Line] = {
    val unique = vs.distinct
    assert(unique.size == vs.size, "Duplicate entries found in segment intersection vector: " + vs.filter(n => vs.count(_ == n) > 1))
    unique.toSet
  }

}
