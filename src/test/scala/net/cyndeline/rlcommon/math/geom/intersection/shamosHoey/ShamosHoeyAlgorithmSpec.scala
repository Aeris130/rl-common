package net.cyndeline.rlcommon.math.geom.intersection.shamosHoey

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.Line
import spire.math.Rational

class ShamosHoeyAlgorithmSpec extends SpecImports {
  implicit def intTupleToRational(t: (Int, Int)): (Rational, Rational) = (Rational(t._1), Rational(t._2))
  private val algorithm = new ShamosHoeyAlgorithm()

  describe("ShamosHoeyAlgorith") {

    it ("should not find intersections between non-intersecting lines") {

      Given("three lines that doesn't intersect")
      val l1 = Line((2, 4), (7, 8))
      val l2 = Line((6, 3), (4, 2))
      val l3 = Line((6, 5), (9, 3))

      When("checking for intersections")
      val hasIntersections = algorithm.hasIntersection(l1, l2, l3)

      Then("o intersection should be found")
      hasIntersections should be (false)

    }

    it ("should find an intersection between two lines") {

      Given("two lines that intersect at a point")
      val l1 = Line((1, 1), (3, 3))
      val l2 = Line((1, 3), (3, 1))

      When("checking for intersections")
      val hasIntersections = algorithm.hasIntersection(l1, l2)

      Then("an intersection should be found")
      hasIntersections should be (true)

    }

    it ("should find an intersection between three lines") {

      Given("two lines that intersect at a point")
      val l1 = Line((1, 1), (3, 3))
      val l2 = Line((1, 3), (3, 1))
      val l3 = Line((1, 2), (3, 2))

      When("checking for intersections")
      val hasIntersections = algorithm.hasIntersection(l1, l2, l3)

      Then("an intersection should be found")
      hasIntersections should be (true)

    }

    it ("should find an overlap between two vertical lines") {

      Given("two vertical overlapping lines")
      val l1 = Line((1, 1), (1, 3))
      val l2 = Line((1, 0), (1, 5))

      When("checking for intersections")
      val hasIntersections = algorithm.hasIntersection(l1, l2)

      Then("an intersection should be found")
      hasIntersections should be (true)

    }

    it ("should find an intersection at the end points of two lines") {

      Given("two segments that intersect at their end points")
      val l1 = Line((1, 1), (3, 3))
      val l2 = Line((5, 5), (3, 3))

      When("checking for intersections")
      val hasIntersections = algorithm.hasIntersection(l1, l2)

      Then("an intersection should be found")
      hasIntersections should be (true)

    }

    it ("should find an intersection between two lines separated by another line at their addition to the queue") {

      Given("two lines separated by a third")
      val l1 = Line((2, 5), (7, 2))
      val l2 = Line((5, 1), (7, 2))
      val l3 = Line((1, 2), (6, 2))

      When("checking for intersections")
      val hasIntersections = algorithm.hasIntersection(l1, l2, l3)

      Then("an intersection should be found")
      hasIntersections should be (true)

    }

  }

}
