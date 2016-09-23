package net.cyndeline.rlcommon.math.geom

import net.cyndeline.rlcommon.SpecImports

import scala.language.implicitConversions

class RPointSpec extends SpecImports {

  /* Moving a point involves angles (i.e the irrational number PI), hence some degree of accuracy loss is enevitable. */
  private class FuzzyRPoint(p: RPoint) extends RPoint(p.x, p.y) {
    def compares(other: RPoint): Boolean = {
      val eps = 1.0E-10
      (other.x - x).abs < eps && (other.y - y).abs < eps
    }
  }
  private implicit def touchFuzzyGetDizzy(p: RPoint): FuzzyRPoint = new FuzzyRPoint(p)

  it ("should move itself upwards") {

    Given("a point (1,1)")
    val p = RPoint(1, 1)

    When("moving the point 90 degrees by 2")
    val moved = p.move(90, 2)

    Then("the new point should be at (1,3)")
    assert(moved.compares(RPoint(1, 3)))

  }

  it ("should move itself downwards") {

    Given("a point (1,2)")
    val p = RPoint(1, 2)

    When("moving the point -90 degrees by 3")
    val moved = p.move(-90, 3)

    Then("the new point should be at (1,-1)")
    assert(moved.compares(RPoint(1, -1)))

  }

  it ("should move itself diagonally") {

    Given("a point (1,1)")
    val p = RPoint(1, 1)

    When("moving the point 45 degrees by 5.65...")
    val moved = p.move(45, Math.sqrt(32)) // a2 + b2 == c2, so to adjust p by 4 on each axis, sqrt(32) must be the distance

    println("Delta x: " + (p.x - moved.x).abs.toInt)

    Then("the new point should be at (5,5)")
    val expected = RPoint(5, 5)
    assert(moved.compares(expected), s"$moved was not $expected")

  }

}
