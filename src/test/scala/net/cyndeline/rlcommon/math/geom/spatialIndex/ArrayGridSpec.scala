package net.cyndeline.rlcommon.math.geom.spatialIndex

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.Line
import spire.math.Rational

class ArrayGridSpec extends SpecImports {
  private implicit def toRat(t: (Int, Int)): (Rational, Rational) = (Rational(t._1), Rational(t._2))

  /* A grid with a single cell, containing three segments */
  private def singleCellGrid = new {
    val l1 = Line((0, 0), (9, 9))
    val l2 = Line((3, 2), (3, 8))
    val l3 = Line((4, 6), (2, 8))
    val grid = ArrayGrid(1, 10).add(l1, l2, l3)
  }

  /* A 10x10 grid, using the same segments as above, but with x10 coordinates. */
  private def multiCellGrid = new {
    val l1 = Line((0, 0), (90, 90))
    val l2 = Line((30, 20), (30, 80))
    val l3 = Line((40, 60), (20, 80))
    val grid = ArrayGrid(10, 10).add(l1, l2, l3)
  }

  describe("ArrayGrid") {

    it ("should report size of a single cell") {

      Given("a grid with one cell")
      val f = singleCellGrid
      import f._

      When("checking its size")
      val size = grid.size

      Then("the size should be 3")
      size should be (3)

    }

    it ("should store lines in a single cell") {

      Given("a grid with one cell and three lines")
      val f = singleCellGrid
      import f._

      When("checking existence of the three lines")
      Then("all three lines should exist")
      grid.contains(l1) should be (true)
      grid.contains(l2) should be (true)
      grid.contains(l3) should be (true)

      And("it should not contain a non-inserted line")
      grid.contains(Line((2, 5), (6, 1))) should be (false)

    }

    it ("should confirm intersections with a single cell") {

      Given("a grid with one cell and three lines")
      val f = singleCellGrid
      import f._

      When("checking intersections with a line that intersects l1 and l2")
      val intersections = grid.intersections(Line((5, 4), (2, 6)))

      Then("l1 and l2 should be returned")
      intersections should have size 2
      intersections.toSet should be (Set(l1, l2))

    }

    it ("should reject intersections with a single cell") {

      Given("a grid with one cell and three lines")
      val f = singleCellGrid
      import f._

      When("checking intersections with a line that doesn't have any intersections in the grid")
      val intersections = grid.intersections(Line((4, 5), (6, 7)))

      Then("no intersections should be found")
      intersections should be ('empty)

    }

    it ("should confirm intersections with multiple cells") {

      Given("a 10x10 grid with three segments")
      val f = multiCellGrid
      import f._

      When("checking intersections with a line that intersects l1 and l2")
      val intersections = grid.intersections(Line((50, 40), (20, 60)))

      Then("l1 and l2 should be returned")
      intersections should have size 2
      intersections.toSet should be (Set(l1, l2))

    }

    it ("should reject intersections with multiple cells") {

      Given("a 10x10 grid with three segments")
      val f = multiCellGrid
      import f._

      When("checking intersections with a line that doesn't have any intersections in the grid")
      val intersections = grid.intersections(Line((40, 50), (60, 70)))

      Then("no intersections should be found")
      intersections should be ('empty)

    }

    it ("should delete segments from the grid") {

      Given("a 10x10 grid with three segments")
      val f = multiCellGrid
      import f._

      When("removing lines 1 and 3")
      val removed = grid.remove(l1, l3)

      Then("the resulting grid should have size 1")
      removed.size should be (1)

      And("only segment 2 should be returned with queries")
      removed.intersections(Line((40, 10), (20, 90))).toSet should be (Set(l2))

      And("contains should no longer validate with the lines")
      removed.contains(l1) should be (false)
      removed.contains(l3) should be (false)

    }

    it ("should throw an exception when querying using segments that has negative coordinates") {

      Given("a 10x10 grid")
      val f = multiCellGrid
      import f._

      When("adding or searching for a segment ending at y = -1")
      val line = Line((20, 30), (50, -1))

      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        grid.intersections(line)
      }
      intercept[IllegalArgumentException] {
        grid.intersects(line)
      }
      intercept[IllegalArgumentException] {
        grid.add(line)
      }

    }

    it ("should throw an exception when querying using segments that exceed he grid range") {

      Given("a 10x10 grid")
      val f = multiCellGrid
      import f._

      When("adding or searching for a segment ending at y = 100")
      val line = Line((20, 30), (50, 100))

      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        grid.intersections(line)
      }
      intercept[IllegalArgumentException] {
        grid.intersects(line)
      }
      intercept[IllegalArgumentException] {
        grid.add(line)
      }

    }

  }



}
