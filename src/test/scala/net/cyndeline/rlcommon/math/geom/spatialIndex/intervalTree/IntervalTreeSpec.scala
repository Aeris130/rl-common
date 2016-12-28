package net.cyndeline.rlcommon.math.geom.spatialIndex.intervalTree

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.math.geom.spatialIndex.common.ElementProperty

class IntervalTreeSpec extends SpecImports {

  /**
    * 1D tree of integers
    */
  private def intTree1D = new {
    val tree = IntervalTree.empty(ElementProperty.intProperty)
  }

  private def intTree2D = new {
    val intProp = new ElementProperty[Point]() {
      override val totalDimensions: Int = 2
      override def value(element: Point, dimension: Int) = dimension match {
        case 1 => element.y
        case 2 => element.x
        case _ => fail("Only two dimensions allowed in 2D points.")
      }
      override def distance(a: Point, b: Point) = ???
      override def axisDistance(a: Point, b: Point, dimension: Int) = ???
    }
    implicit val pointOrdering = new Ordering[Point]() {
      override def compare(a: Point, b: Point): Int = if (a.x < b.x) -1
      else if (b.x < a.x) 1
      else if (a.y < b.y) -1
      else if (b.y < a.y) 1
      else 0
    }

    val tree = IntervalTree.empty(intProp)
  }

  /* A tree with three disjoint intervals */
  private def intTripleDisjoint = new {
    val f = intTree1D
    val a = (-3, 1)
    val b = (4, 6)
    val c = (8, 13)
    val tree = f.tree.insert(a._1, a._2).insert(b._1, b._2).insert(c._1, c._2)
  }

  private def sharingCoordinates = new {
    val f = intTree2D
    val i1l = Point(3, 5)
    val i1h = Point(4, 6)
    val i2l = Point(3, 6)
    val i2h = Point(4, 5)
    val tree = f.tree.insert(i1l, i1h).insert(i2l, i2h)
  }

  describe("IntervalTree") {

    /*
     * 1D trees
     */

    it ("should insert an interval") {

      Given("an empty tree")
      val f = intTree1D
      import f._

      When("inserting the interval [3,7]")
      val withInterval = tree.insert(3, 7)

      Then("the interval [3,7] should be present")
      withInterval.values should be (Set((3, 7)))

    }

    it ("should insert multiple disjoint intervals") {

      Given("an empty tree")
      val f = intTree1D
      import f._

      When("inserting the intervals [-1,2], [3,7] and [9,10]")
      val withInterval = tree.insert(3, 7).insert(-1, 2).insert(9, 10)

      Then("intervals [-1,2], [3,7] and [9,10] should be in the tree")
      withInterval.values should be (Set((3, 7), (-1, 2), (9, 10)))

    }

    it ("should search for intervals overlapping an interval") {

      Given("a tree with intervals [3,6] and [9,12]")
      val f = intTree1D
      val tree = f.tree.insert(3, 6).insert(9, 12)

      When("searching for intervals overlapping [6,10]")
      val overlap = tree.search(6, 10)

      Then("both intervals should be returned")
      overlap should be (Set((3, 6), (9, 12)))

    }

    it ("should ignore intervals outside the search range") {

      Given("a tree with three intervals")
      val f = intTripleDisjoint
      import f._

      When("searching using an interval that overlaps the two upper intervals")
      val overlap = tree.search(2, 12)

      Then("the lower interval should not be present in the result")
      overlap should be (Set(b, c))

    }

    it ("should not add duplicates") {

      Given("an empty tree")
      val f = intTree1D
      import f._

      When("adding an interval twice")
      val withInterval = tree.insert(3, 7).insert(3, 7)

      Then("only a single interval should exist")
      withInterval.values should be (Set((3, 7)))

    }

    it ("should search for intervals to the left of a node that lies above the search interval") {

      Given("a tree where the root (4,5) has a left child (4,4) and right child (8,8) which in turn has a left child (5,8)")
      val f = intTree1D
      import f._
      val withIntervals = tree.insert(4, 4).insert(4, 5).insert(8, 8).insert(5, 8)

      When("searching using the range 4-7")
      val result = withIntervals.search(4, 7)

      Then("the intervals (4,4), (4,5) and (5,8) should be found")
      result should be (Set((4,4), (4,5), (5,8)))

    }

    /*
     * 2D
     */

    it ("should add intervals with multiple dimensions") {

      Given("a two-dimensional tree")
      val f = intTree2D
      import f._

      When("adding the intervals [(3,0), (6,4)] and [(7,5), (8,1)]")
      val i1l = Point(3, 0)
      val i1h = Point(6, 4)
      val i2l = Point(7, 5)
      val i2h = Point(8, 1)
      val withIntervals = tree.insert(i1l, i1h).insert(i2l, i2h)

      Then("both intervals should be in the tree")
      withIntervals.values should be (Set((i1l, i1h), (i2l, i2h)))

    }

    it ("should add multiple intervals sharing the same x- and y coordinates") {

      Given("a two-dimensional tree")
      val f = intTree2D
      import f._

      When("adding the intervals [(3,5), (4,6)] and [(3,6), (4,5)]")
      val i1l = Point(3, 5)
      val i1h = Point(4, 6)
      val i2l = Point(3, 6)
      val i2h = Point(4, 5)
      val withIntervals = tree.insert(i1l, i1h).insert(i2l, i2h)

      Then("both intervals should be in the tree")
      withIntervals.values should be (Set((i1l, i1h), (i2l, i2h)))

    }

    it ("should delete entries") {

      Given("a tree with multiple entries")
      val f = intTree2D
      val tree = f.tree
        .insert(Point(4, 2), Point(6, 3))
        .insert(Point(9, 13), Point(12, 2))
        .insert(Point(1, 2), Point(99, 99))
        .insert(Point(4, 9), Point(21, -1))

      When("removing an interval")
      val toRemove = (Point(9, 13), Point(12, 2))
      val removed = tree.delete(toRemove._1, toRemove._2)

      Then("the resulting tree should contain all values except the removed interval")
      removed.values should be (tree.values - toRemove)

    }

    it ("should preserve intervals after another interval sharing its coordinates has been removed") {

      Given("two intervals sharing coordinates x(3,4) and y(5,6)")
      val f = sharingCoordinates
      import f._

      When("deleting one of the intervals")
      val deleted = tree.delete(i1l, i1h)

      Then("the resulting tree should still contain the other interval")
      deleted.values should be (Set((i2l, i2h)))

    }

    it ("should find entries extending past the search range in one dimension, and lies within it in another") {

      Given("the range [(3,2), (7,2)]")
      val start = Point(3, 2)
      val stop = Point(7, 2)
      val f = intTree2D
      val tree = f.tree
        .insert(start, stop)

      When("searching using the interval [(4,1), (6,3)]")
      val result = tree.search(Point(4, 1), Point(6, 3))

      Then("the range should be found")
      result should be (Set((start, stop)))

    }

  }

}
