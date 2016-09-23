package net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.{Point, RPoint, Rectangle}

class KDTreeSpec extends SpecImports {

  private val threePointTree = new {
    val p1 = RPoint(3, 3)
    val p2 = RPoint(1, 3)
    val p3 = RPoint(5, 3)
    val tree = KDTree.point2DTree(Vector(p1, p2, p3))
  }

  // Example taken from: https://www.cs.umd.edu/class/spring2002/cmsc420-0401/pbasic.pdf
  private val unbalancedTree = new {
    val root = RPoint(20, 20)
    val tree = KDTree.point2DTree(Vector(root))
      .insert(RPoint(10, 30)).insert(RPoint(25, 50)) // First level
      .insert(RPoint(35, 25)) // Second level
      .insert(RPoint(30, 45)).insert(RPoint(55, 40)) // Third level
      .insert(RPoint(30, 35)).insert(RPoint(45, 35)) // Fourth level
      .insert(RPoint(50, 30)) // Fifth level
  }

  private val rectangleSet = new {
    val r1 = Rectangle(Point(0, 0), 3, 3)
    val r2 = Rectangle(Point(1, 1), 4, 4) // Overlaps r1
    val r3 = Rectangle(Point(0, 5), 4, 3)
    val r4 = Rectangle(Point(4, 6), 3, 3)
    val r5 = Rectangle(Point(6, 2), 6, 3)
    val r6 = Rectangle(Point(11, 6), 4, 5)
    val elements = Vector(r1, r2, r3, r4, r5, r6)
  }

  // Various points
  private def neighborTree = new {
    val pa = RPoint(1, 1)
    val pb = RPoint(2, 3)
    val pc = RPoint(6, 2)
    val pd = RPoint(9, 6)
    val pe = RPoint(1, 11)
    val pf = RPoint(6, 9)
    val pg = RPoint(6, 3)
    val tree = KDTree.point2DTree(Vector(pa, pb, pc, pd, pe, pf, pg))

    assert(tree.value == pc)
    assert(tree.left.value == pb && tree.right.value == pd)
    assert(tree.left.left.value == pa && tree.left.right.value == pe)
    assert(tree.right.left.value == pg && tree.right.right.value == pf)
  }

  // Same as the neighbor tree, but with each point being a wxh rectangle instead.
  private def singleRectangleTree(width: Int, height: Int) = new {
    val f = neighborTree
    import f._
    val ra = Rectangle(pa, width, height)
    val rb = Rectangle(pb, width, height)
    val rc = Rectangle(pc, width, height)
    val rd = Rectangle(pd, width, height)
    val re = Rectangle(pe, width, height)
    val rf = Rectangle(pf, width, height)
    val rg = Rectangle(pg, width, height)
    val tree = KDTree.rectangleTree(Vector(ra, rb, rc, rd, re, rf, rg))

    assert(tree.value == rc)
    assert(tree.left.value == rb && tree.right.value == rd)
    assert(tree.left.left.value == ra && tree.left.right.value == re)
    assert(tree.right.left.value == rg && tree.right.right.value == rf)
  }

  describe ("KDTree") {

    it ("should create an empty tree") {

      Given("the KDTree factory object")
      val f = KDTree

      When("creating an empty tree")
      val tree = f.point2DTree(Vector())

      Then("the tree should be empty")
      tree.isEmpty should be (true)

    }

    it ("should add elements to an empty tree") {

      Given("an empty tree")
      val tree = KDTree.point2DTree(Vector())

      When("adding points to the tree")
      val p1 = RPoint(1, 1)
      val p2 = RPoint(2, 2)
      val withPoints = tree.insert(p1).insert(p2)

      Then("both points should be in the tree")
      withPoints.contains(p1) should be (true)
      withPoints.contains(p2) should be (true)

    }

    it ("should create a tree with a single element") {

      Given("a list containing a single point")
      val p = RPoint(0, 0)
      val pv = Vector(p)

      When("creating the tree")
      val tree = KDTree.point2DTree(pv)

      Then("the tree should contain the point")
      tree.contains(p) should be (true)

    }

    it ("should add a children to a node") {

      Given("points (3, 3), (1, 3) and (5, 3)")
      val p1 = RPoint(3, 3)
      val p2 = RPoint(1, 3)
      val p3 = RPoint(5, 3)

      When("creating the tree using the points")
      val tree = KDTree.point2DTree(Vector(p1, p2, p3))

      Then("the root should be point 1")
      tree.value should be (p1)

      And("point 2 should be the left neighbor of point 1")
      tree.left.value should be (p2)

      And("point 3 should be the right neighbor of point 1")
      tree.right.value should be (p3)

      And("the tree should contain all three points")
      tree.contains(p1) should be (true)
      tree.contains(p2) should be (true)
      tree.contains(p3) should be (true)

    }

    it ("should add an element e to a tree with an element sharing the initial property of e") {

      Given("two points with x value 4")
      val p1 = RPoint(4, 5)
      val p2 = RPoint(4, 6)

      When("creating the tree using the points")
      val tree = KDTree.point2DTree(Vector(p1)).insert(p2)

      Then("the tree should contain both points")
      tree.values.toSet should be (Set(p1, p2))

    }

    it ("should add individual nodes to the tree") {

      Given("a tree with three points")
      val f = threePointTree
      import f._

      When("adding a node that lies to the left of the root, and to the right of the roots left child")
      // Sorting based on the x axis when comparing to the root, and on the y axis when comparing to the child
      val p4 = RPoint(2, 4)
      val newTree = tree.insert(p4)

      Then("the tree should contain the new point")
      newTree.contains(p4) should be (true)

      And("the new point should be the right neighbor of p2")
      val p2Node = newTree.left
      p2Node.right.value should be (p4)

    }

    it ("should not add duplicates to the tree") {

      Given("a tree with three points")
      val f = threePointTree
      import f._

      When("adding duplicates of the current points to the tree")
      val withDuplicates = tree.insert(p1).insert(p1).insert(p2).insert(p3).insert(p2).insert(p3)

      Then("the new tree should contain the same node set as the original tree")
      tree should equal (withDuplicates)

    }

    it ("should find the lowest value for some dimension") {

      Given("a tree where the lowest x value is in the leftmost leaf node")
      // Tree is built manually to ensure that every node ends up one the correct side
      val tree = KDTree.point2DTree(Vector(RPoint(51, 75)))
          .insert(RPoint(25, 40)).insert(RPoint(70, 70)) // First level
          .insert(RPoint(10, 30)).insert(RPoint(35, 90)).insert(RPoint(55, 1)).insert(RPoint(60, 80)) // Second level
          .insert(RPoint(1, 10)).insert(RPoint(50, 50))

      When("finding the lowest x value")
      val minX = tree.min(1) // 1 = x, 2 = y

      Then("the lowest point should be (1, 10)")
      minX.value should be (RPoint(1, 10))

    }

    it ("should delete a node") {

      Given("a tree of nodes")
      val f = unbalancedTree
      import f._

      When("removing the root")
      val newRoot = tree.delete(root)

      Then("the new root should be (25, 50) and have l/r neighbors (10, 30)/(35, 25)")
      newRoot.value should be (RPoint(25, 50))
      newRoot.left.value should be (RPoint(10, 30))
      newRoot.right.value should be (RPoint(35, 25))

      And("the point (35, 25) should only have the right neighbor (45, 35)")
      val n35_25 = newRoot.get(RPoint(35, 25))
      n35_25.left should be ('empty)
      n35_25.right.value should be (RPoint(45, 35))

      And("the point (45, 35) should have the l/r neighbors (30, 45)/(55, 40)")
      val n45_35 = newRoot.get(RPoint(45, 35))
      n45_35.left.value should be (RPoint(30, 45))
      n45_35.right.value should be (RPoint(55, 40))

      And("the point (30, 45) should have the left neighbor (30, 35)")
      val n30_45 = newRoot.get(RPoint(30, 45))
      n30_45.left.value should be (RPoint(30, 35))
      n30_45.right should be ('empty)

      And("the point (55, 40) should have the left neighbor (50, 30)")
      val n55_40 = newRoot.get(RPoint(55, 40))
      n55_40.left.value should be (RPoint(50, 30))
      n55_40.right should be ('empty)

    }

    it ("should delete elements sharing common properties") {

      Given("a tree with four rectangles starting at point 2")
      val r1 = Rectangle(Point(2, 2), 1, 2)
      val r2 = Rectangle(Point(2, 3), 1, 4)
      val r3 = Rectangle(Point(2, 3), 5, 4)
      val r4 = Rectangle(Point(2, 2), 5, 2)
      val tree = KDTree.rectangleTree(Vector(r1, r2, r3, r4))

      When("deleting all elements in the tree")
      val d1 = tree.delete(r1)
      val d2 = d1.delete(r2)
      val d3 = d2.delete(r3)
      val d4 = d3.delete(r4)

      Then("the tree should be empty")
      assert(d4.isEmpty, "Not all elements were deleted from the tree")

    }

    it ("should confirm that a value exists") {

      Given("a tree with the point (35, 25)")
      val f = unbalancedTree
      import f._

      When("checking if the point exists")
      val isContained = tree.contains(RPoint(35, 25))

      Then("the result should be true")
      isContained should be (true)

    }

    it ("should confirm that a value doesn't exist") {

      Given("a tree without the point (99, 99)")
      val f = unbalancedTree
      import f._

      When("checking if the point exists")
      val isContained = tree.contains(RPoint(99, 99))

      Then("the result should be false")
      isContained should be (false)

    }

    it ("should confirm that a value doesn't exists even though it partially contains some of the values attributes") {

      Given("a tree with the point (2,2)")
      val root = RPoint(2, 2)
      val tree = KDTree.point2DTree(Vector(root))

      When("checking if it contains (2,3)")
      val contains23 = tree.contains(RPoint(2, 3))

      Then("the result should be false")
      contains23 should be (false)

    }

    /*
     * Point range search
     */

    it ("should return a range of point elements") {

      Given("a tree of nodes")
      // See method for tree description
      val f = unbalancedTree
      import f._

      When("searching for the range (30, 20) -> (45, 35)")
      val range = tree.rangeSearch(Rectangle(Point(30, 20), 16, 16))

      Then("the result should contain elements (35,25), (30,35), (45,35)")
      range should have size 3
      range.toSet should be (Set(RPoint(35, 25), RPoint(30, 35), RPoint(45, 35)))

    }

    it ("remove duplicates") {

      Given("a list of duplicate elements")
      val elements = Vector(RPoint(3, 4), RPoint(3, 4), RPoint(3, 4), RPoint(3, 4))

      When("creating a tree using the elements")
      val tree = KDTree.point2DTree(elements)

      Then("the tree should have size 1")
      tree.values should have length 1

      And("the root should be the specified point")
      tree.value should be (RPoint(3, 4))

    }

    it ("should balance itself") {

      Given("a tree with a root, and the root having the rest of the tree in its right sub tree child")
      val root = RPoint(0, 0)
      val p1 = RPoint(1, 1)
      val p2 = RPoint(2, 2)
      val p3 = RPoint(3, 3)
      val p4 = RPoint(5, 5)
      val tree = KDTree.point2DTree(Vector(root)).insert(p1).insert(p2).insert(p3).insert(p4)

      When("balancing the tree")
      val balanced = tree.balance

      Then("the new tree should contain all the old points")
      balanced.values should have size 5
      balanced.values.toSet should be (Set(root, p1, p2, p3, p4))

      And("point 2 should be the root")
      val bRoot = balanced
      bRoot.value should be (p2)

      And("the root should have 2 values on each side")
      bRoot.left.values should have size 2
      bRoot.right.values should have size 2

    }

    /*
     *
     * Rectangle tests
     *
     */

    it ("should create a tree with a single rectangle") {

      Given("a single rectangle")
      val r = Rectangle(Point(0, 0), 4, 7)

      When("creating the tree")
      val tree = KDTree.rectangleTree(Vector(r))

      Then("the tree should contain the rectangle")
      tree.values should have size 1
      tree.value should be (r)

    }

    it ("should perform an orthogonal search on a rectangle set") {

      Given("a tree with a rectangle set")
      val f = rectangleSet
      import f._
      val tree = KDTree.rectangleTree(elements)

      When("searching using a rectangle that intersects rectangles 4 and 5")
      val range = Rectangle(Point(5, 1), 4, 8)
      val inRange = tree.rangeSearch(range)

      Then("rectangles 4 and 5 should be in range")
      inRange should have size 2
      inRange.toSet should be (Set(r4, r5))

    }

    it ("should find the rightmost rectangle") {

      Given("a tree with a rectangle set")
      val f = rectangleSet
      import f._
      val tree = KDTree.rectangleTree(elements)

      When("searching using a rectangle that intersects rectangle 6")
      val range = Rectangle(Point(10, 5), 4, 4)
      val inRange = tree.rangeSearch(range)

      Then("rectangle 6 should be in range")
      inRange should have size 1
      inRange should be (Seq(r6))

    }

    it ("should find a rectangle that matches the range") {

      Given("a tree with a rectangle set")
      val f = rectangleSet
      import f._
      val tree = KDTree.rectangleTree(elements)

      When("searching using the range of r4")
      val inRange = tree.rangeSearch(r4)

      Then("rectangle 4 should be in range")
      inRange should be (Seq(r4))

    }

    it ("should report tangential rectangles as intersecting") {

      Given("a tree with a rectangle starting at (0,5) with width 4 and height 3")
      val f = rectangleSet
      import f._
      val tree = KDTree.rectangleTree(Vector(r3))

      When("searching using ranges that shares a segment with r3")
      val rangeAbove = Rectangle(Point(0, 7), 3, 2)
      val rangeLeft = Rectangle(Point(-1, 6), 2, 3)
      val rangeRight = Rectangle(Point(3, 4), 3, 2)
      val rangeBottom = Rectangle(Point(0, 5), 4, 1)
      assert(rangeAbove.overlaps(r3))
      assert(rangeLeft.overlaps(r3))
      assert(rangeRight.overlaps(r3))
      assert(rangeBottom.overlaps(r3))
      val inRange = Seq(tree.rangeSearch(rangeAbove), tree.rangeSearch(rangeLeft), tree.rangeSearch(rangeRight), tree.rangeSearch(rangeBottom)).flatten

      Then("rectangle 3 should be in range for all queries")
      inRange should be (Seq(r3, r3, r3, r3))

    }

    it ("should report rectangles sharing a corner as intersecting") {

      Given("a tree with a rectangle set")
      val f = rectangleSet
      import f._
      val tree = KDTree.rectangleTree(elements)

      When("searching using a range that shares a corner with r3")
      val range = Rectangle(Point(3, 7), 1, 1)
      val inRange = tree.rangeSearch(range)

      Then("rectangle 3 should be in range")
      inRange should be (Seq(r3))

    }

    it ("should report a rectangle that is encompassed by the range as intersecting") {

      Given("a tree with a rectangle set")
      val f = rectangleSet
      import f._
      val tree = KDTree.rectangleTree(elements)

      When("searching using a range that covers r5")
      val range = Rectangle(Point(5, 1), 8, 5)
      val inRange = tree.rangeSearch(range)

      Then("rectangle 5 should be in range")
      inRange should be (Seq(r5))

    }

    it ("should report every rectangle in the tree as intersecting") {

      Given("a tree with a rectangle set")
      val f = rectangleSet
      import f._
      val tree = KDTree.rectangleTree(elements)

      When("searching using a range that covers the tree")
      val range = Rectangle(Point(-1, -1), 20, 20)
      val inRange = tree.rangeSearch(range)

      Then("every rectangle should be in range")
      inRange should have size elements.size
      inRange.toSet should be (elements.toSet)

    }

    it ("should not return any results when the range is empty") {

      Given("a range not covering any rectangles in a tree")
      val f = rectangleSet
      import f._
      val tree = KDTree.rectangleTree(elements)
      val range = Rectangle(Point(7, 6), 3, 3)

      When("searching for rectangles intersecting the range")
      val inRange = tree.rangeSearch(range)

      Then("no rectangles should be found")
      inRange should be ('empty)

    }

    /*
     *
     * Nearest neighbor searches
     *
     */

    it ("should not find neighbors in an empty tree") {

      Given("an empty tree")
      val empty = KDTree.point2DTree(Vector())

      When("searching for nearest neighbors of some point")
      val neighbors = empty.nearestNeighbor(3, Point(4, 12))

      Then("no neighbors should be found")
      neighbors should be ('empty)

    }

    it ("should throw an exception if a negative amount of neighbors are specified to be retrieved") {

      Given("a tree")
      val empty = KDTree.point2DTree(Vector())

      When("trying to find the -1 nearest neighbors")
      Then("an exception should be thrown")
      val thrown = intercept[IllegalArgumentException] {
        empty.nearestNeighbor(-1, Point(11, 22))
      }
      thrown.getMessage should be ("requirement failed: Number of neighbors sought must be >= 0.")

    }

    it ("should not find neighbors in a tree with no elements") {

      Given("an empty tree")
      val empty = KDTree.point2DTree(Vector())

      When("trying to find the k nearest neighbors")
      val result = empty.nearestNeighbor(5, Point(11, 22))

      Then("no neighbors should be found")
      result should be ('empty)

    }

    it ("should find 0 neighbors in a tree with elements") {

      Given("a tree with a single point p")
      val p = RPoint(4, 8)
      val empty = KDTree.point2DTree(Vector(p))

      When("searching for 0 neighbors")
      val neighbors = empty.nearestNeighbor(0, Point(6, 12))

      Then("no neighbors should be found")
      neighbors should be ('empty)

    }

    it ("should find a neighbor in a tree with a single neighbor") {

      Given("a tree with a single point p")
      val p = RPoint(4, 8)
      val empty = KDTree.point2DTree(Vector(p))

      When("searching for nearest neighbors of some point that differs from the one in the tree")
      val neighbors = empty.nearestNeighbor(3, Point(6, 12))

      Then("p should be found")
      neighbors should be (Vector(p))

    }

    it ("should find multiple neighbors of a point") {

      Given("a tree with the points (6,2) and (6,3)")
      val f = neighborTree
      import f._

      When("looking for the 2 nearest neighbors of (7,1)")
      val neighbors = tree.nearestNeighbor(2, Point(7, 1))

      Then("(6,2) and (6,3) should be found")
      neighbors should have size 2
      neighbors.toSet should be (Set(pc, pg))

    }

    it ("should find all neighbors when k is greater than the amount of points in the tree") {

      Given("a tree with 7 points")
      val f = neighborTree
      import f._

      When("looking for 8 neighbors")
      val neighbors = tree.nearestNeighbor(8, Point(7, 1))

      Then("every point should be found")
      neighbors should have size 7
      neighbors.toSet should be (Set(pa, pb, pc, pd, pe, pf, pg))

    }

    it ("should find two neighbors with the same distance to the search point") {

      Given("a tree with two points within the same distance of (4,4)")
      val p1 = RPoint(4, 0)
      val p2 = RPoint(4, 8)
      val tree = KDTree.point2DTree(Vector(p1, p2))

      When("looking for 2 neighbors from (4,4)")
      val neighbors = tree.nearestNeighbor(8, Point(4, 4))

      Then("both points should be found")
      neighbors should have size 2
      neighbors.toSet should be (Set(p1, p2))

    }

    it ("should include points that overlap with the search point") {

      Given("a tree with the point (1,11)")
      val f = neighborTree
      import f._
      val point = pe

      When("searching for the nearest neighbor of (1,11)")
      val neighbors = tree.nearestNeighbor(1, point)

      Then("(1,11) should be returned")
      neighbors should be (Vector(point))

    }

    it ("should compute neighbors using rectangles") {

      Given("a tree with 1x1 rectangles with points (6,9) and (9,6) closest to (8,7)")
      val f = singleRectangleTree(1, 1)
      import f._

      When("searching for the nearest neighbor of (8,7)")
      val neighbors = tree.nearestNeighbor(2, Rectangle(Point(8, 7), 1, 1))

      Then("the rectangles (6,9) and (9,6) should be found")
      neighbors should have size 2
      neighbors.toSet should be (Set(rd, rf))

    }

    it ("should find multiple neighbors using rectangles") {

      Given("a tree with 7 1x1 rectangles, having (1,11) furthest away from the search point")
      val f = singleRectangleTree(1, 1)
      import f._

      When("searching for the 6 nearest neighbors of (10,0)")
      val neighbors = tree.nearestNeighbor(6, Rectangle(Point(10, 0), 1, 1))

      Then("every point except (1,11) should be found")
      neighbors should have size 6
      neighbors.toSet should be (Set(ra, rb, rc, rd, rf, rg))

    }

    it ("should search for neighbors that overlaps the search rectangle") {

      Given("a tree with 1x1 rectangles and points (2,3), (6,3) and (6,9)")
      val f = singleRectangleTree(1, 1)
      import f._

      When("searching for 4 neighbors using a search rectangle that overlaps those three points")
      val search = Rectangle(rb.start, rf.stop)
      val neighbors = tree.nearestNeighbor(4, search)

      Then("the three points along with the nearest non-overlapping neighbor should be found")
      neighbors should have size 4
      neighbors.toSet should be (Set(rb, rc, rf, rg))

    }

    it ("should look for neighbors using multi-coordinate rectangles and no overlap") {

      Given("a tree with rectangles (1,4)->(3,8) and (10,4)->(12,6) along with the 1x1 point (4,2)")
      val r1 = Rectangle(Point(1,4), Point(3, 8))
      val r2 = Rectangle(Point(10,4), Point(12, 6))
      val p = Rectangle(Point(4,2), 1, 1)
      val tree = KDTree.rectangleTree(Vector(r1, r2, p))

      When("searching for the two nearest neighbors of the rectangle (4,4)->(9,5)")
      val search = Rectangle(Point(4,4), Point(9, 5))
      val neighbors = tree.nearestNeighbor(2, search)

      Then("rectangles 1 and 2 should be returned despite p being closer to the search start point")
      neighbors should have size 2
      neighbors.toSet should be (Set(r1, r2))

    }

    it ("should only return some neighbor if two rectangles overlap at the same distance from the search point") {

      Given("a tree with rectangles (1,4)->(3,8) and (1,5)->(3,8)")
      val r1 = Rectangle(Point(1,4), Point(3, 8))
      val r2 = Rectangle(Point(1,5), Point(3, 8))
      val tree = KDTree.rectangleTree(Vector(r1, r2))

      When("searching for the nearest neighbor of the 1x1 rectangle (4,9)")
      val search = Rectangle(Point(4,9), 1, 1)
      val neighbors = tree.nearestNeighbor(1, search)

      Then("one of the rectangles should be returned")
      neighbors should have size 1
      assert(neighbors == Vector(r1) || neighbors == Vector(r2))

    }

    it ("should approximate the k nearest neighbors") {

      // Write more tests on this if needed, this is just to ensure that it runs
      Given("a tree")
      val f = singleRectangleTree(1, 1)
      import f._

      When("finding the 4 nearest neighbors to some point")
      val neighbors = tree.nearestNeighborApproximation(4, Rectangle(Point(7, 11), 4, 3), 2)

      Then("the neighbors should be returned")
      neighbors should have size 4
      neighbors.toSet should be (Set(rd, re, rf, rg))

    }

  }

}
