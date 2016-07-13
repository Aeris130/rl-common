package net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}

class KDTreeSpec extends SpecImports {

  private val threePointTree = new {
    val p1 = Point(3, 3)
    val p2 = Point(1, 3)
    val p3 = Point(5, 3)
    val tree = KDTree.point2DTree(Vector(p1, p2, p3))
  }

  // Example taken from: https://www.cs.umd.edu/class/spring2002/cmsc420-0401/pbasic.pdf
  private val unbalancedTree = new {
    val root = Point(20, 20)
    val tree = KDTree.point2DTree(Vector(root))
      .insert(Point(10, 30)).insert(Point(25, 50)) // First level
      .insert(Point(35, 25)) // Second level
      .insert(Point(30, 45)).insert(Point(55, 40)) // Third level
      .insert(Point(30, 35)).insert(Point(45, 35)) // Fourth level
      .insert(Point(50, 30)) // Fifth level
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
      val p1 = Point(1, 1)
      val p2 = Point(2, 2)
      val withPoints = tree.insert(p1).insert(p2)

      Then("both points should be in the tree")
      withPoints.contains(p1) should be (true)
      withPoints.contains(p2) should be (true)

    }

    it ("should create a tree with a single element") {

      Given("a list containing a single point")
      val p = Point(0, 0)
      val pv = Vector(p)

      When("creating the tree")
      val tree = KDTree.point2DTree(pv)

      Then("the tree should contain the point")
      tree.contains(p) should be (true)

    }

    it ("should add a children to a node") {

      Given("points (3, 3), (1, 3) and (5, 3)")
      val p1 = Point(3, 3)
      val p2 = Point(1, 3)
      val p3 = Point(5, 3)

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

    it ("should add individual nodes to the tree") {

      Given("a tree with three points")
      val f = threePointTree
      import f._

      When("adding a node that lies to the left of the root, and to the right of the roots left child")
      // Sorting based on the x axis when comparing to the root, and on the y axis when comparing to the child
      val p4 = Point(2, 4)
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
      val tree = KDTree.point2DTree(Vector(Point(51, 75)))
          .insert(Point(25, 40)).insert(Point(70, 70)) // First level
          .insert(Point(10, 30)).insert(Point(35, 90)).insert(Point(55, 1)).insert(Point(60, 80)) // Second level
          .insert(Point(1, 10)).insert(Point(50, 50))

      When("finding the lowest x value")
      val minX = tree.min(1) // 1 = x, 2 = y

      Then("the lowest point should be (1, 10)")
      minX.value should be (Point(1, 10))

    }

    it ("should delete a node") {

      Given("a tree of nodes")
      val f = unbalancedTree
      import f._

      When("removing the root")
      val newRoot = tree.delete(root)

      Then("the new root should be (25, 50) and have l/r neighbors (10, 30)/(35, 25)")
      newRoot.value should be (Point(25, 50))
      newRoot.left.value should be (Point(10, 30))
      newRoot.right.value should be (Point(35, 25))

      And("the point (35, 25) should only have the right neighbor (45, 35)")
      val n35_25 = newRoot.get(Point(35, 25))
      n35_25.left should be ('empty)
      n35_25.right.value should be (Point(45, 35))

      And("the point (45, 35) should have the l/r neighbors (30, 45)/(55, 40)")
      val n45_35 = newRoot.get(Point(45, 35))
      n45_35.left.value should be (Point(30, 45))
      n45_35.right.value should be (Point(55, 40))

      And("the point (30, 45) should have the left neighbor (30, 35)")
      val n30_45 = newRoot.get(Point(30, 45))
      n30_45.left.value should be (Point(30, 35))
      n30_45.right should be ('empty)

      And("the point (55, 40) should have the left neighbor (50, 30)")
      val n55_40 = newRoot.get(Point(55, 40))
      n55_40.left.value should be (Point(50, 30))
      n55_40.right should be ('empty)

    }

    it ("should confirm that a value exists") {

      Given("a tree with the point (35, 25)")
      val f = unbalancedTree
      import f._

      When("checking if the point exists")
      val isContained = tree.contains(Point(35, 25))

      Then("the result should be true")
      isContained should be (true)

    }

    it ("should confirm that a value doesn't exist") {

      Given("a tree without the point (99, 99)")
      val f = unbalancedTree
      import f._

      When("checking if the point exists")
      val isContained = tree.contains(Point(99, 99))

      Then("the result should be false")
      isContained should be (false)

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
      range.toSet should be (Set(Point(35, 25), Point(30, 35), Point(45, 35)))

    }

    it ("remove duplicates") {

      Given("a list of duplicate elements")
      val elements = Vector(Point(3, 4), Point(3, 4), Point(3, 4), Point(3, 4))

      When("creating a tree using the elements")
      val tree = KDTree.point2DTree(elements)

      Then("the tree should have size 1")
      tree.values should have length 1

      And("the root should be the specified point")
      tree.value should be (Point(3, 4))

    }

    it ("should balance itself") {

      Given("a tree with a root, and the root having the rest of the tree in its right sub tree child")
      val root = Point(0, 0)
      val p1 = Point(1, 1)
      val p2 = Point(2, 2)
      val p3 = Point(3, 3)
      val p4 = Point(5, 5)
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

      Given("a tree with a rectangle set")
      val f = rectangleSet
      import f._
      val tree = KDTree.rectangleTree(elements)

      When("searching using a range that shares a segment with r3")
      val range = Rectangle(Point(0, 7), 3, 2)
      val inRange = tree.rangeSearch(range)

      Then("rectangle 3 should be in range")
      inRange should be (Seq(r3))

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

  }

}
