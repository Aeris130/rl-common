package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.collections.RBTree
import net.cyndeline.rlcommon.math.geom.{DPoint, Line, Point}
import net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann.SweepLine.LineEntry

class NeighbourLookupSpec extends SpecImports{
  private implicit val leOrdering = SweepLine.buildOrdering[Line]

  /* Used to ease writing specs for non-collinear neighbors. A fixed tree is used, and each test can decide
   * which of its nodes should be collinear or not. This way different tree setups can be done without having to
   * tinker with the underlying RBTree class in order to generate a tree that fits each spec. The tree contains
   * 30 nodes, arranged as such:
   *
   *                                              16
   *                      8                                               24
   *          4                     12                        20                        28
   *     2         6          10        14              18        22             26           30
   *   1   3     5    7     9    11   13   15        17    19   21   23        25   27      29   31
   *
   * Since collinearity is supposed to be determined in each test, slopes are arbitrary.
   */
  private val specTree = new {
    val defaultX = DPoint(1, 1)
    val s1 = LineEntry(defaultX, Segment(1, (0, 30), (1, 1)))
    val s2 = LineEntry(defaultX, Segment(2, (0, 30), (1, 2)))
    val s3 = LineEntry(defaultX, Segment(3, (0, 30), (1, 3)))
    val s4 = LineEntry(defaultX, Segment(4, (0, 30), (1, 4)))
    val s5 = LineEntry(defaultX, Segment(5, (0, 30), (1, 5)))
    val s6 = LineEntry(defaultX, Segment(6, (0, 30), (1, 6)))
    val s7 = LineEntry(defaultX, Segment(7, (0, 30), (1, 7)))
    val s8 = LineEntry(defaultX, Segment(8, (0, 30), (1, 8)))
    val s9 = LineEntry(defaultX, Segment(9, (0, 30), (1, 9)))
    val s10 = LineEntry(defaultX, Segment(10, (0, 30), (1, 10)))
    val s11 = LineEntry(defaultX, Segment(11, (0, 30), (1, 11)))
    val s12 = LineEntry(defaultX, Segment(12, (0, 30), (1, 12)))
    val s13 = LineEntry(defaultX, Segment(13, (0, 30), (1, 13)))
    val s14 = LineEntry(defaultX, Segment(14, (0, 30), (1, 14)))
    val s15 = LineEntry(defaultX, Segment(15, (0, 30), (1, 15)))
    val s16 = LineEntry(defaultX, Segment(16, (0, 30), (1, 16)))
    val s17 = LineEntry(defaultX, Segment(17, (0, 30), (1, 17)))
    val s18 = LineEntry(defaultX, Segment(18, (0, 30), (1, 18)))
    val s19 = LineEntry(defaultX, Segment(19, (0, 30), (1, 19)))
    val s20 = LineEntry(defaultX, Segment(20, (0, 30), (1, 20)))
    val s21 = LineEntry(defaultX, Segment(21, (0, 30), (1, 21)))
    val s22 = LineEntry(defaultX, Segment(22, (0, 30), (1, 22)))
    val s23 = LineEntry(defaultX, Segment(23, (0, 30), (1, 23)))
    val s24 = LineEntry(defaultX, Segment(24, (0, 30), (1, 24)))
    val s25 = LineEntry(defaultX, Segment(25, (0, 30), (1, 25)))
    val s26 = LineEntry(defaultX, Segment(26, (0, 30), (1, 26)))
    val s27 = LineEntry(defaultX, Segment(27, (0, 30), (1, 27)))
    val s28 = LineEntry(defaultX, Segment(28, (0, 30), (1, 28)))
    val s29 = LineEntry(defaultX, Segment(29, (0, 30), (1, 29)))
    val s30 = LineEntry(defaultX, Segment(30, (0, 30), (1, 30)))
    val s31 = LineEntry(defaultX, Segment(31, (0, 30), (1, 31)))

    // Pad the array by 1 to keep the id's matching
    val entries = Vector(null, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31)
    val tree = RBTree.build(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31)
    val nLookup = new NeighbourLookup(tree, entries)(SweepLine.buildOrdering[Line])

    require(tree.value == s16 && tree.subTree(s16).left.value == s8 && tree.subTree(s16).right.value == s24)
      require(tree.subTree(s8).left.value == s4 && tree.subTree(s8).right.value == s12)
        require(tree.subTree(s4).left.value == s2 && tree.subTree(s4).right.value == s6)
          require(tree.subTree(s2).left.value == s1 && tree.subTree(s2).right.value == s3)
          require(tree.subTree(s6).left.value == s5 && tree.subTree(s6).right.value == s7)
        require(tree.subTree(s12).left.value == s10 && tree.subTree(s12).right.value == s14)
          require(tree.subTree(s10).left.value == s9 && tree.subTree(s10).right.value == s11)
          require(tree.subTree(s14).left.value == s13 && tree.subTree(s14).right.value == s15)
      require(tree.subTree(s24).left.value == s20 && tree.subTree(s24).right.value == s28)
        require(tree.subTree(s20).left.value == s18 && tree.subTree(s20).right.value == s22)
          require(tree.subTree(s18).left.value == s17 && tree.subTree(s18).right.value == s19)
          require(tree.subTree(s22).left.value == s21 && tree.subTree(s22).right.value == s23)
        require(tree.subTree(s28).left.value == s26 && tree.subTree(s28).right.value == s30)
          require(tree.subTree(s26).left.value == s25 && tree.subTree(s26).right.value == s27)
          require(tree.subTree(s30).left.value == s29 && tree.subTree(s30).right.value == s31)
  }

  private val triple = new {
    val f = specTree
    val s1 = f.s1
    val s2 = f.s2
    val s3 = f.s3
    val entries = Vector(null, s1, s2, s3)
    val tree = RBTree.build(s1, s2, s3)
    val nLookup = new NeighbourLookup(tree, entries)(SweepLine.buildOrdering[Line])
    require(tree.value == s2 && tree.left.value == s1 && tree.right.value == s3)
  }

  describe("NeighbourLookup") {

    it ("should find neighbors for a segment stored in a leaf node with a lower parent") {

      Given("a tree with entry s being the highest value in the left sub tree")
      val x = DPoint(5, 5)
      val sP = LineEntry(DPoint(3, 3), Segment(0, Point(3, 10), Point(10, 9)))
      val s1 = LineEntry(DPoint(2, 2), Segment(1, Point(2, 8), Point(8, 6)))
      val s = LineEntry(x, Segment(2, Point(5, 7), Point(9, 10)))
      val s2 = LineEntry(DPoint(1, 1), Segment(3, Point(1, 5), Point(9, 4)))
      val entries = Vector(sP, s1, s, s2) // Matches id's
      val tree = RBTree.build(s2, s1, sP, s)(SweepLine.buildOrdering[Line])

      // S1 should be root, with a left child s2 having a right child s. s' should be the right child of s1.
      require(tree.value == s1 && tree.subTree(s).left.isEmpty && tree.subTree(s).right.isEmpty)

      When("computing neighbors for s")
      val neighbors = new NeighbourLookup(tree, entries)(SweepLine.buildOrdering[Line]).aboveAndBelow(s.segment)

      Then("s1 should be the upper neighbor")
      neighbors._1 should be (Some(s1.segment))

      And("s2 should be the lower neighbor")
      neighbors._2 should be (Some(s2.segment))

    }

    it ("should find neighbors for a segment stored in a leaf node with an upper parent") {

      Given("a tree with entry s1 being the lowest value in the right sub tree")
      val s0 = LineEntry(DPoint(2, 2), Segment(0, Point(2, 10), Point(9, 8)))
      val s1 = LineEntry(DPoint(2, 2), Segment(1, Point(2, 7), Point(9, 5)))
      val s2 = LineEntry(DPoint(2, 2), Segment(2, Point(2, 4), Point(9, 2)))
      val s3 = LineEntry(DPoint(2, 2), Segment(3, Point(2, 2), Point(9, 0)))
      val entries = Vector(s0, s1, s2, s3) // Matches id's
      val tree = RBTree.build(s3, s2, s0, s1)(SweepLine.buildOrdering[Line])

      require(tree.value == s2 && tree.subTree(s0).left.value == s1)

      When("computing neighbors for s1")
      val neighbors = new NeighbourLookup(tree, entries)(SweepLine.buildOrdering[Line]).aboveAndBelow(s1.segment)

      Then("s0 should be the upper neighbor")
      neighbors._1 should be (Some(s0.segment))

      And("s2 should be the lower neighbor")
      neighbors._2 should be (Some(s2.segment))

    }

    /*
     *
     * Lowest upper non-collinear neighbor
     *
     */

    it ("should select the leftmost child in a sub tree that lies to the right of a collinear segment") {

      Given("the spec tree where segments 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 are collinear with s (12)")
      val f = specTree
      import f._
      val collinear = segmentSet(s8, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25)

      When("computing the lowest upper non-collinear neighbor")
      val n = nLookup.closestNonCollinear(s12.segment, true, isCollinear(collinear))

      Then("segment 26 should be returned")
      n should be (Some(s26.segment))

    }

    it ("should select the root as upper neighbor if no upper child exists in a collinear segment, and all non-collinear " +
        "segments below the root are below the first found collinear segment") {

      Given("the spec tree where segments 10, 12, 13, 15 are collinear with s (14)")
      val f = specTree
      import f._
      val collinear = segmentSet(s10, s12, s13, s14, s15)

      When("computing the lowest upper non-collinear neighbor")
      val n = nLookup.closestNonCollinear(s14.segment, true, isCollinear(collinear))

      Then("segment 16 should be returned")
      n should be (Some(s16.segment))

    }

    it ("should select the parent as neighbor of a collinear segment whose right sub tree consists of only collinear segments") {

      Given("the spec tree where segments 20, 21, 22, 23 are collinear with s (20)")
      val f = specTree
      import f._
      val s = s20
      val collinear = segmentSet(s, s21, s22, s23)

      When("computing the lowest upper non-collinear neighbor")
      val n = nLookup.closestNonCollinear(s.segment, true, isCollinear(collinear))

      Then("segment 24 should be returned")
      n should be (Some(s24.segment))

    }

    it ("should select the child of a collinear segment if both the parent and child are non-collinear and greater than the segment") {

      Given("the spec tree where segments 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21 are collinear with s (20)")
      val f = specTree
      import f._
      val s = s20
      val collinear = segmentSet(s8, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s, s21)

      When("computing the lowest upper non-collinear neighbor")
      val n = nLookup.closestNonCollinear(s.segment, true, isCollinear(collinear))

      Then("segment 22 should be returned")
      n should be (Some(s22.segment))

    }

    it ("should return the upper parent if the root of the tree is a collinear segment, and no upper right non-collinear children exists") {

      Given("the spec tree where segments 16, 17, 18, 19, 20, 21, 22, 23 are collinear with s (20)")
      val f = specTree
      import f._
      val s = s20
      val collinear = segmentSet(s16, s17, s18, s19, s, s21, s22, s23)

      When("computing the lowest upper non-collinear neighbor")
      val n = nLookup.closestNonCollinear(s.segment, true, isCollinear(collinear))

      Then("the upper parent of segment 20 should be returned")
      n should be (Some(s24.segment))

    }

    it ("should not return a match if the uppermost segment is collinear with s") {

      Given("the spec tree where segments 30 and 31 are collinear")
      val f = specTree
      import f._
      val s = s31
      val collinear = segmentSet(s30, s)

      When("computing the lowest upper non-collinear neighbor")
      val n = nLookup.closestNonCollinear(s.segment, true, isCollinear(collinear))

      Then("no neighbor should be returned")
      n should be ('empty)

    }

    /*
     * All collinear segments
     */

    it ("should return a segment that is collinear with some segment s, occupies > 1 coordinate and only shares an endpoint") {

      Given("a segment s and a collinear segment v")
      val s = LineEntry(DPoint(4, 4), Segment(0, (2, 2), (4, 4)))
      val v = LineEntry(DPoint(4, 4), Segment(1, (4, 4), (6, 6)))
      val tree = RBTree.build(s, v)
      val lookup = new NeighbourLookup(tree, Vector(s, v))(SweepLine.buildOrdering[Line])

      When("looking up all collinear segments for s")
      val collinear = lookup.findAllCollinearSegments(s.segment)

      Then("no segments should be found")
      collinear should be (Seq(v.segment))

    }

    /*
     * Closest valid segment
     */

    it ("should return the segment closest to the root if it is valid") {

      Given("the spec tree where segments 8 and 24 are valid")
      val f = specTree
      import f._

      When("finding the segments closest to 16")
      val valid = isValid(Set(s8, s24)) _
      val above = nLookup.findClosest(s16.segment, true, valid)
      val below = nLookup.findClosest(s16.segment, false, valid)

      Then("segments 8 and 24 should be found")
      above should be (Some(s24.segment))
      below should be (Some(s8.segment))

    }

    it ("should traverse to the closest bottom child when looking for valid segments") {

      Given("the spec tree where segments 8, 24, 15 and 17 are valid")
      val f = specTree
      import f._
      val valid = isValid(Set(s8, s24, s15, s17)) _

      When("finding the segments closest to 16")
      val above = nLookup.findClosest(s16.segment, true, valid)
      val below = nLookup.findClosest(s16.segment, false, valid)

      Then("segments 15 and 17 should be found")
      above should be (Some(s17.segment))
      below should be (Some(s15.segment))

    }

    it ("should include parents in the segment search") {

      Given("the spec tree where segments 16 and 24 are valid")
      val f = specTree
      import f._
      val valid = isValid(Set(s16, s24)) _

      When("finding the segments closest to 28")
      val below = nLookup.findClosest(s28.segment, false, valid)

      Then("segment 24 should be found")
      below should be (Some(s24.segment))

    }

  }

  private def isCollinear(set: Set[Segment[Line]])(s1: Segment[Line], s2: Segment[Line]): Boolean = set.contains(s1) && set.contains(s2)
  private def isValid(set: Set[LineEntry[Line]])(s: Segment[Line]) = set.map(_.segment).contains(s)
  private def segmentSet(entries: LineEntry[Line]*) = entries.map(_.segment).toSet

}
