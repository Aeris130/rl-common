package net.cyndeline.rlcommon.collections

import net.cyndeline.rlcommon.SpecImports

import scala.util.Random

class RBTreeSpec extends SpecImports {

  /** In order to trigger the insert cases, a tree large enough to require red nodes is needed. */
  private def treeBase = new {
    val tree: RBTree[Int] = RBTree.build[Int](13)
      .insert(8).insert(17) // Level 1
      .insert(1).insert(11).insert(15).insert(25) // Level 2 etc.
      .insert(-3).insert(6).insert(22).insert(20).insert(27)
  }

  /* A tree where 1 == 2 */
  private def customOrdering = new {
    val tree = RBTree.build[Int](1)
    val o = new Ordering[Int]() {
      override def compare(x: Int, y: Int): Int = if (Set(x, y) == Set(1, 2)) 0 else x compareTo y
    }
  }

  describe("RBTree") {

    it ("should add an element as root") {

      Given("an empty tree")
      val tree = RBTree.empty[Int]
      tree should be ('empty)

      When("adding the element 1")
      val element = 1
      val added = tree.insert(element)

      Then("the root should be 1")
      added.value should be (1)
      added.color should be (Black)

    }

    it ("should check the existence of an element") {

      Given("a tree with elements 1, 2, 3, 4, 5, 6")
      val tree = RBTree.build[Int](1, 2, 3, 4, 5, 6)

      When("checking for existence of elements")
      Then("the tree should contain 1, 2, 3, 4, 5, 6")
      tree.contains(1) should be (true)
      tree.contains(2) should be (true)
      tree.contains(3) should be (true)
      tree.contains(4) should be (true)
      tree.contains(5) should be (true)
      tree.contains(6) should be (true)

      And("the tree should not contains values differing from the element set")
      tree.contains(-12) should be (false)
      tree.contains(7) should be (false)
      tree.contains(81) should be (false)

    }

    it ("should check the existence of elements using custom orderings") {

      Given("a tree with element 1 and an ordering where 1 == 2")
      val f = customOrdering
      import f._

      When("checking the existence of 2")
      val exists2 = tree.contains(2)(o)

      Then("the result should be true")
      exists2 should be (true)

    }

    it ("should retrieve sub-trees using custom ordering") {

      Given("a tree with element 1 and an ordering where 1 == 2")
      val f = customOrdering
      import f._

      When("retrieving the sub-tree for 2")
      val sub2 = tree.subTree(2)(o)

      Then("the node for 1 should be returned")
      sub2.value should be (1)

    }

    it ("should return all added elements") {

      Given("a tree with elements 1, 2, 3, 4, 5, 6")
      val tree = RBTree.build[Int](1, 2, 3, 4, 5, 6)

      When("retrieving all elements")
      val elements = tree.values

      Then("6 values should be found")
      elements should have size 6

      And("those elements should be 1, 2, 3, 4, 5, 6")
      elements.toSet should be (Set(1, 2, 3, 4, 5, 6))

    }

    it ("should handle insertion case 1: Black parent with red left child having a red left child") {

      Given("a tree with a black node 1 (left child of 8), having a red left child -3")
      val f = treeBase
      import f._

      When("adding a left child to node -3")
      val newChild = -5
      val case1Tree = tree.insert(newChild)

      Then("the new node should be in the tree")
      case1Tree.subTree(newChild) // Exception if missing

      And("the tree should maintain its Red-Black tree properties")
      treeIsValid(case1Tree)

    }

    it ("should handle insertion case 2: Black parent with red left child having a red right child") {

      Given("a tree with a black node 1 (left child of 8), having a red left child -3")
      val f = treeBase
      import f._

      When("adding a right neighbor to -3")
      val newChild = 0
      val case2Tree = tree.insert(newChild)

      Then("the new node should be in the tree")
      case2Tree.subTree(newChild) // Exception if missing

      And("the tree should maintain its Red-Black tree properties")
      treeIsValid(case2Tree)

    }

    it ("should handle insertion case 3: Black parent with red right child having a red right child") {

      Given("a tree with a black node 25 (right child of 22), having a red right child 27")
      val f = treeBase
      import f._

      When("adding a right neighbor to 27")
      val case3Tree = tree.insert(29)

      Then("the new node should be in the tree")
      case3Tree.subTree(29) // Exception if missing

      And("the tree should maintain its Red-Black tree properties")
      treeIsValid(case3Tree)

    }

    it ("should handle insertion case 4: Black parent with red right child having a red left child") {

      Given("a tree with a black node 25 (right child of 22), having a red right child 27")
      val f = treeBase
      import f._

      When("adding a left neighbor to 27")
      val case4Tree = tree.insert(26)

      Then("the new node should be in the tree")
      case4Tree.subTree(26) // Exception if missing

      And("the tree should maintain its Red-Black tree properties")
      treeIsValid(case4Tree)

    }

    it ("should return its size") {

      Given("trees with 0, 1 and 98 nodes")
      val tree0 = RBTree.empty[Int]
      val tree1 = RBTree.build(1)
      val tree98 = RBTree.build(0, 1 until 98:_*)

      When("checking their size")
      val size0 = tree0.size
      val size1 = tree1.size
      val size98 = tree98.size

      Then("the size should be 0, 1 and 98")
      size0 should be (0)
      size1 should be (1)
      size98 should be (98)

    }

    /*
     *
     * Deletion
     *
     */

    it ("should omit deleting an entry that doesn't exist") {

      Given("a tree")
      val f = treeBase
      import f._

      When("deleting a non-existent entry")
      val afterDeletion = tree.delete(14)

      Then("the tree should remain unchanged")
      tree should equal (afterDeletion)

    }

    it ("should delete every node in a tree") {

      Given("a node amount")
      val amount = 500

      When("building a tree with the specified amount of values and then deleting the values in the same order")
      var tree = RBTree.empty[Int]

      Then("the tree should maintain its Red-Black tree properties at every step")
      for (i <- 0 until amount) {
        tree = tree.insert(i)
      }
      val fullTree = tree
      val fullSize = tree.size
      treeIsValid(fullTree)
      for (i <- 0 until amount) { // Delete in insertion order
        tree = tree.delete(i)
        //  These call will make the test take time if the amount is large, since the test is O(n2).
        treeIsValid(tree)
        tree.size should be (fullSize - i - 1)
      }
      assert(tree.isEmpty)
      tree = fullTree
      val randomOrder = new Random(1).shuffle((0 until amount).toVector)
      for (i <- 0 until amount) { // Delete in random order
        tree = tree.delete(randomOrder(i))
        treeIsValid(tree)
        tree.size should be (fullSize - i - 1)
      }
      assert(tree.isEmpty)

    }

  }

  private def treeIsValid(t: RBTree[Int]): Unit = {
    val nodes = allNodes(t)
    assert(t.color == Black)
    assert(!nodes.exists(n => n.color == DoubleBlack || n.color == NegativeBlack))

    // Red nodes should have black children
    val allRedNodes = nodes.filter(_.color == Red)
    require(!allRedNodes.exists(n => n.left.color == Red || n.right.color == Red), "A red node had a red child.")

    // Black depths should be equal for every path from root to leaf
    blackDepthIsEqual(t)

    // lt/gt relations
    for (n <- nodes if !n.isEmpty) {
      if (!n.left.isEmpty)
        assert(n.left.value < n.value, "BST left-child < parent relation didn't hold.")
      if (!n.right.isEmpty)
        assert(n.right.value > n.value, "BST right-child > parent relation didn't hold.")
    }
  }

  /* Every path from the root to the leaves contain the same number of black nodes. */
  private def blackDepthIsEqual(t: RBTree[Int]): Unit = {
    var firstBlackNodeAmount: Option[Int] = None

    def traverse(current: RBTree[Int], blackAmount: Int): Unit = {
      if (current.isEmpty) {
        if (firstBlackNodeAmount.isEmpty)
          firstBlackNodeAmount = Some(blackAmount)
        else
          require(blackAmount == firstBlackNodeAmount.get, "The path from a node to a leaf contained a different amount of black nodes than a previous path.")
      } else {
        val updatedMount = if (current.color == Black) blackAmount + 1 else blackAmount
        traverse(current.left, updatedMount)
        traverse(current.right, updatedMount)
      }
    }
    traverse(t, 0)
  }

  private def allNodes(t: RBTree[Int]): Vector[RBTree[Int]] = {
    if (t.isEmpty)
      Vector()
    else
      t +: (allNodes(t.left) ++ allNodes(t.right))
  }

}
