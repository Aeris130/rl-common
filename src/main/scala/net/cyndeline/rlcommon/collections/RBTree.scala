package net.cyndeline.rlcommon.collections

/**
  * Implements a functional version of red-black trees using the balancing algorithm by C.Okasaki in the article
  * "Functional pearls, red black trees in a functional setting". As Okasaki omits element deletion in his algorithm,
  * the bubble-algorithm by K.Germane and M.Might described in "Functional Pearl, Deletion: The curse of the red-black
  * tree" is used instead.
  *
  * Red-black trees are self-balancing binary search trees that are capable of doing insertion, deletion and lookups in
  * O(log n) time.
  */
abstract sealed class RBTree[+E] {

  /** The value stored in the root of this tree. */
  def value: E

  /** The color of this tree node. */
  def color: Color

  /** The left sub-tree of this tree. */
  def left: RBTree[E]

  /** The right sub-tree of this tree. */
  def right: RBTree[E]

  /** @return True if this tree is a leaf node. It will not contain values or sub-trees. */
  def isEmpty: Boolean

  /**
    * Inserts a value into the tree.
    * @param value Value to insert.
    * @param ordering Specifies lt/gt relations between tree values.
    * @return A copy of this tree with the specified value inserted, or this tree if the value was already in it.
    */
  def insert[A >: E](value: A)(implicit ordering: Ordering[A]): RBTree[A] = {
    def add(tree: RBTree[A]): RBTree[A] = {
      if (tree.isEmpty)
        RBTree.build(value, Red)
      else if (ordering.lt(value, tree.value))
        balance(tree.value, tree.color, add(tree.left), tree.right)
      else if (ordering.gt(value, tree.value))
        balance(tree.value, tree.color, tree.left, add(tree.right))
      else
        tree
    }

    blacken(add(this))
  }

  /**
    * Deletes a value from the tree.
    * @param value Value to delete.
    * @param ordering Specifies lt/gt relations between tree values.
    * @return A copy of the tree with the node containing the specified value deleted, or this tree if no such
    *         value was found.
    */
  def delete[A >: E](value: A)(implicit ordering: Ordering[A]): RBTree[A] = {
    def bubble(v: A, color: Color, left: RBTree[A], right: RBTree[A]): RBTree[A] = if (isBB(left) || isBB(right)) {
      balance(v, blacker(color), redder(left), redder(right))
    } else {
      balance(v, color, left, right)
    }

    def remove(tree: RBTree[A]): RBTree[A] = tree match {
      case empty @ (BLeaf | BBLeaf) => empty
      case Branch(_, Red, BLeaf, BLeaf) => BLeaf
      case Branch(_, Black, BLeaf, BLeaf) => BBLeaf
      case Branch(_, Black, BLeaf, Branch(x, Red, l, r)) => Branch(x, Black, l, r)
      case Branch(_, Black, Branch(x, Red, l, r), BLeaf) => Branch(x, Black, l, r)
      case Branch(x, color, left, right) => bubble(left.max, color, removeMax(left), right)
    }

    def removeMax(tree: RBTree[A]): RBTree[A] = tree match {
      case BLeaf | BBLeaf => error("Cannot remove maximum from empty tree")
      case s @ Branch(_, _, _, BLeaf) => remove(s)
      case Branch(x, color, l, r) => bubble(x, color, l, removeMax(r))
    }

    def del(tree: RBTree[A]): RBTree[A] = if (tree.isEmpty) {
      tree
    } else if (ordering.lt(value, tree.value)) {
      bubble(tree.value, tree.color, del(tree.left), tree.right)
    } else if (ordering.gt(value, tree.value)) {
      bubble(tree.value, tree.color, tree.left, del(tree.right))
    } else {
      remove(tree)
    }

    blacken(del(this))
  }

  /**
    * @return The largest element in the tree.
    */
  def max: E = this match {
    case BLeaf | BBLeaf => error("Empty tree, no largest element.")
    case Branch(x, _, _, BLeaf) => x
    case Branch(x, _, _, right) => right.max
  }

  /**
    * Returns the root of a sub-tree.
    * @param value Value to search for.
    * @param ordering Specifies lt/gt relations between tree values.
    * @tparam A Type to search for.
    * @return A tree node in this tree whose value is equal to the specified value.
    */
  def subTree[A >: E](value: A)(implicit ordering: Ordering[A]): RBTree[A] = {
    if (isEmpty) {
      throw new NoSuchElementException("No subtree for value " + value + " found.")
    } else {
      if (value == this.value)
        this
      else if (ordering.lt(value, this.value))
        left.subTree(value)
      else
        right.subTree(value)
    }
  }

  /**
    * Counts the number of elements in the tree in O(n) time.
    */
  def size: Int = if (isEmpty) {
    0
  } else {
    1 + left.size + right.size
  }

  /** @return All values in this root and its sub-trees. */
  def values: Vector[E] = {
    def traverse(t: RBTree[E]): Vector[E] = if (t.isEmpty) {
      Vector()
    } else {
      t.value +: (traverse(t.left) ++ traverse(t.right))
    }
    traverse(this)
  }

  /**
    * @param e A value to look for in this sub-tree.
    * @return True if the root or any of its descendants contains e, otherwise false.
    */
  def contains[A >: E](e: A)(implicit ord: Ordering[A]): Boolean = if (isEmpty) {
    false
  } else {
    value == e || (ord.lt(e, value) && left.contains(e)) || (ord.gt(e, value) && right.contains(e))
  }

  private def balance[A >: E : Ordering](v: A, color: Color, left: RBTree[A], right: RBTree[A]): RBTree[A] = (color, left, right) match {
    /* The original 4 cases for Okasaki's insertion */
    // Left case
    case (Black, Branch(v1, Red, Branch(v2, Red, a, b), c), d) => RBTree.build(v1, Red, RBTree.build(v2, Black, a, b), RBTree.build(v, Black, c, d))
    case (Black, Branch(v2, Red, a, Branch(v3, Red, b, c)), d) => RBTree.build(v3, Red, RBTree.build(v2, Black, a, b), RBTree.build(v, Black, c, d))
    // Right case
    case (Black, a, Branch(v1, Red, Branch(v2, Red, b, c), d)) => RBTree.build(v2, Red, RBTree.build(v, Black, a, b), RBTree.build(v1, Black, c, d))
    case (Black, a, Branch(v1, Red, b, Branch(v2, Red, c, d))) => RBTree.build(v1, Red, RBTree.build(v, Black, a, b), RBTree.build(v2, Black, c, d))

    /* 6 additional cases for M.Might's deletion */
    case (DoubleBlack, Branch(y, Red, Branch(x, Red, a, b), c), d) => Branch(y, Black, Branch(x, Black, a, b), Branch(v, Black, c, d))
    case (DoubleBlack, Branch(x, Red, a, Branch(y, Red, b, c)), d) => Branch(y, Black, Branch(x, Black, a, b), Branch(v, Black, c, d))
    case (DoubleBlack, a, Branch(z, Red, Branch(y, Red, b, c), d)) => Branch(y, Black, Branch(v, Black, a, b), Branch(z, Black, c, d))
    case (DoubleBlack, a, Branch(y, Red, b, Branch(z, Red, c, d))) => Branch(y, Black, Branch(v, Black, a, b), Branch(z, Black, c, d))

      // For the last two, the input to redden can't be inferred by the compiler for some reason, thinks it's Any
    case (DoubleBlack, a, Branch(z, NegativeBlack, Branch(y, Black, b, c), d @ (Branch(_, Black, _, _)))) =>
      val dWithType: RBTree[A] = d
      Branch(y, Black, Branch(v, Black, a, b), balance(z, Black, c, redden(dWithType)))
    case (DoubleBlack, Branch(x, NegativeBlack, a @ Branch(_, Black, _, _), Branch(y, Black, b, c)), d) =>
      val aWithType: RBTree[A] = a
      Branch(y, Black, balance(x, Black, redden(aWithType), b), Branch(v, Black, c, d))

    case _ => RBTree.build(v, color, left, right)
  }

  private def blacken[A >: E : Ordering](t: RBTree[A]): RBTree[A] = t match {
    case BLeaf => BLeaf
    case BBLeaf => BLeaf
    case Branch(v, _, left, right) => RBTree.build(v, Black, left, right)
  }

  private def redden[A >: E : Ordering](t: RBTree[A]): RBTree[A] = t match {
    case BLeaf | BBLeaf => error("Cannot color an empty tree red")
    case Branch(v, _, left, right) => Branch(v, Red, left, right)
  }

  private def isBB(t: RBTree[_]): Boolean = t match {
    case BBLeaf | Branch(_, DoubleBlack, _, _) => true
    case  _ => false
  }

  private def blacker(color: Color): Color = color match {
    case NegativeBlack => Red
    case Red => Black
    case Black => DoubleBlack
    case DoubleBlack => error("Cannot darken the color of a double-black tree.")
  }

  private def redder(color: Color): Color = color match {
    case NegativeBlack => error("Trees with color Negative black cannot be made redder.")
    case Red => NegativeBlack
    case Black => Red
    case DoubleBlack => Black
  }

  private def redder[A >: E : Ordering](t: RBTree[A]): RBTree[A] = t match {
    case BBLeaf => BLeaf
    case Branch(v, color, left, right) => Branch(v, redder(color), left, right)
    case BLeaf => error("Unhandled case: Attempted to redden the color of a black leaf.")
  }

  private def error(str: String) = throw new Error(str)
}

/**
  * Factory objects for new trees.
  */
object RBTree {

  /**
    * @tparam E Element type in the tree.
    * @return An empty tree.
    */
  def empty[E]: RBTree[E] = BLeaf

  /**
    * The main build method.
    */
  def build[E : Ordering](e: E, es: E*): RBTree[E] = build(Seq(e) ++ es.toSeq)

  /**
    * The main build method.
    */
  def build[E : Ordering](elements: Seq[E]): RBTree[E] = if (elements.isEmpty) {
    empty[E]
  } else {
    var tree = empty[E].insert(elements.head)
    val eIt = elements.drop(1).iterator
    while (eIt.hasNext) {
      tree = tree.insert(eIt.next())
    }
    tree
  }

  /**
    * Builds a tree node using specified node data, including its left and right sub-tree. Does not guarantee
    * a valid tree.
    */
  private def build[E : Ordering](value: E, color: Color, left: RBTree[E], right: RBTree[E]): RBTree[E] = Branch(value, color, left, right)

  /**
    * Builds an empty tree node with a specified color.
    */
  private def build[E : Ordering](value: E, color: Color): RBTree[E] = Branch(value, color, BLeaf, BLeaf)

}

/**
  * A red-black tree node implemented as a recursive data structure where every node is a sub-tree of its parent.
  * @param value Value stored in the tree node.
  * @param color Color of the node.
  * @param left The nodes left sub-tree, containing entries with a value less than this node's.
  * @param right The nodes right sub-tree, containing entries with a value greater than this node's.
  * @tparam E Element type of the node.
  */
case class Branch[E : Ordering](value: E, color: Color, left: RBTree[E], right: RBTree[E]) extends RBTree[E] {
  override def isEmpty = false
}

/**
  * An empty node, always colored black. Used as leaves in a non-empty tree, and root in an empty tree.
  */
case object BLeaf extends RBTree[Nothing] {
  override def color = Black
  override def value: Nothing = error("Attempted to retrieve value from empty tree.")
  override def left = error("Attempted to retrieve left sub-tree from empty tree.")
  override def right = error("Attempted to retrieve right sub-tree from empty tree.")
  override def isEmpty = true
  private def error(str: String) = throw new NoSuchElementException(str)
}

/**
  * An empty node colored Double Black. Not found in any returned graph, this node is for internal use only.
  */
case object BBLeaf extends RBTree[Nothing] {
  override def color = DoubleBlack
  override def value: Nothing = error("Attempted to retrieve value from empty tree.")
  override def left = error("Attempted to retrieve left sub-tree from empty tree.")
  override def right = error("Attempted to retrieve right sub-tree from empty tree.")
  override def isEmpty = true
  private def error(str: String) = throw new NoSuchElementException(str)
}

/**
  * The color associated with tree nodes. Only Red and Black is used in final trees.
  */
sealed trait Color
case object Red extends Color
case object Black extends Color
/** Extra color used for balancing trees during the deletion process (not found in the resulting tree) */
case object DoubleBlack extends Color
/** Extra color used for balancing trees during the deletion process (not found in the resulting tree) */
case object NegativeBlack extends Color
