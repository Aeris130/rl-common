package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.collections.RBTree
import net.cyndeline.rlcommon.math.geom.Line
import net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann.SweepLine.LineEntry

/**
  * Helper methods for the sweep line.
  */
class NeighbourLookup[L <: Line](data: RBTree[LineEntry[L]], entries: Vector[LineEntry[L]], ord: Ordering[LineEntry[L]]) {

  /**
    * @param segment A segment intersecting the sweep line.
    * @return The neighbors (above, below) the specified segment, or None if this segment is the top- or bottommost
    *         entry on the sweep line.
    */
  def aboveAndBelow(segment: Segment[L],
                    belowValid: Segment[L] => Boolean = defaultValidation _,
                    aboveValid: Segment[L] => Boolean = defaultValidation _): (Option[Segment[L]], Option[Segment[L]]) = {
    val entry = entries(segment.id)
    findNeighbors(entry, null, null, data, belowValid, aboveValid)
  }

  /**
    * Recursively traverses the tree in order to find a segment and return the closest segment below and above it.
    * If the segment lacks a left (right) child, then the parent is used as the lower (higher) neighbor if it occurs
    * on the correct side of the segment in the tree.
    *
    * @param seg The segment to find neighbors for.
    * @param previousHighestLower The highest entry that is lower than the sought segment. Null if no such entry
    *                             was found while searching for the segment so far, or if no such entry was valid.
    *                             I.e, this is the nearest neighbor (below) that should be used if no such neighbor
    *                             exists in the target segments child sub-trees.
    * @param previousLowestHigher Same as above, but the lowest higher neighbor (i.e the closest neighbor above).
    * @param current The current root (may or may not be the segment to look for) to visit, child of 'previous.
    * @param leftIsValid Keeps searching for neighbors below the segment on the sweep line until one is found that
    *                    is validated by this function.
    * @param rightIsValid Keeps searching for neighbors above the segment on the sweep line until one is found that
    *                    is validated by this function.
    */
  private def findNeighbors(seg: LineEntry[L],
                            previousHighestLower: LineEntry[L],
                            previousLowestHigher: LineEntry[L],
                            current: RBTree[LineEntry[L]],
                            leftIsValid: Segment[L] => Boolean = defaultValidation _,
                            rightIsValid: Segment[L] => Boolean = defaultValidation _): (Option[Segment[L]], Option[Segment[L]]) = {
    if (current.isEmpty) {
      (None, None)
    } else if (current.value != seg) {
      if (ord.lt(seg, current.value)) {
        val newPreviousHigh = if (previousLowestHigher == null || ord.lt(current.value, previousLowestHigher)) current.value else previousLowestHigher
        findNeighbors(seg, previousHighestLower, newPreviousHigh, current.left, leftIsValid, rightIsValid)
      } else {
        val newPreviousLow = if (previousHighestLower == null || ord.gt(current.value, previousHighestLower)) current.value else previousHighestLower
        findNeighbors(seg, newPreviousLow, previousLowestHigher, current.right, leftIsValid, rightIsValid)
      }

    } else {

      // Match found
      val below = findHighestInLeftSubtree(current.left, leftIsValid).getOrElse {
        if (previousHighestLower != null && leftIsValid(previousHighestLower.segment))
          previousHighestLower.segment
        else
          null
      }

      val above = findLowestInRightSubtree(current.right, rightIsValid).getOrElse {
        if (previousLowestHigher != null && rightIsValid(previousLowestHigher.segment))
          previousLowestHigher.segment
        else
          null
      }

      (Option(above), Option(below))
    }
  }

  /**
    * This method hinges on a few properties that can be inferred from the definition of the segments and their
    * above/below status.
    *
    * First of all, if there are multiple collinear segments neighbouring each other (which there is, since that's when
    * this method is called during the collinear sub-algorithm in the source case) then they overlap each other.
    * Otherwise all of them wouldn't be cut by the sweep line at the same time.
    *
    * If the collinear segments are overlapping, that means that the first collinear segment in the tree that is found
    * when looking for s contains all other collinear segments in one of its two sub trees (otherwise there would
    * somewhere be a non.collinear segment dividing the collinear segments into two, placing it between them on the
    * sweep line). This sub tree may contain non-collinear segments, but the left (right) tree has its leftmost
    * (rightmost) segments collinear.
    *
    * To find the lowest upper non-collinear neighbor of s, find a collinear segment connected to a non-collinear
    * neighbor greater than itself. This can be done in NlogN time using by calling procA on the root of the BST.
    *
    * procA: Search for segment s in the tree until s or a segment collinear with s is found. Store the lowest upper
    * non-collinear segment (NC) that has been traversed during this search. NC is null if the root of the tree
    * is collinear. Call procB on the collinear segment (C) found this way.
    *
    * procB: From the segment C found using procA, travel right until a collinear segment with a non-collinear
    * child greater than itself is found. If no such child exists (the right-most entry in the sub tree is collinear),
    * check if NC is greater than the current segment. If not (or if NC is null), no upper non-collinear neighbor
    * exists.
    *
    * If a right child exists, call procC on the child and return the result. If not, and NC is not null and greater
    * than the upper collinear segment found, call procC on NC instead.
    *
    * procC: Do an in-order search on a given segment, and don't traverse nodes that are collinear with s or their
    * sub-trees. Return the first result found this way.
    *
    * @param segment The collinear segment s.
    * @param isCollinear Returns true if two segments are collinear. Used to make testing easier.
    * @return The lowest upper non-collinear neighbor of s, or None if s if the uppermost segment or if every neighbor
    *         above it is collinear with s.
    */
  def closestNonCollinearAbove(segment: Segment[L], isCollinear: (Segment[L], Segment[L]) => Boolean = (s1: Segment[L], s2: Segment[L]) => s1.collinearWith(s2)): Option[Segment[L]] = {
    def procA(s: LineEntry[L], current: RBTree[LineEntry[L]], lowestUpper: RBTree[LineEntry[L]]): Option[LineEntry[L]] = if (current.isEmpty) {
      None
    } else if (isCollinear(current.value.segment, s.segment)) {
      // The first collinear segment is found
      procB(s, current, lowestUpper)

    } else {
      val updatedLowestUpper = if (lowestUpper == null && ord.gt(current.value, s)) current
                               /* The current segment needs to be lower than the so-far lowest upper segment found, but
                                * still greater than s to be assigned as the new lowest upper segment in the tree.
                                */
                               else if (lowestUpper != null && ord.lt(current.value, lowestUpper.value) && ord.gt(current.value, s)) current
                               else lowestUpper

      if (ord.lt(s, current.value))
        procA(s, current.left, updatedLowestUpper)
      else
        procA(s, current.right, updatedLowestUpper)
    }

    def procB(s: LineEntry[L], current: RBTree[LineEntry[L]], lowestUpper: RBTree[LineEntry[L]]): Option[LineEntry[L]] =
      if (!current.right.isEmpty && !isCollinear(s.segment, current.right.value.segment)) {
        // The collinear segment with an upper non-collinear neighbor has been found
        val neighborFromChild = procC(s.segment, current)

        if (neighborFromChild.isDefined) {
          Some(neighborFromChild.get.value)
        } else if (lowestUpper != null) {
          Some(procC(s.segment, lowestUpper).get.value)
        } else {
          None
        }

      } else if (!current.right.isEmpty && isCollinear(s.segment, current.right.value.segment)) {
        procB(s, current.right, lowestUpper)
      } else if (current.right.isEmpty && lowestUpper != null) {
        Some(lowestUpper.value)
      } else {
        None // No right non-collinear child, and lowest upper is null
      }

    def procC(s: Segment[L], current: RBTree[LineEntry[L]]): Option[RBTree[LineEntry[L]]] = if (current.isEmpty){
      None
    } else {
      val cs = current.value.segment

      /* If the current node is collinear, the left child can be pruned since it having a greater non-collinear
       * neighbor would put that neighbor between the current node and the child.
       */
      val left: Option[RBTree[LineEntry[L]]] = if (isCollinear(s, cs))
        None
      else
        procC(s, current.left)

      val result = left.getOrElse {
        if (!isCollinear(s, cs))
          current
        else
          procC(s, current.right).orNull
      }
      Option(result)
    }

    val result = procA(entries(segment.id), data, null)
    if (result.isEmpty)
      None
    else
      Some(result.get.segment)
  }

  /**
    * Bounded from above at O(n) running time, where n is the size of the output (the number of collinear segments).
    * This is achieved by pruning any right (left) sub tree whose root is non-collinear and lies above (below) the
    * segment s, as that segment otherwise would be positioned between two or more collinear segments on the sweep line.
    * @param s A segment in the sweep line.
    * @return Every segment on the line collinear with s.
    */
  def findAllCollinearSegments(s: Segment[L]): Vector[Segment[L]] = {
    var result = Vector[Segment[L]]()
    val segment = entries(s.id)

    def search(current: RBTree[LineEntry[L]]): Unit = if (!current.isEmpty) {
      val currentSeg = current.value.segment
      val currentIsCollinearWithS = currentSeg.collinearWith(s)

      /* Keep going left and right regardless of ordering if the current segment is collinear, as only a non-collinear
       * segment would cause a split between collinear neighbors.
       */
      if (currentIsCollinearWithS || !ord.lt(current.value, segment))
        search(current.left)

      if (currentIsCollinearWithS || !ord.gt(current.value, segment))
        search(current.right)

      if (current.value.segment != s && currentIsCollinearWithS)
        result = current.value.segment +: result
    }
    search(data)
    result
  }

  /**
    * In-order traversal that finds the lowest neighbor above a root.
    * @param rightChild The right child of a root (may be empty).
    * @param isValid Function that returns true if a found segment is valid for return.
    * @return The lowest value in the sub-tree starting at the right child, or None if the child is empty or if no
    *         element was valid.
    */
  private def findLowestInRightSubtree(rightChild: RBTree[LineEntry[L]], isValid: Segment[L] => Boolean): Option[Segment[L]] = if (rightChild.isEmpty) {
    None
  } else {
    def inOrder(n: RBTree[LineEntry[L]]): Option[Segment[L]] = if (n.isEmpty) {
      None
    } else {
      val result = inOrder(n.left).getOrElse {
        if (isValid(n.value.segment))
          n.value.segment
        else
          inOrder(n.right).orNull
      }

      Option(result)
    }

    inOrder(rightChild)
  }

  /**
    * Reversed In-order traversal that finds the highest neighbor below a root.
    * @param leftChild The left child of a root (may be empty).
    * @param isValid Function that returns true if a found segment is valid for return.
    * @return The highest value in the sub-tree starting at the left child, or None if the child is empty or if no
    *         element was valid.
    */
  private def findHighestInLeftSubtree(leftChild: RBTree[LineEntry[L]], isValid: Segment[L] => Boolean): Option[Segment[L]] = if (leftChild.isEmpty) {
    None
  } else {
    def reverseInOrder(n: RBTree[LineEntry[L]]): Option[Segment[L]] = if (n.isEmpty) {
      None
    } else {
      val result = reverseInOrder(n.right).getOrElse {
        if (isValid(n.value.segment))
          n.value.segment
        else
          reverseInOrder(n.left).orNull
      }

      Option(result)
    }

    reverseInOrder(leftChild)
  }

  private def defaultValidation(s: Segment[L]) = true

}
