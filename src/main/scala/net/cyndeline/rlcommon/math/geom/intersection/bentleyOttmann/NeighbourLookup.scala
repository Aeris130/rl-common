package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.collections.RBTree
import net.cyndeline.rlcommon.math.geom.Line
import net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann.SweepLine.LineEntry

/**
  * Helper methods for the sweep line.
  */
class NeighbourLookup[L <: Line](data: RBTree[LineEntry[L]], entries: Vector[LineEntry[L]])(implicit ord: Ordering[LineEntry[L]]) {

  /**
    * @param segment A segment intersecting the sweep line.
    * @return The neighbors (above, below) the specified segment, or None if this segment is the top- or bottommost
    *         entry on the sweep line.
    */
  def aboveAndBelow(segment: Segment[L],
                    belowValid: Segment[L] => Boolean = defaultValidation,
                    aboveValid: Segment[L] => Boolean = defaultValidation): (Option[Segment[L]], Option[Segment[L]]) = {
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
                            leftIsValid: Segment[L] => Boolean = defaultValidation,
                            rightIsValid: Segment[L] => Boolean = defaultValidation): (Option[Segment[L]], Option[Segment[L]]) = {
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
      val below = findClosestInSubtree(current, false, leftIsValid).getOrElse {
        if (previousHighestLower != null && leftIsValid(previousHighestLower.segment))
          previousHighestLower.segment
        else
          null
      }

      val above = findClosestInSubtree(current, true, rightIsValid).getOrElse {
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
    * somewhere be a non-collinear segment dividing the collinear segments into two, placing it between them on the
    * sweep line). This sub tree may contain non-collinear segments, but the left (right) tree has its leftmost
    * (rightmost) segments collinear.
    *
    * NOTE: This method may be used to find both upper and lower closest neighbors, but this description only concerns
    * upper neighbors. The description for lower is similar but reverses the order in which nodes are visited.
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
    * NOTE: This method cannot be used to reliably find segments enclosed by collinear neighbors by adjusting the
    * isCollinear method.
    *
    * @param segment The collinear segment s.
    * @param upper True if the upper neighbor should be found, false if lower.
    * @param isCollinear Returns true if two segments are collinear. Used to make testing easier.
    * @return The lowest upper non-collinear neighbor of s, or None if s if the uppermost segment or if every neighbor
    *         above it is collinear with s.
    */
  def closestNonCollinear(segment: Segment[L], upper: Boolean = true, isCollinear: (Segment[L], Segment[L]) => Boolean = (s1: Segment[L], s2: Segment[L]) => s1.collinearWith(s2)): Option[Segment[L]] = {
    // Next and opposite causes the search to switch between in-order and its reverse
    def nextChild(n: RBTree[LineEntry[L]]) = if (upper) n.left else n.right
    def oppositeChild(n: RBTree[LineEntry[L]]) = if (upper) n.right else n.left
    def lt(a: LineEntry[L], b: LineEntry[L]) = if (upper) a < b else a > b
    def gt(a: LineEntry[L], b: LineEntry[L]) = if (upper) a > b else a < b

    def procA(s: LineEntry[L], current: RBTree[LineEntry[L]], lowestUpper: RBTree[LineEntry[L]]): Option[LineEntry[L]] = if (current.isEmpty) {
      None
    } else if (isCollinear(current.value.segment, s.segment)) {
      // The first collinear segment is found
      procB(s, current, lowestUpper)

    } else {
      val updatedLowestUpper = if (lowestUpper == null && gt(current.value, s)) current
                               /* The current segment needs to be lower than the so-far lowest upper segment found, but
                                * still greater than s to be assigned as the new lowest upper segment in the tree.
                                */
                               else if (lowestUpper != null && lt(current.value, lowestUpper.value) && gt(current.value, s)) current
                               else lowestUpper

      if (lt(s, current.value))
        procA(s, nextChild(current), updatedLowestUpper)
      else
        procA(s, oppositeChild(current), updatedLowestUpper)
    }

    def procB(s: LineEntry[L], current: RBTree[LineEntry[L]], lowestUpper: RBTree[LineEntry[L]]): Option[LineEntry[L]] = {
      val opposite = oppositeChild(current)

      if (!opposite.isEmpty && !isCollinear(s.segment, opposite.value.segment)) {
        // The collinear segment with an upper non-collinear neighbor has been found
        val neighborFromChild = procC(s.segment, current)

        if (neighborFromChild.isDefined) {
          Some(neighborFromChild.get.value)
        } else if (lowestUpper != null) {
          Some(procC(s.segment, lowestUpper).get.value)
        } else {
          None
        }

      } else if (!opposite.isEmpty && isCollinear(s.segment, opposite.value.segment)) {
        procB(s, opposite, lowestUpper)
      } else if (opposite.isEmpty && lowestUpper != null) {
        Some(lowestUpper.value)
      } else {
        None // No upper non-collinear child, and lowest upper is null
      }
    }


    def procC(s: Segment[L], current: RBTree[LineEntry[L]]): Option[RBTree[LineEntry[L]]] = if (current.isEmpty){
      None
    } else {
      val cs = current.value.segment

      /* If the current node is collinear, the closer child can be pruned since it having a greater non-collinear
       * neighbor would put that neighbor between the current node and the child.
       */
      val closer: Option[RBTree[LineEntry[L]]] = if (isCollinear(s, cs))
        None
      else
        procC(s, nextChild(current))

      val result = closer.getOrElse {
        if (!isCollinear(s, cs))
          current
        else
          procC(s, oppositeChild(current)).orNull
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
    * @return Every segment on the line collinear with s, including the ones only sharing a single coordinate with s.
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
      if (currentIsCollinearWithS || !(current.value < segment))
        search(current.left)

      if (currentIsCollinearWithS || !(current.value > segment))
        search(current.right)

      /* Here we need to not only check collinearity, but also confirm that the collinear segment intersects s in more
       * than one point unless it is single-coordinate. The intersection will always be defined as two collinear lines
       * cut by the sweep-line must share at least one coordinate.
       */
      if (current.value.segment != s && currentIsCollinearWithS) {
          result = current.value.segment +: result
      }

    }
    search(data)
    result
  }

  /**
    * Finds a segment closest to another segment s using a user-specified predicate.
    * @param s A segment on the sweep line.
    * @param upper True if the candidate should be searched for above s, false if below.
    * @param isValid Function that returns true for any segment != s that can be returned.
    * @return The closest valid segment to s, or None if no such segment exists.
    */
  def findClosest(s: Segment[L], upper: Boolean, isValid: (Segment[L] => Boolean)): Option[Segment[L]] = {
    def nextChild(n: RBTree[LineEntry[L]]) = if (upper) n.left else n.right
    def oppositeChild(n: RBTree[LineEntry[L]]) = if (upper) n.right else n.left
    def gteq(a: LineEntry[L], b: LineEntry[L]) = if (upper) a >= b else a <= b

    // Find the top-most node containing a segment above (below) or at s
    def findTop(current: RBTree[LineEntry[L]]): Option[RBTree[LineEntry[L]]] = if (current.isEmpty) {
      None
    } else if (gteq(current.value, entries(s.id))) {
      Some(current)
    } else {
      findTop(oppositeChild(current))
    }

    // Do in-order (reversed in-order) search until found, stop at the next child of s
    def search(current: RBTree[LineEntry[L]]): Option[RBTree[LineEntry[L]]] = if (current.isEmpty) {
      None
    } else {

      /* Only search lower (upper) children if we're not at s, and not on a segment that is lower (higher) than s. */
      val next = if (current.value.segment != s && gteq(current.value, entries(s.id))) {
        search(nextChild(current))
      } else {
        None
      }

      val result = Option(next.getOrElse {
        val c = current.value.segment
        if (c != s && gteq(current.value, entries(s.id)) && isValid(c))
          current
        else
          search(oppositeChild(current)).orNull
      })

      result
    }

    val top = findTop(data)
    if (top.isDefined) {
      val r = search(top.get)
      if (r.isDefined)
        Some(r.get.value.segment)
      else
        None
    } else {
      None
    }
  }

  /**
    * In-order traversal that finds the lowest/highest neighbor above/below a root.
    * @param root The root (may not be empty).
    * @param upper True if lowest neighbor above the root should be found, false if looking for the highest neighbor
    *              below.
    * @param isValid Function that returns true if a found segment is valid for return.
    * @return The lowest value in the sub-tree starting at the right child, or None if the child is empty or if no
    *         element was valid.
    */
  private def findClosestInSubtree(root: RBTree[LineEntry[L]], upper: Boolean, isValid: Segment[L] => Boolean): Option[Segment[L]] = {
    require(!root.isEmpty, "Attempted to find the closest neighbor of an empty tree node.")
    def upperChild(r: RBTree[LineEntry[L]]) = if (upper) r.right else r.left
    def lowerChild(r: RBTree[LineEntry[L]]) = if (upper) r.left else r.right

    def inOrder(n: RBTree[LineEntry[L]]): Option[Segment[L]] = if (n.isEmpty) {
      None
    } else {
      val result = inOrder(lowerChild(n)).getOrElse {
        if (isValid(n.value.segment))
          n.value.segment
        else
          inOrder(upperChild(n)).orNull
      }

      Option(result)
    }

    inOrder(upperChild(root))
  }

  private def defaultValidation(s: Segment[L]) = true

}
