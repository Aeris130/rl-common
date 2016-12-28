package net.cyndeline.rlcommon.math.geom

import net.cyndeline.rlcommon.SpecImports

class RectangleSpec extends SpecImports {

  describe("Rectangle") {

    it ("it should consider rectangles on its border as overlapping") {

      Given("a rectangle between (1,1) and (2,2)")
      val r = Rectangle(Point(1, 1), 2, 2)

      When("checking overlap for rectangles on its four sides")
      val west = Rectangle(Point(0, 1), 2, 2)
      val north = Rectangle(Point(1, 2), 2, 2)
      val east = Rectangle(Point(2, 1), 2, 2)
      val south = Rectangle(Point(1, 0), 2, 2)

      Then("all four should be overlapping")
      r.overlaps(west) should be (true)
      r.overlaps(north) should be (true)
      r.overlaps(east) should be (true)
      r.overlaps(south) should be (true)

    }

    it ("should consider a rectangle sharing a corner as overlapping") {

      Given("a rectangle between (1,1) and (2,2)")
      val r1 = Rectangle(Point(1, 1), 2, 2)

      When("checking overlap with a rectangle (0,0) to (1,1)")
      val r2 = Rectangle(Point(0, 0), 2, 2)

      Then("they should overlap")
      r1.overlaps(r2) should be (true)

    }

    it ("should not overlap a rectangle outside of it") {

      Given("a rectangle between (2,4) and (8,7)")
      val r1 = Rectangle(Point(2, 4), 7, 4)

      When("checking overlap with a rectangle above it")
      val r2 = Rectangle(Point(12, 14), 7, 4)

      Then("they should not overlap")
      r1.overlaps(r2) should be (false)

    }

    it ("should overlap a rectangle partly inside of it") {

      Given("a rectangle between (3,3) and (7,7)")
      val r1 = Rectangle(Point(3, 3), 5, 5)

      When("checking overlap with a rectangle between (0,0) and (4,4)")
      val r2 = Rectangle(Point(0, 0), 5, 5)

      Then("they should overlap")
      r1.overlaps(r2) should be (true)

    }

    it ("should overlap a rectangle inside of it") {

      Given("a rectangle between (0,0) and (7,7)")
      val r1 = Rectangle(Point(0, 0), 8, 8)

      When("checking overlap with a rectangle between (1,1) and (4,4)")
      val r2 = Rectangle(Point(1, 1), 5, 5)

      Then("they should overlap")
      r1.overlaps(r2) should be (true)

    }

    it ("should confirm that a point on its border is inside it") {

      Given("a rectangle from (1,1) to (3,4)")
      val r = Rectangle(Point(1, 1), Point(3, 4))

      When("checking if the point (2,1) is inside it")
      val isInside = r.containsPoint(Point(2, 1))

      Then("the result should be true")
      isInside should be (true)

    }

    it ("should confirm that a point inside its border is inside it") {

      Given("a rectangle from (1,1) to (3,4)")
      val r = Rectangle(Point(1, 1), Point(3, 4))

      When("checking if the point (2,2) is inside it")
      val isInside = r.containsPoint(Point(2, 2))

      Then("the result should be true")
      isInside should be (true)

    }

    it ("should confirm that a point is outside of it") {

      Given("a rectangle from (1,1) to (3,4)")
      val r = Rectangle(Point(1, 1), Point(3, 4))

      When("checking if the point (23,11) is inside it")
      val isInside = r.containsPoint(Point(23, 11))

      Then("the result should be false")
      isInside should be (false)

    }

    it ("should compute a rectangle with odd sides centered around a point") {

      Given("a point (3,4), width 3 and height 5")
      val p = Point(3, 4)
      val width = 3
      val height = 5

      When("computing a rectangle centered around p")
      val centered = Rectangle.centerAround(p, width, height)

      Then("the rectangle should begin at (2,2) and end at (4,6)")
      centered.start should be (Point(2, 2))
      centered.stop should be (Point(4, 6))

      And("the rectangle should have the specified height/width")
      centered.width should be (width)
      centered.height should be (height)

    }

    it ("should compute a rectangle with even sides centered around a point") {

      Given("a point (3,4), width 4 and height 6")
      val p = Point(3, 4)
      val width = 4
      val height = 6

      When("computing a rectangle centered around p")
      val centered = Rectangle.centerAround(p, width, height)

      Then("the rectangle should begin at (2,2) and end at (5,7)")
      centered.start should be (Point(2, 2))
      centered.stop should be (Point(5, 7))

      And("the rectangle should have the specified height/width")
      centered.width should be (width)
      centered.height should be (height)

    }

    /*
     * Line intersection.
     */

    it ("should detect intersection at corners") {

      Given("a rectangle with corners (2,2), (4,2), (2,4) and (4,4)")
      val r = Rectangle(Point(2, 2), Point(4, 4))

      When("checking intersection for segments ending in the four corners")
      val ul = r.intersectsLine(Line(Point(1, 5), Point(2, 4)))
      val ll = r.intersectsLine(Line(Point(0, 0), Point(2, 2)))
      val ur = r.intersectsLine(Line(Point(5, 5), Point(4, 4)))
      val lr = r.intersectsLine(Line(Point(5, 0), Point(4, 2)))

      Then("all lines should intersect")
      ul should be (true)
      ll should be (true)
      ur should be (true)
      lr should be (true)

    }

    it ("should detect intersection with no point inside") {

      Given("a rectangle and a segment that intersects it with no end-point in the rectangle")
      val r = Rectangle(Point(2, 2), Point(5, 5))
      val l = Line(Point(3, 6), Point(6, 3))

      When("checking if the segment intersects")
      val intersects = r.intersectsLine(l)

      Then("an intersection should be found")
      intersects should be (true)

    }

    it ("should detect intersection with both points inside") {

      Given("a rectangle and a segment overlapping with it")
      val r = Rectangle(Point(2, 2), Point(5, 5))
      val l = Line(Point(3, 3), Point(4, 4))

      When("checking if the segment intersects")
      val intersects = r.intersectsLine(l)

      Then("an intersection should be found")
      intersects should be (true)

    }

    it ("should detect intersection with one point inside") {

      Given("a rectangle and four segment that intersects each side")
      val r = Rectangle(Point(2, 2), Point(5, 5))

      When("checking intersection for each segment")
      val left = r.intersectsLine(Line(Point(1, 4), Point(3, 4)))
      val top = r.intersectsLine(Line(Point(3, 6), Point(3, 4)))
      val right = r.intersectsLine(Line(Point(4, 4), Point(6, 4)))
      val bot = r.intersectsLine(Line(Point(4, 1), Point(4, 3)))

      Then("all segments should intersect")
      left should be (true)
      top should be (true)
      right should be (true)
      bot should be (true)

    }

    /*
     * Distance between rectangles
     */

    it ("should compute the distance -1 if two rectangles intersect beyond sharing a segment") {

      Given("two rectangles that overlap")
      val r1 = Rectangle(Point(0, 0), 4, 4)
      val r2 = Rectangle(Point(2, 2), 4, 4)

      When("computing the distance between the rectangles")
      val distance = r1.shortestDistance(r2)

      Then("the distance should be -1")
      distance should be (-1)

    }

    it ("compute the distance -1 if two rectangles share segments but also overlap") {

      Given("a rectangle")
      val r = Rectangle(Point(3, 4), 6, 4)

      When("computing the distance to itself")
      val distance = r.shortestDistance(r)

      Then("the distance should be -1")
      distance should be (-1)

    }

    it ("should compute the distance 0 if two rectangles share a segment") {

      Given("a rectangle r and four rectangles that share a segment with r")
      val r = Rectangle(Point(5, 5), 4, 4) // 5,5 to 8,8
      val top = Rectangle(Point(5, 8), 4, 3)
      val left = Rectangle(Point(3, 4), 3, 4)
      val bottom = Rectangle(Point(6, 4), 2, 2)
      val right = Rectangle(Point(8, 3), 2, 7)

      When("computing the distances to each segment")
      val dTop = r.shortestDistance(top)
      val dLeft = r.shortestDistance(left)
      val dBottom = r.shortestDistance(bottom)
      val dRight = r.shortestDistance(right)

      Then("each distance should be 0")
      dTop should be (0)
      dLeft should be (0)
      dBottom should be (0)
      dRight should be (0)

    }

    it ("should compute the distance to segments that lies directly to one of its sides") {

      Given("a rectangle r and four rectangles to its sides")
      val r = Rectangle(Point(5, 5), 4, 4) // 5,5 to 8,8
      val top = Rectangle(Point(5, 10), 3, 2)
      val left = Rectangle(Point(2, 5), 3, 4)
      val bottom = Rectangle(Point(6, 1), 4, 4)
      val right = Rectangle(Point(11, 6), 3, 2)

      When("computing the distances to each segment")
      val dTop = r.shortestDistance(top)
      val dLeft = r.shortestDistance(left)
      val dBottom = r.shortestDistance(bottom)
      val dRight = r.shortestDistance(right)

      Then("the distance to top should be 2")
      dTop should be (2.0)

      And("the distance to left should be 1")
      dLeft should be (1.0)

      And("the distance to bottom should be 1")
      dBottom should be (1.0)

      And("the distance to right should be 3")
      dRight should be (3.0)

    }

    it ("should compute the distance to segments that lies diagonally to one of its sides") {

      Given("a rectangle r and four rectangles diagonally to its sides")
      val r = Rectangle(Point(5, 5), 4, 4) // 5,5 to 8,8
      val topLeft = Rectangle(Point(2, 9), 3, 3)
      val bottomLeft = Rectangle(Point(1, 1), 2, 3)
      val bottomRight = Rectangle(Point(10, 0), 3, 3)
      val topRight = Rectangle(Point(9, 10), 4, 3)

      When("computing the distances to each segment")
      val dTopLeft = r.shortestDistance(topLeft)
      val dBottomLeft = r.shortestDistance(bottomLeft)
      val dBottomRight = r.shortestDistance(bottomRight)
      val dTopRight = r.shortestDistance(topRight)

      Then("the distance to top left should be the distance from r.topLeft to tl.bottomRight")
      val tlBottomRight = topLeft.start + (topLeft.width - 1, 0)
      dTopLeft should be ((r.start + (0, r.height - 1)).distanceTo(tlBottomRight))

      And("the distance to bottom left should be the distance from r.bottomLeft to bl.topRight corner")
      val blTopRight = bottomLeft.start + (bottomLeft.width - 1, bottomLeft.height - 1)
      dBottomLeft should be (blTopRight.distanceTo(r.start))

      And("the distance to bottom right should be the distance from r.bottomRight to br.topLeft")
      val brTopLeft = bottomRight.start + (0, bottomRight.height - 1)
      dBottomRight should be ((r.start + (r.width - 1, 0)).distanceTo(brTopLeft))

      And("the distance to top right should be the distance from r.topRight to tr.start")
      dTopRight should be ((r.start + (r.width - 1, r.height - 1)).distanceTo(topRight.start))

    }

    it ("should consider two single-point rectangles with the same coordinate as overlapping") {

      Given("a single-point rectangle")
      val r = Rectangle(Point(2, 9), 1, 1)

      When("checking the distance to itself")
      val distance = r.shortestDistance(r)

      Then("the distance should be -1")
      distance should be (-1)

    }

    it ("should consider a single-point rectangle on the edge of another rectangle as having distance 0") {

      Given("a single-point rectangle on the edge of another rectangle")
      val r = Rectangle(Point(2, 9), 1, 1)
      val other = Rectangle(Point(2, 9), 4, 5)

      When("checking the distance between the rectangles")
      val distance = r.shortestDistance(other)

      Then("the distance should be 0")
      distance should be (0)

    }

    it ("should compute distance to single-point rectangles") {

      Given("two separate single-point rectangles")
      val r1 = Rectangle(Point(2, 9), 1, 1)
      val r2 = Rectangle(Point(24, 43), 1, 1)

      When("checking the distance between the rectangles")
      val d1 = r1.shortestDistance(r2)
      val d2 = r1.shortestDistance(r2)

      Then("the distance should be the distance between their start coordinates")
      d1 should be (r1.start.distanceTo(r2.start))
      d2 should be (r2.start.distanceTo(r1.start))

    }

    /*
     * Intersections
     */

    it ("should compute intersections for rectangles that lie inside another rectangle") {

      Given("a rectangle r1 and another rectangle r2 inside it")
      val r1 = Rectangle(Point(4, 4), 7, 8)
      val r2 = Rectangle(Point(5, 5), 3, 3)

      When("computing the intersection between the rectangles")
      val r1IntR2 = r1.intersection(r2)
      val r2IntR1 = r2.intersection(r1)

      Then("the intersection should be r2")
      r1IntR2 should be (Some(r2))
      r2IntR1 should be (Some(r2))

    }

    it ("should not compute intersections for disjoint rectangles") {

      Given("two disjoint rectangles")
      val r1 = Rectangle(Point(4, 4), 3, 2)
      val r2 = Rectangle(Point(67, 54), 3, 12)

      When("computing the intersection between the rectangles")
      val r1IntR2 = r1.intersection(r2)
      val r2IntR1 = r2.intersection(r1)

      Then("no intersection should be found")
      r1IntR2 should be ('empty)
      r2IntR1 should be ('empty)

    }

    it ("should compute a partial rectangle intersection") {

      Given("two rectangles that intersect partially between (6,6) and (8,9)")
      val r1 = Rectangle(Point(4, 3), Point(8, 9))
      val r2 = Rectangle(Point(6, 6), Point(10, 10))

      When("computing the intersection between the rectangles")
      val r1IntR2 = r1.intersection(r2)
      val r2IntR1 = r2.intersection(r1)

      Then("the intersection should be between (6,6) and (8,9)")
      val intersection = Rectangle(r2.start, 3, 4)
      r1IntR2 should be (Some(intersection))
      r2IntR1 should be (Some(intersection))

    }

    it ("should compute an intersection on the border between rectangles") {

      Given("two rectangles that share the lower border on r1")
      val r1 = Rectangle(Point(4, 3), Point(8, 9))
      val r2 = Rectangle(Point(3, 1), Point(8, 3))

      When("computing the intersection between the rectangles")
      val r1IntR2 = r1.intersection(r2)
      val r2IntR1 = r2.intersection(r1)

      Then("the intersection should be between (4,3) and (8,3)")
      val intersection = Rectangle(r1.start, 5, 1)
      r1IntR2 should be (Some(intersection))
      r2IntR1 should be (Some(intersection))

    }

    it ("should compute a single-coordinate rectangle for intersecting corners") {

      Given("two rectangles that intersect at (4,5)")
      val i = Point(4, 5)
      val r1 = Rectangle(Point(1, 2), i)
      val r2 = Rectangle(i, Point(6, 8))

      When("computing the intersection between the rectangles")
      val r1IntR2 = r1.intersection(r2)
      val r2IntR1 = r2.intersection(r1)

      Then("the intersection should be (4,5)")
      val result = Rectangle(i, 1, 1)
      r2IntR1 should be (Some(result))
      r1IntR2 should be (Some(result))

    }

  }
}
