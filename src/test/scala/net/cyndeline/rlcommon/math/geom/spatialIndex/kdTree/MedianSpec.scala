package net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.spatialIndex.common.ElementProperty
import net.cyndeline.rlcommon.math.geom.{Point, RPoint}

class MedianSpec extends SpecImports {

  private case class Point3D(x: Int, y: Int, z: Int)
  private val prop = new ElementProperty[Point3D]() {
    override val totalDimensions: Int = 3
    override def value(element: Point3D, dimension: Int) = dimension match {
      case 1 => element.x
      case 2 => element.y
      case 3 => element.z
      case _ => throw new Error("Unspecified dimension for " + element + ": " + dimension)
    }
    override def distance(a: Point3D, b: Point3D) = ???
    override def axisDistance(a: Point3D, b: Point3D, dimension: Int) = ??? // Not needed for medians.
  }

  describe("Median") {

    it ("should compute the median for a single element") {

      Given("a single element list")
      val e = Vector(Point(0, 0))

      When("computing its median")
      val median = Median[Point](e, new Point2DProperty()).split(1)

      Then("the element should be the median")
      median._1 should be (e.head)

      And("both left and right splits should be empty")
      median._2 should be ('empty)
      median._3 should be ('empty)

    }

    it ("should compute multiple elements") {
      // Example taken from "Building a Balanced k-d Tree in O(kn log n) Time".

      Given("a 3-dimensional point set")
      val p0 = Point3D(2, 3, 3)
      val p1 = Point3D(5, 4, 2)
      val p2 = Point3D(9, 6, 7)
      val p3 = Point3D(4, 7, 9)
      val p4 = Point3D(8, 1, 5)
      val p5 = Point3D(7, 2, 6)
      val p6 = Point3D(9, 4, 1)
      val p7 = Point3D(8, 4, 2)
      val p8 = Point3D(9, 7, 8)
      val p9 = Point3D(6, 3, 1)
      val p10 = Point3D(3, 4, 5)
      val p11 = Point3D(1, 6, 8)
      val p12 = Point3D(9, 5, 3)
      val p13 = Point3D(2, 1, 3)
      val p14 = Point3D(8, 7, 6)
      val ps = Vector(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)

      When("creating a median computation using the point list")
      val median = Median[Point3D](ps, prop)

      Then("the median should be (7, 2, 6) when splitting it once along the x axis")
      val firstSplit = median.split(1)
      val left1 = firstSplit._2
      val right1 = firstSplit._3

      firstSplit._1 should be (p5)

      And("the second split should be (5, 4, 2) and (9, 5, 3) when splitting a second time along the y axis")
      val secondSplit1 = left1.split(2)
      val secondSplit2 = right1.split(2)
      val left2_1 = secondSplit1._2
      val right2_1 = secondSplit1._3
      val left2_2 = secondSplit2._2
      val right2_2 = secondSplit2._3

      secondSplit1._1 should be (p1)
      secondSplit2._1 should be (p12)

      And("the third splits should be (2,1,3), (1,6,8), (8,4,2), (9,6,7) when splitting along the z axis")
      val thirdSplit1 = left2_1.split(3)
      val thirdSplit2 = right2_1.split(3)
      val thirdSplit3 = left2_2.split(3)
      val thirdSplit4 = right2_2.split(3)

      thirdSplit1._1 should be (p13)
      thirdSplit2._1 should be (p11)
      thirdSplit3._1 should be (p7)
      thirdSplit4._1 should be (p2)

    }

    it ("should remove duplicates") {

      Given("5 points with 3 duplicates")
      val p1 = Point3D(2, 3, 3)
      val p2 = Point3D(2, 3, 3)
      val p3 = Point3D(2, 3, 3)
      val p4 = Point3D(6, 2, 99)
      val p5 = Point3D(8, 1, 1)
      val points = Vector(p1, p4, p2, p3, p5)

      When("creating a median and splitting it along the x axis")
      val median = Median[Point3D](points, prop)
      val split = median.split(1)

      Then("the median should be p4")
      split._1 should be (p4)

      And("the remaining splits should have 1 element left in them")
      val split2 = split._2.split(2)
      val split3 = split._3.split(2)

      split2._1 should be (p1)
      split3._1 should be (p5)
      split2._2 should be ('empty)
      split2._3 should be ('empty)
      split3._2 should be ('empty)
      split3._3 should be ('empty)

    }

  }

}
