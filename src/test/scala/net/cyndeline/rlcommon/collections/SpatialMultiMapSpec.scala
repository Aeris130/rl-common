package net.cyndeline.rlcommon.collections

import net.cyndeline.rlcommon.SpecImports
import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}

class SpatialMultiMapSpec extends SpecImports {

  describe("SpatialMultiMap") {

    it ("should map an element to a value") {

      Given("an empty map")
      val map = SpatialMultiMap.withPoint2D[Int]()

      When("mapping an element to a value")
      val element = Point(0, 0)
      val value = 1
      val withElement = map + (element -> value)

      Then("the element should be mapped to the value")
      withElement.get(element) should be (Some(Set(value)))

    }

    it ("should map an element to multiple values") {

      Given("an empty map and an element")
      val map = SpatialMultiMap.withPoint2D[Int]()
      val element = Point(2, 2)

      When("mapping the element to two values")
      val v1 = 1
      val v2 = 2
      val withElement = map + (element -> v1, element -> v2)

      Then("the element should be mapped to both values")
      withElement.get(element) should be (Some(Set(v1, v2)))

    }

    it ("should discard duplicates") {

      Given("a map that maps an element to the value")
      val e = Point(0, 0)
      val v = 1
      val map = SpatialMultiMap.withPoint2D[Int]() + (e -> v)

      When("mapping the same key-value pair")
      val withDuplicate = map + (e -> v)

      Then("the element should be mapped to the value")
      withDuplicate.get(e) should be (Some(Set(v)))

    }

    it ("should delete a value") {

      Given("a map with three key-value pairs")
      val toBeDeleted = Point(2, 2) -> 2
      val map = SpatialMultiMap.withPoint2D[Int]() + (Point(1, 1) -> 1, toBeDeleted, Point(3, 3) -> 3)

      When("deleting one value")
      val deleted = map - toBeDeleted

      Then("the map should have size 2")
      deleted.keySize should be (2)

      And("both remaining entries should exist")
      deleted.get(Point(1, 1)) should be (Some(Set(1)))
      deleted.get(Point(3, 3)) should be (Some(Set(3)))

    }

    it ("should not attempt to delete non-existent values") {

      Given("a map with an element mapped to a value")
      val map = SpatialMultiMap.withPoint2D[Int]() + (Point(1, 1) -> 1)

      When("deleting the value using the wrong key or value")
      Then("an exception should be thrown")
      intercept[NoSuchElementException] {
        map - (Point(3, 3), 1)
      }
      intercept[NoSuchElementException] {
        map - (Point(1, 1), 2)
      }

    }

    it ("should maintain keys that are mapped to multiple values when deleting one value") {

      Given("a map with a key mapped to two values")
      val map = SpatialMultiMap.withPoint2D[Int]() + (Point(1, 1) -> 1, Point(1, 1) -> 2)

      When("deleting one of the values")
      val deleted = map - (Point(1, 1), 2)

      Then("the remaining value should still be in the map")
      deleted.get(Point(1, 1)) should be (Some(Set(1)))

    }

    it ("should return mapped values based on a spatial range search") {

      Given("a map with five mappings")
      val m1 = Point(1, 1) -> 1
      val m2 = Point(1, 1) -> 11
      val m3 = Point(3, 3) -> 3
      val m4 = Point(5, 5) -> 5
      val m5 = Point(7, 7) -> 7
      val map = SpatialMultiMap.withPoint2D[Int]() + (m1, m2, m3, m4, m5)

      When("retrieving values mapped within the range enclosing values 1 to 4")
      val range = Rectangle(Point(0, 0), 7, 7)
      val withinRange = map.getRange(range)

      Then("values mapped to m1 through m4 should be in the result")
      withinRange should be (Set(1, 11, 3, 5))

    }

    it ("should return an empty set for a range query on an empty map") {

      Given("an empty map")
      val map = SpatialMultiMap.withPoint2D[Int]()

      When("querying a rectangular area")
      val result = map.getRange(Rectangle(Point(0, 0), 3, 4))

      Then("the result should be an empty set")
      result should be (Set())

    }

    it ("should not include results in a ranged search after deleting them") {

      Given("a map with a point")
      val p = Point(1, 2)
      val map = SpatialMultiMap.withPoint2D[Int]() + (p, 1)

      When("deleting the point and performing a ranged search including it")
      val deleted = map - (p, 1)
      val search = deleted.getRange(Rectangle(Point(0, 0), 3, 4))

      Then("the result should be empty")
      search should be ('empty)

    }

  }
}
