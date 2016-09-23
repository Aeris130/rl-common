package net.cyndeline.rlcommon.collections

import net.cyndeline.rlcommon.SpecImports

class BoundedPriorityQueueSpec extends SpecImports {

  describe("BoundedPriorityQueue") {

    it ("should add elements while the bound hasn't been reached") {

      Given("a queue of size 4")
      val queue = BoundedPriorityQueue[Int](4)

      When("adding 4 elements to it")
      val added = queue.insert(3, 1, 2, 4)

      Then("the queue should contain the elements")
      added.values should be (Vector(4, 3, 2, 1))

    }

    it ("should discard a value once the bound has been reached if it is greater than the current greatest value") {

      Given("a full queue with the greatest value being 5")
      val size = 3
      val queue = BoundedPriorityQueue(size, 2, 5, 4)

      When("inserting the value 6")
      val added = queue.insert(6)

      Then("the orginal value set should remain as is")
      added.values should be (Vector(5, 4, 2))

    }

    it ("should remove the current greatest value if the bound is reached and a smaller value is inserted") {

      Given("a full queue with the greatest value being 5")
      val size = 3
      val queue = BoundedPriorityQueue(size, 2, 5, 4)

      When("inserting the value 3")
      val added = queue.insert(3)

      Then("the value 5 should be removed")
      added.values should be (Vector(4, 3, 2))

    }

    it ("should reject a value greater than the current greatest when previously an even greater value has been removed") {

      Given("a queue with size two and the elements 4 and 5, where previously the element 9 was pushed out by 5")
      val size = 2
      val queue = BoundedPriorityQueue(size, 4, 9).insert(5)

      When("adding the value 6")
      val added = queue.insert(6)

      Then("the orginal value set should remain as is")
      added.values should be (Vector(5, 4))

    }

    it ("should throw an exception when creating a queue with bounds 0") {

      Given("a bound 0")
      val bound = 0

      When("creating a queue using the bound")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        BoundedPriorityQueue(bound)
      }

    }

  }

}
