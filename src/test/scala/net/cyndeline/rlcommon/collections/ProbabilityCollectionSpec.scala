package net.cyndeline.rlcommon.collections

import net.cyndeline.rlcommon.SpecImports

import scala.util.Random

class ProbabilityCollectionSpec extends SpecImports {

  describe("ProbabilityCollection") {

    it ("should return the same values from objects with the same seed") {

      Given("three random collections with the same content")

      val collection1 = new ProbabilityCollection[Int]()
        .add(1, 1).add(1, 2).add(1, 3).add(1, 4).add(1, 5)
      val collection2 = new ProbabilityCollection[Int]()
        .add(1, 1).add(1, 2).add(1, 3).add(1, 4).add(1, 5)
      val collection3 = new ProbabilityCollection[Int]()
        .add(1, 1).add(1, 2).add(1, 3).add(1, 4).add(1, 5)

      When("adding 5 elements and retriving 1 element from each collection")
      val r1 = new Random(1)
      val r2 = new Random(1)
      val r3 = new Random(1)

      val result1 = collection1.next(r1)
      val result2 = collection2.next(r2)
      val result3 = collection3.next(r3)

      Then("all results should be equal")
      result1 should equal (result2)
      result2 should equal (result3)

    }

    it ("should throw an exception if retrieving a next() value on an empty collection") {

      Given("an empty collection")
      val collection = new ProbabilityCollection[Int]()

      When("retrieving an element")
      Then("an exception should be thrown")
      intercept[NoSuchElementException] {
        collection.next(null)
      }

    }

    it ("should throw an error when attempting to add elements with probability 0") {

      Given("a probability collection")
      val collection = new ProbabilityCollection[String]()

      When("adding an element with probability 0")
      Then("an error should be thrown")
      intercept[IllegalArgumentException] {
        collection.add(0, "")
      }

    }

    it ("should throw an error when attempting to add elements with negative probability") {

      Given("a probability collection")
      val collection = new ProbabilityCollection[String]()

      When("adding an element with probability -1")
      Then("an error should be thrown")
      intercept[IllegalArgumentException] {
        collection.add(-1, "")
      }

    }

    it ("should thrown an error when adding duplicate values") {

      Given("a probability collection")
      val collection = new ProbabilityCollection[String]()

      When("adding an element twice")
      Then("an error should be thrown")
      intercept[IllegalArgumentException] {
        collection.add(1, "A").add(2, "A")
      }

    }

    it ("should allow creation of an empty collection") {

      When("creating an empty collection")
      val collection = new ProbabilityCollection[String]()

      Then("the collection should be empty")
      collection.size should be (0)
      collection.isEmpty should be (true)
      collection.allElements should be ('empty)

    }

    it ("should allow creation from a sequence of tuples") {

      Given("a list of weight/element tuples")
      val list = Vector((1.0, "A"), (2.0, "B"))

      When("creating a collection")
      val collection = ProbabilityCollection.from[String](list:_*)

      Then("both tuples should be in the collection")
      collection.iterator.toVector should equal (list)
      collection.allElements should contain("A")
      collection.allElements should contain("B")

    }

    it ("should iterate over the element/weight set") {

      Given("a collection with two elements having weight 1 and 2")
      val collection = new ProbabilityCollection[String]()
        .add(1, "A").add(2, "B")

      When("iterating over the elements")
      val elements = collection.iterator.toSet

      Then("both tuples should be present")
      collection.iterator should have size 2
      elements should contain ((1, "A"))
      elements should contain ((2, "B"))

    }

    it ("should preserve weights and elements when adding elements to the collection") {

      Given("3 elements with weights 1, 2 and 3")

      When("updating the collection")
      val c = new ProbabilityCollection[String]().add(1, "A").add(2, "B").add(3, "C")

      Then("the collection should contain the same elements")
      c.combinedWeights should equal (Vector((1, "A"), (3, "B"), (6, "C")))
      c.iterator.toVector should equal (Vector((1, "A"), (2, "B"), (3, "C")))

    }

    it ("should preserve weights and elements when adding and removing elements to the collection") {

      Given("3 elements with weights 1, 2 and 3")
      val collection = new ProbabilityCollection[String]().add(1, "A").add(2, "B").add(3, "C")

      When("removing one value and adding another")
      val updated = collection.remove("B").add(4, "D")

      Then("the collection should adjust values correctly")
      updated.combinedWeights should equal (Vector((1, "A"), (4, "C"), (8, "D")))
      updated.iterator.toVector should equal (Vector((1, "A"), (3, "C"), (4, "D")))

    }

    it ("should preserve the collection when removing entries") {

      Given("a collection with three entries")
      val collection = new ProbabilityCollection[String]()
        .add(1, "A").add(2, "B").add(3, "C")

      When("removing the second entry")
      val removed = collection.remove("B")

      Then("two elements should remain")
      removed.size should be (2)
      removed.allElements should contain("A")
      removed.allElements should contain("C")
      removed.iterator.toVector should be (Vector((1, "A"), (3, "C")))
      removed.combinedWeights should equal (Vector((1, "A"), (4, "C")))

    }

    it ("should add a collection") {

      Given("two collections with elements in them")
      val collection1 = new ProbabilityCollection[String]()
        .add(1, "A").add(3, "B").add(5, "C")

      val collection2 = new ProbabilityCollection[String]()
        .add(2, "D").add(4, "E").add(6, "F")

      When("adding one collection to the other")
      val added = collection1.addCollection(collection2)

      Then("the first collection should have all elements in their weight order")
      added.iterator should have size 6
      added.combinedWeights should have size 6
      added.iterator.toSet should equal (Set((1, "A"), (2, "D"), (3, "B"), (4, "E"), (5, "C"), (6, "F")))
      added.combinedWeights should equal (Vector((1, "A"), (4, "B"), (9, "C"), (11, "D"), (15, "E"), (21, "F")))

    }

    it ("should do nothing when adding an empty collection") {

      Given("a collection with elements and an empty collection")
      val collection1 = new ProbabilityCollection[String]()
      .add(1, "A").add(2, "B").add(3, "C")
      val size = collection1.size

      val collection2 = new ProbabilityCollection[String]()

      When("adding the empty collection")
      val added = collection1.addCollection(collection2)

      Then("the first collection should remain as-is")
      added.size should be (size)
      added.iterator.toVector should be (Vector((1, "A"), (2, "B"), (3, "C")))
      added.combinedWeights should be (Vector((1, "A"), (3, "B"), (6, "C")))

    }

    /**
      * Test checking that an element is returned at the expected rate.
      */
    it ("should return elements at a rate corresponding to their weight") {

      Given("a collection with total weight 20, and an element C with weight 3")
      val collection1 = new ProbabilityCollection[String]()
        .add(7, "A").add(10, "B").add(3, "C")

      /* Perform some modifications first to check if they affect the result. */
      val collectionToTest = collection1.remove("C").addCollection(ProbabilityCollection.from[String]((3, "C")))

      When("retrieving a large amount of values")
      val r = new Random()
      val runs = 10000000
      val precision = 0.05 * runs // 5% error margin with 10000000 runs

      var aFound = 0
      var bFound = 0
      var cFound = 0
      for (i <- 0 to runs) {
        val n = collectionToTest.next(r)
        if (n == "C") cFound += 1
        else if (n == "A") aFound += 1
        else bFound += 1
      }

      Then("the element A should make up roughly 7/20 parts of the result")
      val expectedAOccurences = (7.0/20) * runs
      assert((aFound - expectedAOccurences) < precision, s"The number of element occurences ($aFound) did not match the expected outcome ($expectedAOccurences). The allowed error margin was $precision, but the error was ${aFound - expectedAOccurences}.")

      Then("the element B should make up roughly 10/20 parts of the result")
      val expectedBOccurences = (10.0/20) * runs
      assert((bFound - expectedBOccurences) < precision, s"The number of element occurences ($bFound) did not match the expected outcome ($expectedBOccurences). The allowed error margin was $precision, but the error was ${bFound - expectedBOccurences}.")

      Then("the element C should make up roughly 3/20 parts of the result")
      val expectedCOccurences = (3.0/20) * runs
      assert((cFound - expectedCOccurences) < precision, s"The number of element occurences ($cFound) did not match the expected outcome ($expectedCOccurences). The allowed error margin was $precision, but the error was ${cFound - expectedCOccurences}.")



    }

  }
}
