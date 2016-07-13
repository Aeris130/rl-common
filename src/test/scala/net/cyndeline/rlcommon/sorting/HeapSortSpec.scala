package net.cyndeline.rlcommon.sorting

import net.cyndeline.rlcommon.SpecImports

class HeapSortSpec extends SpecImports {

  describe("HeapSort") {

    it ("should sort an empty list") {

      Given("an empty list")
      val empty = Vector[String]()

      When("sorting the list")
      val sorted = HeapSort(empty)

      Then("the result should be empty")
      sorted should be ('empty)

    }

    it ("should sort a single element") {

      Given("a list with a single element")
      val element = "A"
      val empty = Vector(element)

      When("sorting the list")
      val sorted = HeapSort(empty)

      Then("the result should contain the element")
      sorted should be (Seq(element))

    }

    it ("should sort a list of elements") {

      Given("the non-ordered elements having sorted order A, B, C, D, E")
      val elements = Vector("C", "A", "E", "B", "D")

      When("sorting the list")
      val sorted = HeapSort(elements)

      Then("the elements should be ordered: A, B, C, D, E")
      sorted should be (Seq("A", "B", "C", "D", "E"))

    }

  }

}
