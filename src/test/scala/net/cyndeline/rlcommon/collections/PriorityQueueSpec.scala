package net.cyndeline.rlcommon.collections

import net.cyndeline.rlcommon.SpecImports

import scala.collection.mutable.ArrayBuffer

class PriorityQueueSpec extends SpecImports {

  private implicit val tupleOrdering = Ordering.by[(String, Int), (Int, String)](e => (e._2, e._1))

  private def multipleElements = new {
    val a = ("A", 4)
    val b = ("B", 45)
    val c = ("C", 16)
    val d = ("D", -34)
    val e = ("E", 34)
    val queue = new PriorityQueue[(String, Int)]()
      .insert(a)
      .insert(b)
      .insert(c)
      .insert(d)
      .insert(e)
  }

  describe("PriorityQueue") {

    it ("should create an empty queue") {

      Given("...nothing I guess?")

      When("creating an empty queue")
      val queue = new PriorityQueue[(String, Int)]()

      Then("the new queue should be empty")
      queue should be ('empty)

    }

    it ("should insert an element") {

      Given("a queue and an element A")
      val queue = new PriorityQueue[(String, Int)]()
      val element = ("A", 0)

      When("inserting A into the queue")
      val queueWithA = queue.insert(element)

      Then("the element with the highest priority should be A")
      queueWithA.peek should be (element)

    }

    it ("should report all elements in the queue") {

      Given("a queue with elements in the order A, B, C, D")
      val queue = new PriorityQueue[(String, Int)]()
      val a = ("A", 4)
      val b = ("B", 3)
      val c = ("C", 2)
      val d = ("D", 1)
      val queueWithElements = queue.insert(a, b, c, d)

      When("retrieving all values from the queue")
      val values = queueWithElements.values

      Then("the values should be in the order A, B, C, D")
      values should be (Vector(a, b, c, d))

    }

    it ("should return the element with the highest priority") {

      Given("a number of elements with different priority, where B is the highest")
      val f = multipleElements
      import f._

      When("retrieving the element with the highest priority")
      val highest = queue.peek

      Then("the element with the highest priority should be B")
      highest should be (b)

    }

    it ("should remove an element from the queue") {

      Given("a queue with elements in the order B, E, C, A, D")
      val f = multipleElements
      import f._

      When("retrieving them in order")
      var updatedQueue = queue
      val result = new ArrayBuffer[(String, Int)]()
      for (i <- 0 until 5) {
        val removal = updatedQueue.poll
        result += removal._1
        updatedQueue = removal._2
      }

      Then("the elements should be returned in the order B, E, C, A, D")
      result should be (Seq(b, e, c, f.a, d))

      And("the queue should be empty")
      updatedQueue should be ('empty)

    }

    it ("should sort elements using their own ordering if they have the same priority") {

      Given("a queue with elements having the same priority and regular ordering E, D, C, B, A")
      val a = ("A", 0)
      val b = ("B", 0)
      val c = ("C", 0)
      val d = ("D", 0)
      val e = ("E", 0)
      val queue = new PriorityQueue[(String, Int)]()
        .insert(a)
        .insert(b)
        .insert(c)
        .insert(d)
        .insert(e)

      When("retrieving them in order")
      var updatedQueue = queue
      val result = new ArrayBuffer[(String, Int)]()
      for (i <- 0 until 5) {
        val removal = updatedQueue.poll
        result += removal._1
        updatedQueue = removal._2
      }

      Then("the elements should be returned in the order E, D, C, B, A")
      result should be (Seq(e, d, c, b, a))

      And("the queue should be empty")
      updatedQueue should be ('empty)

    }

  }

}
