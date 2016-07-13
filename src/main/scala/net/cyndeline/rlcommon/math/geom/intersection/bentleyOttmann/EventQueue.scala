package net.cyndeline.rlcommon.math.geom.intersection.bentleyOttmann

import net.cyndeline.rlcommon.collections.PriorityQueue
import net.cyndeline.rlcommon.math.geom.{DPoint, Line, LineIntersection, Point}

/**
  * Extends the priority queue functionality by allowing intersection events to be updated with a new set of segments.
  *
  * @param currentIntersections Maps an intersection-point or overlap segment to the entry used in the queue to
  *                             represent it.
  */
class EventQueue[L <: Line] private (currentIntersections: Map[DPoint, Intersection[L]], queue: PriorityQueue[EventPoint[L]])(implicit ord: Ordering[EventPoint[L]]) {

  /**
    * Creates an empty event queue.
    */
  def this()(implicit ord: Ordering[EventPoint[L]]) = this(Map(), new PriorityQueue[EventPoint[L]]())(ord)

  def insert(p: EventPoint[L]) = p match {
    case i: Intersection[L] =>
      require(!intersectionExists(i.coordinate), "An intersection is already registered for point " + i.coordinate + ", update the current point instead.")
      make(currentIntersections + (i.coordinate -> i), queue.insert(p))
    case _ => make(currentIntersections, queue.insert(p))
  }

  def poll: (EventPoint[L], EventQueue[L]) = {
    val next: (EventPoint[L], PriorityQueue[EventPoint[L]]) = queue.poll
    val element = next._1
    val updatedQueue = element match {
      case i: Intersection[L] => make(currentIntersections - i.coordinate, next._2)
      case _ => make(currentIntersections, next._2)
    }
    (element, updatedQueue)
  }

  def isEmpty: Boolean = queue.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def update(p: DPoint, updated: Intersection[L]): EventQueue[L] = {
    require(intersectionExists(p), "No intersection to update at point " + p)
    require(updated.coordinate == p, "Coordinate for intersection did not match the intersection point it is updated for.")
    val current = currentIntersections(p)
    make(currentIntersections + (p -> updated), queue.delete(current).insert(updated))
  }

  def intersectionExists(i: DPoint): Boolean = currentIntersections.contains(i)
  def intersectionFor(i: DPoint): Intersection[L] = currentIntersections(i)

  private def make(m: Map[DPoint, Intersection[L]], q: PriorityQueue[EventPoint[L]]) = new EventQueue[L](m, q)

  override def toString: String = if (isEmpty) {
    "Empty queue"
  } else {
    queue.values.mkString(", ")
  }

}
