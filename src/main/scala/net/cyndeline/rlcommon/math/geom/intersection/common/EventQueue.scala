package net.cyndeline.rlcommon.math.geom.intersection.common

import java.util.Comparator

import net.cyndeline.rlcommon.collections.PriorityQueue
import net.cyndeline.rlcommon.math.geom._

/**
  * Extends the priority queue functionality by allowing intersection events to be updated with a new set of segments.
  *
  * @param currentIntersections Maps an intersection-point or overlap segment to the entry used in the queue to
  *                             represent it.
  */
class EventQueue[L <: Line] private (currentIntersections: Map[RPoint, Intersection[L]], queue: PriorityQueue[EventPoint[L]])(implicit ord: Ordering[EventPoint[L]]) {

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

  def update(p: RPoint, updated: Intersection[L]): EventQueue[L] = {
    require(intersectionExists(p), "No intersection to update at point " + p)
    require(updated.coordinate == p, "Coordinate for intersection did not match the intersection point it is updated for.")
    val current = currentIntersections(p)
    make(currentIntersections + (p -> updated), queue.delete(current).insert(updated))
  }

  def intersectionExists(i: RPoint): Boolean = currentIntersections.contains(i)
  def intersectionFor(i: RPoint): Intersection[L] = currentIntersections(i)

  private def make(m: Map[RPoint, Intersection[L]], q: PriorityQueue[EventPoint[L]]) = new EventQueue[L](m, q)

  override def toString: String = if (isEmpty) {
    "Empty queue"
  } else {
    val nl = System.getProperty("line.separator")
    "Event queue:" + nl + queue.values.mkString(s", $nl")
  }

}

object EventQueue {

  def fromSegments[L <: Line](segments: Vector[L]): EventQueue[L] = {
    var id = 0
    var queue = new EventQueue[L]()(eventOrdering[L])
    for (s <- segments) {
      val segment = new Segment(id, s)
      id += 1
      queue = queue.insert(SegmentPoint(segment.source, Source, segment))
      queue = queue.insert(SegmentPoint(segment.target, Target, segment))
    }
    queue
  }

  /** This is the ordering that determines an events position in the priority queue (based on the point it represents).
    * A point 'A is greater than 'B if A's x coordinate is lower than B's. This is to make the sweep line move from
    * the leftmost point towards the rightmost.
    */
  private def eventOrdering[L <: Line] = Ordering.comparatorToOrdering[EventPoint[L]](new Comparator[EventPoint[L]] {
    override def compare(o1: EventPoint[L], o2: EventPoint[L]): Int = if (o1 before o2) 1
    else if (o2 before o1) -1
    else 0
  })

}
