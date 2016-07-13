package net.cyndeline.rlcommon.math.geom

/**
  * A 2D line.
  *
  * @param start Start point for this line segment.
  * @param stop end point for this line segment, inclusive.
  */
class Line(val start: Point, val stop: Point) {
  private val epsilon = 1e-6

  val length: Double = {
    val width = Math.abs(start.x - stop.x) + 1
    val height = Math.abs(start.y - stop.y) + 1
    Math.sqrt(width * width + height * height)
  }

  /**
    * @param other Another line.
    * @return True if this line is collinear with the other, otherwise false.
    */
  def collinearWith(other: Line): Boolean = {
    val a = this.start
    val b = this.stop
    val c = other.start
    val d = other.stop

    val denom: Long = ((b.x.toLong - a.x.toLong) * (d.y.toLong - c.y.toLong)) -
                      ((d.x.toLong - c.x.toLong) * (b.y.toLong - a.y.toLong))

    denom == 0
  }

  /**
    * @return The slope for this line, unspecified for vertical lines.
    */
  def slope: Double = {
    assert(start.x != stop.x, "Attempted to compute slope for a vertical segment.")
    (stop.y - start.y).toDouble / (stop.x - start.x)
  }

  def intersection(other: Line): Option[LineIntersection] = {
    val pointIntersect = intersectionPoint(other)

    if (pointIntersect.isDefined) {
      pointIntersect
    } else {
      val overlap = intersectionInterval(other)

      if (overlap.isDefined)
        overlap
      else {
        // No inner intersection and no overlap found. Finish by checking the endpoints.
        if (this.containsPoint(other.start))
          Some(LineIntersection(other.start))
        else if (this.containsPoint(other.stop))
          Some(LineIntersection(other.stop))
        else if (other.containsPoint(this.start))
          Some(LineIntersection(this.start))
        else if (other.containsPoint(this.stop))
          Some(LineIntersection(this.stop))
        else
          None
      }
    }
  }

  /**
    * @param p A point to check overlap for.
    * @return True if the point lies on this line segment, otherwise false.
    */
  def containsPoint(p: Point): Boolean = containsPoint(p.toDouble)

  /**
    * @param p A point to check overlap for.
    * @return True if the point lies on this line segment, otherwise false.
    */
  def containsPoint(p: DPoint): Boolean = {
    val a = start.toDouble
    val b = stop.toDouble
    val c = p

    val crossProduct = (c.y - a.y) * (b.x - a.x) - (c.x - a.x) * (b.y - a.y)
    val dotProduct = (c.x - a.x) * (b.x - a.x) + (c.y - a.y) * (b.y - a.y)
    val squaredLengthBA = (b.x - a.x)*(b.x - a.x) + (b.y - a.y)*(b.y - a.y)

    if (Math.abs(crossProduct) > epsilon) {
      false

    } else if (dotProduct < 0) {
      false

    } else if (dotProduct > squaredLengthBA) {
      false

    } else {
      true
    }
  }

  /**
    * Splits this line into n number of coordinates with equal distance between each other.
    * @param n Number of coordinates to split into, must be 2 or greater.
    * @return n coordinate points, signifying sub intervals along the original line.
    */
  def split(n: Int): Vector[DPoint] = {
    require(n > 1, "The number of splits must be > 1 (currently " + n + ").")
    // Actually, this formula computes the points that are 1/n away from the end point, but not the end point itself.
    // To include it, compute the n - 1 points instead.
    val d = n.toDouble - 1
    (for (i <- 0 until n) yield DPoint(start.x + ((i/d) * (stop.x - start.x)), start.y + ((i/d) * (stop.y - start.y)))).toVector
  }

  /**
    * Original code example: "Real-Time Collision Detection by Christer Ericson, published by Morgan Kaufmaan
    * Publishers, © 2005 Elvesier Inc".
    *
    * @param other Another line to check for intersection.
    * @return None if the lines do not intersect, or if they run parallel inside each other (even partly). Otherwise
    *         the single coordinate where they intersect is returned.
    */
  private def intersectionPoint(other: Line): Option[LineIntersection] = {
    val a = start
    val b = stop
    val c = other.start
    val d = other.stop

    // signs of areas correspond to which side of ab points c and d are
    val a1 = signed2DTriArea(a,b,d); // Compute winding of abd (+ or -)
    val a2 = signed2DTriArea(a,b,c); // To intersect, must have sign opposite of a1

    // If c and d are on different sides of ab, areas have different signs
    if( a1 * a2 < 0.0f ) // require unsigned x & y values.
    {
      val a3 = signed2DTriArea(c,d,a); // Compute winding of cda (+ or -)
      val a4 = a3 + a2 - a1; // Since area is constant a1 - a2 = a3 - a4, or a4 = a3 + a2 - a1

      // Points a and b on different sides of cd if areas have different signs
      if( a3 * a4 < 0.0f )
      {
        // Segments intersect. Find intersection point along L(t) = a + t * (b - a).
        val t = a3 / (a3 - a4)
        val p = a + ((b - a) * t) // the point of intersection
        return Some(LineIntersection(p))
      }
    }

    // Segments not intersecting or collinear
    None

  }

  /**
    * @param other Another line segment to check intersection for.
    * @return A segment interval that represents the overlap between the other segment and this one.
    */
  private def intersectionInterval(other: Line): Option[LineIntersection] = {

    /* Triangular area. */
    def area2(a: Point, b: Point, c: Point): Int = ((b.x - a.x) * (c.y - a.y)) - ((c.x - a.x) * (b.y - a.y))
    def collinear(a: Point, b: Point, c: Point): Boolean = area2(a, b, c) == 0

    /* Checks if c is between a and b. */
    def between(a: Point, b: Point, c: Point): Boolean = if (a.x != b.x) {
      (a.x <= c.x && c.x <= b.x) || (b.x <= c.x && c.x <= a.x)
    } else {
      (a.y <= c.y && c.y <= b.y) || (b.y <= c.y && c.y <= a.y)
    }

    val a = this.start
    val b = this.stop
    val c = other.start
    val d = other.stop

    if (collinearWith(other)) { // Parallel lines

      /* Two segments a-----b and c-----d */
      if (!collinear(a, b, c))
        None
      else if (between(c, d, a) && between(c, d, b)) // c---a-----b---d
        buildIntersectionSegment(a, b)
      else if (between(a, b, c) && between(a, b, d)) // a---c------d----b
        buildIntersectionSegment(c, d)
      else if (between(c, d, a) && !between(c, d, b)) // c-----a-------d----b
        buildIntersectionSegment(a, d)
      else if (!between(c, d, a) && between(c, d, b)) // a-----c------b-----d
        buildIntersectionSegment(c, b)
      else // a----b c----d or c----d a----b
        None

    } else {
      None
    }

  }

  private def signed2DTriArea(a: DPoint, b: DPoint, c: DPoint): Double = (a.x - c.x) * (b.y - c.y) - (a.y - c.y) * (b.x - c.x)

  private def buildIntersectionSegment(a: Point, b: Point) = {
    val smallestFirst = orderPoints(a, b)
    Some(LineIntersection(smallestFirst._1, smallestFirst._2))
  }

  private def orderPoints(a: Point, b: Point): (Point, Point) = {
    if (a.x < b.x) (a, b)
    else if (a.y < b.y) (a, b)
    else if (b.x < a.x) (b, a)
    else if (b.y < a.y) (b, a)
    else (a, b)
  }

  override def equals(other: Any): Boolean = other match {
    case l: Line => start.x == l.start.x && start.y == l.start.y && stop.x == l.stop.x && stop.y == l.stop.y
    case _ => false
  }

  override def hashCode: Int = start.## ^ stop.##

  override def toString: String = "Line[" + start + " to " + stop + "]"

}

/**
  * Factory object.
  */
object Line {
  def apply(from: Point, to: Point): Line = new Line(from, to)
  def apply(from: (Int, Int), to: (Int, Int)) = new Line(Point(from), Point(to))
}
