package net.cyndeline.rlcommon.math.geom

/**
  * A 2D line.
  *
  * @param start Start point for this line segment.
  * @param stop end point for this line segment, inclusive.
  */
class Line(val start: DPoint, val stop: DPoint) {
  private val epsilon = 1e-3

  val length: Double = {
    val width = Math.abs(start.x - stop.x) + 1
    val height = Math.abs(start.y - stop.y) + 1
    Math.sqrt(width * width + height * height)
  }

  /**
    * Original code example: "Real-Time Collision Detection by Christer Ericson, published by Morgan Kaufmaan P
    * ublishers, © 2005 Elvesier Inc".
 *
    * @param other Another line to check for intersection.
    * @return None if the lines do not intersect, or if they run parallel inside each other (even partly). Otherwise
    *         the single coordinate where they intersect is returned.
    */
  def intersectionPoint(other: Line): Option[DPoint] = {
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
        return Some(p)
      }
    }

    // Segments not intersecting or collinear
    None

  }

  /**
    * @param p A point to check overlap for.
    * @return True if the point lies on this line segment, otherwise false.
    */
  def containsPoint(p: DPoint): Boolean = {
    val a = start
    val b = stop
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

  def signed2DTriArea(a: DPoint, b: DPoint, c: DPoint): Double = (a.x - c.x) * (b.y - c.y) - (a.y - c.y) * (b.x - c.x)

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
  def apply(from: DPoint, to: DPoint): Line = new Line(from, to)
}
