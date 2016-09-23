package net.cyndeline.rlcommon.math.geom

import spire.math.Rational

/**
  * 2D ellipse.
  *
  * @param center The center coordinate of the ellipse.
  * @param xRadius Radius of the ellipse along the x axis, given that it is angled orthogonally.
  * @param yRadius Radius of the ellipse along the y axis, given that it is angled orthogonally.
  */
class Ellipse(center: RPoint, xRadius: Rational, yRadius: Rational, angle: Int) {

  /**
    * Creates an ellipse with its radius's angled along their axis.
    */
  def this(center: RPoint, xRadius: Rational, yRadius: Rational) = this(center, xRadius, yRadius, 0)

  private val radianAngle = Math.toRadians(angle)

  private val sinRot = Rational(Math.sin(radianAngle))
  private val cosRot = Rational(Math.cos(radianAngle))

  // Workaround: Keep the ellipse in the same position regardless of which side is the longest. Change some day.
  // Major becomes the x axis radius.
  private val majorRadius = xRadius//Math.max(xRadius, yRadius)
  private val minorRadius = yRadius//Math.min(xRadius, yRadius)

  private val k1 = SQR(cosRot / majorRadius) + SQR(sinRot / minorRadius)
  private val k2 = 2 * sinRot * cosRot * ((1 / SQR(majorRadius)) - (1 / SQR(minorRadius)))
  private val k3 = SQR(sinRot / majorRadius) + SQR(cosRot / minorRadius)

  /**
    * @param lStart Line start coordinate (inclusive).
    * @param lStop Line stop coordinate (exclusive)
    * @return None if no intersection exists, otherwise one or two points where the line intersects the ellipse.
    */
  def intersectsLine(lStart: RPoint, lStop: RPoint): Option[Seq[RPoint]] = {
    val x1: Rational = center.x.doubleValue()
    val y1: Rational = center.y.doubleValue()
    val u1: Rational = lStart.x
    val v1: Rational = lStart.y
    val u2: Rational = lStop.x
    val v2: Rational = lStop.y
    val dx: Rational = u2 - u1
    val dy: Rational = v2 - v1
    val q0: Rational = k1 * SQR(u1 - x1) + k2 * (u1 - x1) * (v1 - y1) + k3 * SQR(v1 - y1) - 1
    val q1: Rational = (2 * k1 * dx * (u1 - x1)) + (k2 * dx * (v1 - y1)) + (k2 * dy * (u1 - x1)) + (2 * k3 * dy * (v1 - y1))
    val q2: Rational = (k1 * SQR(dx)) + (k2 * dx * dy) + (k3 * SQR(dy))

    val d: Rational = SQR(q1) - (4 * q0 * q2)

    if (d < 0) {
      None

    } else if (d == 0) {
      val t = -q1 / (2 * q2)

      if (0 <= t && t <= 1) {
        val x = u1 + t * dx
        val y = v1 + t * dy
        Some(Seq(RPoint(x, y)))

      } else {
        None
      }


    } else {
      var n = 0
      var intersections = Seq[RPoint]()
      val q: Rational = Rational(Math.sqrt(d.doubleValue()))
      val t1: Rational = (-q1 - q) / (2 * q2)

      if (0 <= t1 && t1 <= 1) {
        intersections = intersections :+ RPoint(u1 + t1 * dx, v1 + t1 * dy)
        n += 1
      }

      val t2: Rational = (-q1 + q) / (2 * q2)

      if (0 <= t2 && t2 <= 1) {
        if (n == 0) {
          if (intersections.isEmpty)
            intersections = intersections :+ RPoint(u1 + t2 * dx, v1 + t2 * dy)

          n += 1
        } else {
          intersections = intersections :+ RPoint(u1 + t2 * dx, v1 + t2 * dy)

        }
    }

      if (intersections.isEmpty)
        None
      else
        Some(intersections)
    }
  }

  /**
    * Checks if a point lies on or within the circumference of this ellipse.
 *
    * @param p Coordinate to process.
    * @return True if the coordinate lies on or within the ellipses circumference, otherwise false.
    */
  def containsPoint(p: RPoint): Boolean = {
    val dx = (p.x - center.x).doubleValue()
    val dy = (p.y - center.y).doubleValue()
    val eq = (k1 * SQR(dx)) + (k2 * dx * dy) + (k3 * SQR(dy)) - 1
    eq <= 0
  }

  /**
    * Checks if a rectangle intersects the ellipse.
 *
    * @param rStart Start coordinate for the rectangle. Inclusive.
    * @param rStop Stop coordinate for the rectangle. Exclusive.
    * @return Intersects if both shapes share a point, Outside if no intersection occurs and Inside if the entire
    *         rectangle is enclosed by the ellipse.
    */
  def intersectsRectangle(rStart: RPoint, rStop: RPoint): IntersectCase = {
    require(rStart != rStop, "Rectangle start and stop coordinates must be separate. To evaluate a single coordinate, use the Point function.")
    require(rStart.x < rStop.x && rStart.y < rStop.y, "Rectangle stop coordinates must be greater than its start coordinates. To evaluate a line, use line intersection.")

    /* Check all four lines separately */
    if (intersectsLine(RPoint(rStart.x, rStop.y), rStop).isDefined) { // Top
      Intersects

    } else if (intersectsLine(RPoint(rStart.x, rStop.y), rStart).isDefined) { // Left
      Intersects

    } else if (intersectsLine(rStart, RPoint(rStop.x, rStart.y)).isDefined) { // Bottom
      Intersects

    } else if (intersectsLine(rStop, RPoint(rStop.x, rStart.y)).isDefined) { // Right
      Intersects

    } else {

      // With no intersections, if a corner of the rectangle is inside the ellipse, the entire rectangle is.
      if (containsPoint(rStart))
        Inside
      else
        Outside
    }
  }

  private def SQR(value: Rational) = value * value

}
