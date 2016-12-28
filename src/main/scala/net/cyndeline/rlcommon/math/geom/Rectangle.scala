package net.cyndeline.rlcommon.math.geom

/**
  * Rectangle on the cartesian place, aligned with the x and y axis.
  */
class Rectangle (val start: Point, val width: Int, val height: Int) extends Shape[Rectangle] with Ordered[Rectangle] {

  val diagonal: Double = Math.sqrt((width * width) + (height * height))
  val stop: Point = start + (width - 1, height - 1)

  override def points = Vector(start, start + (0, height - 1), stop, start + (width - 1, 0))

  override def +(x: Int, y: Int): Rectangle = Rectangle(start + (x, y), width, height)

  override def -(x: Int, y: Int): Rectangle = Rectangle(start - (x, y), width, height)

  override def *(x: Int, y: Int): Rectangle = Rectangle(start * (x, y), width, height)

  def containsPoint(p: Point): Boolean = {
    val stop = this.stop
    p.x >= start.x && p.x <= stop.x && p.y >= start.y && p.y <= stop.y
  }

  /**
    * @param other Another rectangle that may or may not overlap this one.
    * @return True if the rectangles overlap, even if only on the border.
    */
  def overlaps(other: Rectangle): Boolean = {
    val r1x1 = start.x
    val r1x2 = start.x + width - 1 // -1 Since the end coordinate should be inclusive
    val r2x1 = other.start.x
    val r2x2 = other.start.x + other.width - 1
    val r1y1 = start.y
    val r1y2 = start.y + height - 1
    val r2y1 = other.start.y
    val r2y2 = other.start.y + other.height - 1

    (r1x2 >= r2x1) && (r1y2 >= r2y1) && (r1x1 <= r2x2) && (r1y1 <= r2y2)
  }

  /**
    * @param other Another rectangle.
    * @return The rectangle representing the intersection between this one and the other (width or height == 1 if
    *         intersection occurs on border), or None if the rectangles are disjoint.
    */
  def intersection(other: Rectangle): Option[Rectangle] = if (overlaps(other)) {
    val r1Start = this.start
    val r1Stop = r1Start + (width - 1, height - 1)
    val r2Start = other.start
    val r2Stop = r2Start + (other.width - 1, other.height - 1)

    val startX = if (r1Start.x > r2Start.x) r1Start.x else r2Start.x
    val startY = if (r1Start.y > r2Start.y) r1Start.y else r2Start.y
    val stopX = Math.min(r1Stop.x, r2Stop.x)
    val stopY = Math.min(r1Stop.y, r2Stop.y)

    Some(Rectangle(Point(startX, startY), Point(stopX, stopY)))
  } else {
    None
  }

  /**
    * @param line A line segment.
    * @return True if the line segment cuts the border of the rectangle, or if the rectangle encloses it.
    *         Otherwise false.
    */
  def intersectsLine(line: Line): Boolean = {
    if (containsPoint(line.start) || containsPoint(line.stop)) {
      true
    } else {
      val stop = this.stop
      val left = Line(start, start + (0, height - 1))
      val bottom = Line(start, start + (width - 1, 0))
      val top = Line(start + (0, height - 1), stop)
      val right = Line( start + (width - 1, 0), stop)
      Seq(left, bottom, top, right).map(_.intersection(line)).exists(_.isDefined)
    }

  }

  /**
    * Computes the shortest distance between two rectangles.
    * @param other Another rectangle.
    * @return The distance between a point on each rectangle that lies closest to each other. 0 if the rectangles
    *         share a line segment, -1 if they overlap.
    */
  def shortestDistance(other: Rectangle): Double = {
    if (isSingleCoordinate(this) && isSingleCoordinate(other) && start == other.start)
      return -1

    val thisStart = this.start
    val thisStop = thisStart + (width - 1, height - 1)
    val otherStart = other.start
    val otherStop = otherStart + (other.width - 1, other.height - 1)
    val left = otherStop.x <= thisStart.x
    val right = thisStop.x <= otherStart.x
    val bottom = otherStop.y <= thisStart.y
    val top = thisStop.y <= otherStart.y

    if (top && left)
      Point(thisStart.x, thisStop.y).distanceTo(Point(otherStop.x, otherStart.y))
    else if (left &&  bottom)
      Point(thisStart.x, thisStart.y).distanceTo(Point(otherStop.x, otherStop.y))
    else if (bottom  && right)
      Point(thisStop.x, thisStart.y).distanceTo(Point(otherStart.x, otherStop.y))
    else if (right && top)
      Point(thisStop.x, thisStop.y).distanceTo(Point(otherStart.x, otherStart.y))
    else if (left)
      thisStart.x - otherStop.x
    else if (right)
      otherStart.x - thisStop.x
    else if (bottom)
      thisStart.y - otherStop.y
    else if (top)
      otherStart.y - thisStop.y
    else
      -1
  }

  private def isSingleCoordinate(r: Rectangle) = r.width == 1 && r.height == 1

  override def compare(that: Rectangle): Int = {
    val startOrder = this.start.compare(that.start)
    if (startOrder != 0)
      startOrder
    else {
      val stopOrder = this.stop.compare(that.stop)
      if (stopOrder != 0)
        stopOrder
      else
        0
    }
  }

  override def equals(other: Any): Boolean = other match {
    case r: Rectangle => start == r.start && width == r.width && height == r.height
    case _ => false
  }

  override def hashCode: Int = start.## ^ width.## ^ height.##

  override def toString: String = s"R[$start, width $width, height $height]"

}

object Rectangle {
  def apply(start: Point, width: Int, height: Int): Rectangle = new Rectangle(start, width, height)
  def apply(start: Point, dim: Dimensions): Rectangle = new Rectangle(start, dim.width, dim.height)
  def apply(start: Point, stop: Point): Rectangle = {
    val startX = Math.min(start.x, stop.x)
    val startY = Math.min(start.y, stop.y)
    val width = Math.abs(start.x - stop.x) + 1
    val height = Math.abs(start.y - stop.y) + 1
    new Rectangle(Point(startX, startY), width, height)
  }

  /**
    * Computes a starting coordinate for a rectangle that has a given point p as its center. If the supplied width and
    * height doesn't allow for a center coordinate, the one closer to the start will be used instead.
    * @param p A point to place in the center of the rectangle.
    * @param width Rectangle width.
    * @param height Rectangle height.
    * @return A rectangle with given width and height, with p in its center.
    */
  def centerAround(p: Point, width: Int, height: Int): Rectangle = {
    val xAdjust = adjust(width)
    val yAdjust = adjust(height)
    apply(Point(p.x - xAdjust, p.y - yAdjust), width, height)
  }

  def centerAround(p: Point, dim: Dimensions): Rectangle = centerAround(p, dim.width, dim.height)

  private def adjust(v: Int): Int = if (v % 2 != 0) Math.floor(v / 2d).toInt else (v / 2) - 1
}
