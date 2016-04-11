package net.cyndeline.rlcommon.math.geom

/**
  * Rectangle on the cartesian place, aligned with the x and y axis.
  */
class Rectangle (val start: Point, val width: Int, val height: Int) {

  val diagonal: Double = Math.sqrt((width * width) + (height * height))

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

  override def equals(other: Any): Boolean = other match {
    case r: Rectangle => start == r.start && width == r.width && height == r.height
    case _ => false
  }

  override def hashCode: Int = start.## ^ width.## ^ height.##

  override def toString: String = "R[" + start + ", width " + width + ", height " + height + "]"

}

object Rectangle {
  def apply(start: Point, width: Int, height: Int): Rectangle = new Rectangle(start, width, height)
}
