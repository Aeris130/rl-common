package net.cyndeline.rlcommon.math.geom

import scala.collection.mutable.ArrayBuffer

/**
  * Given a nxn grid, the supercover computes which cells in the grid a line from point A to B intersects. If a line
  * cuts the intersection of two cells, both cells are reported.
  */
object SuperCover {

  /**
    * @param size The size n of a nxn grid.
    * @param line A line on the grid.
    * @return Every gridd cell intersected by the line.
    */
  def cover(size: Int, line: Line): Vector[Point] = {
    computeCover(size, line) match {
      case Right(v) => v
      case _ => throw new Error(s"Failed to compute spercover of grid with size $size using line $line")
    }
  }

  /**
    * @param size The size n of a nxn grid.
    * @param line A line on the grid.
    * @param isValid Function that returns true for some point on the grid.
    * @return The first found point the validates using the function, or None of no such point exists.
    */
  def find(size: Int, line: Line, isValid: Point => Boolean): Option[Point] = {
    computeCover(size, line, isValid) match {
      case Left(p) => Some(p)
      case _ => None
    }
  }

  /* http://playtechs.blogspot.se/2007/03/raytracing-on-grid.html */
  private def computeCover(size: Int, line: Line, valid: Point => Boolean = (p: Point) => false): Either[Point, Vector[Point]] = {
    boundaryCheck(size, line)
    require(line.start.x.isWhole() && line.start.y.isWhole() && line.stop.x.isWhole() && line.stop.y.isWhole(), "Grid cover requires line endpoints to be whole integers.")
    val result = new ArrayBuffer[Point]()
    val x1 = line.start.x.toInt
    val x2 = line.stop.x.toInt
    val y1 = line.start.y.toInt
    val y2 = line.stop.y.toInt
    var dx = Math.abs(x2 - x1)
    var dy = Math.abs(y1 - y2)
    var x = x1
    var y = y1
    var n = 1 + dx + dy
    val xInc = if (x2 > x1) 1 else -1
    val yInc = if (y2 > y1) 1 else -1
    var error = dx - dy
    dx *= 2
    dy *= 2

    var done = false

    while (n > 0) {
      val p = Point(x, y)

      if (valid(p))
        return Left(p)
      else {
        result += p

        if (p.x == x2 && p.y == y2)
          done = true
      }


      if (error > 0) {
        x += xInc
        error -= dy
      } else {
        y += yInc

        if (error == 0 && !done) {
          val p = Point(x + xInc, y - yInc)
          if (valid(p))
            return Left(p)
          else
            result += p
        }

        error += dx
      }

      n -= 1
    }

    Right(result.toVector)
  }

  private def boundaryCheck(size: Int, line: Line): Unit = {
    require(line.start.x >= 0 && line.start.y >= 0 && line.stop.x >= 0 && line.stop.y >= 0, "Cannot compute grid overlap on negative coordinates.")
    require(line.start.x < size && line.start.y < size && line.stop.x < size && line.stop.y < size, "Supercover line cannot exceed grid boundaries.")
  }

}
