package net.cyndeline.rlcommon.math.geom.spatialIndex

import net.cyndeline.rlcommon.math.geom.{Line, Point, RPoint, SuperCover}

import scala.collection.mutable.ArrayBuffer

/**
  * Detects intersections between a line segment and a segment set by storing every segment in every grid cell
  * (size specified by the user) the line intercepts between its start and stop coordinates. Queries traverses
  * every cell covered by the line and returns detected intersections.
  *
  * Note that the performance of this class breaks down if the set S is not evenly distributed between the cells.
  * If every segment occupies the same cell, the performance degrades to O(n2).
  *
  * @param size The number of segments in the grid.
  */
class ArrayGrid[L <: Line] private (added: Set[L], val size: Int, grid: Vector[Vector[Vector[L]]], cPerCell: Int) {

  /**
    * @param l A line segment.
    * @return True if the grid contains the segment, otherwise false.
    */
  def contains(l: L): Boolean = added.contains(l)

  /** @return How many coordinates on each axis that the grid covers. */
  val gridSize: Int = grid.size * cPerCell

  /** @return How many coordinates each cell in the grid contains. */
  val cellSize: Int = cPerCell

  /** @return How many cells each axis of the grid contains. */
  val cellAmount: Int = grid.size

  /** @return Every segment added to the grid. */
  def values: Set[L] = added

  /**
    * @param lines Line segments to add to the grid.
    * @return A copy of the grid with the segments added.
    */
  def add(lines: Vector[L]) = if (lines.isEmpty) this else iterate(lines, true)
  def add(line: L, ls: L*): ArrayGrid[L] = iterate(line +: ls.toVector, true)

  /**
    * @param lines Line segments to remove from the grid.
    * @return A copy of the grid with the segments removed.
    */
  def remove(lines: Vector[L]) = if (lines.isEmpty) this else iterate(lines, false)
  def remove(line: L, ls: L*): ArrayGrid[L] = iterate(line +: ls.toVector, false)

  /**
    * @param line A line with its endpoints within the grid.
    * @param intersection Function that determines if two segments intersect. Defaults to regular intersection.
    * @return True if a grid cell contains a segment intersecting the line, otherwise false.
    */
  def intersects(line: L, intersection: (L, L) => Boolean = (l1: Line, l2: Line) => l1.intersects(l2)): Boolean = {
    validateInput(line)
    def cellContainsIntersection(p: Point): Boolean = {
      val segments = grid(p.x)(p.y)
      segments.exists(s => intersection(line, s))
    }

    val gridLine = convertLine(line)
    SuperCover.find(gridSize, gridLine, cellContainsIntersection).isDefined
  }

  /**
    * @param line A line with its endpoints within the grid.
    * @param intersection Function that determines if two segments intersect. Defaults to regular intersection.
    * @return Every segment intersecting the line in the grid.
    */
  def intersections(line: L, intersection: (L, L) => Boolean = (l1: L, l2: L) => l1.intersects(l2)): Vector[L] = {
    validateInput(line)
    val gridLine = convertLine(line)
    val points = SuperCover.cover(gridSize, gridLine).iterator
    val result = new ArrayBuffer[L]()

    while (points.hasNext) {
      val p = points.next()
      val lines = grid(p.x)(p.y)
      result ++= lines.filter(l => intersection(l, line))
    }

    result.toVector.distinct
  }

  private def iterate(lines: Vector[L], add: Boolean) = {
    val line = lines.head
    var current = modify(line, add)
    val lss = lines.drop(1).iterator
    while (lss.hasNext) {
      val l = lss.next()
      current = current.modify(l, add)
    }
    current
  }

  private def modify(line: L, add: Boolean): ArrayGrid[L] = {
    validateInput(line)
    if ((add && contains(line)) || (!add && !contains(line))) {
      this
    } else {
      val gridLine = convertLine(line)
      val covered = SuperCover.cover(gridSize, gridLine).iterator
      var finalGrid = grid

      while (covered.hasNext) {
        val p = covered.next()
        val contents = finalGrid(p.x)(p.y)
        val newContent = if (add) line +: contents else contents.diff(Vector(line))
        finalGrid = finalGrid.updated(p.x, finalGrid(p.x).updated(p.y, newContent))
      }

      new ArrayGrid(if (add) added + line else added - line, if (add) size + 1 else size - 1, finalGrid, cPerCell)
    }
  }

  /* Converts a line into starting and ending on the grid cells containing its original coordinates. */
  private def convertLine(line: Line): Line = {
    // Rounded to whole integers before constructing the point
    val start = RPoint(line.start.x.toInt / cPerCell, line.start.y.toInt / cPerCell)
    val stop = RPoint(line.stop.x.toInt / cPerCell, line.stop.y.toInt / cPerCell)
    Line(start, stop)
  }

  /**
    * @param line The users input line.
    */
  private def validateInput(line: Line): Unit = {
    require(line.start.x.isWhole() && line.start.y.isWhole() && line.stop.x.isWhole() && line.stop.y.isWhole(), "Grid cover requires line endpoints to be whole integers.")
    require(line.start.x >= 0 && line.start.y >= 0 && line.stop.x >= 0 && line.stop.y >= 0, "Cannot compute grid overlap on negative coordinates.")
    require(line.start.x < gridSize && line.start.y < gridSize && line.stop.x < gridSize && line.stop.y < gridSize, s"$line exceeds grid coordinates.")
  }

  override def hashCode: Int = grid.## ^ cellSize

  override def equals(other: Any): Boolean = other match {
    case g: ArrayGrid[L] => g.cellSize == cellSize && g.gridSize == gridSize && g.values == values
    case _ => false
  }

  override def toString: String = {
    val builder = new StringBuilder()
    if (added.isEmpty)
      builder ++= String.format("Empty ArrayGrid")
    else {
      builder ++= String.format("ArrayGrid%n")
      for (l <- added)
        builder ++= String.format(s"$l%n")
    }

    builder.toString()
  }
}

/**
  * Factory object.
  */
object ArrayGrid {

  /**
    * Constructs an empty grid.
    * @param gridSize Number of cells in the grid.
    * @param cellSize Number of coordinates each cell should represent.
    * @return An empty nxn grid.
    */
  def apply[L <: Line](gridSize: Int, cellSize: Int) = {
    require(gridSize > 0, "Grid size must be positive")
    require(cellSize > 0, "Cell size must be positive")
    new ArrayGrid[L](Set(), 0, Vector.fill(gridSize, gridSize)(Vector[L]()), cellSize)
  }

}
