package net.cyndeline.rlcommon.util

/**
 * Specifies a start and stop coordinate for a rectangular area.
 */
trait RectangleCoordinates {

  /**
   * @return The starting point for this rectangular area, must be equal or less than the stop value.
   */
  def start: Point

  /**
   * @return The ending coordinate (inclusive) for this rectangular area.
   */
  def stop: Point
}
