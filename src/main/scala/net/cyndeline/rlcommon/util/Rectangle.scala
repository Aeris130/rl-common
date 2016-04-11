package net.cyndeline.rlcommon.util

import net.cyndeline.rlcommon.math.geom.Point

/**
 * Specifies a start and stop coordinate for a rectangular area.
 */
trait Rectangle {

  /**
   * @return The starting point for this rectangular area, must be equal or less than the stop value.
   */
  def start: Point

  /**
   * @return The ending coordinate (inclusive) for this rectangular area.
   */
  def stop: Point
}
