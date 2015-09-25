package net.cyndeline.rlcommon.util

/**
 * Specifies a width in coordinates for a drawn element on a grid map.
 */
trait WidthConstraint {

  /**
   * The number of tiles (where each tile occupies a single coordinate) corresponding to the elements width.
   * @return The width of the element, must be higher than 0.
   */
  def elementWidth: Int

}
