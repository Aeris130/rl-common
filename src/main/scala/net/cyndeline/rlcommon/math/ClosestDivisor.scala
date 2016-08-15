package net.cyndeline.rlcommon.math

/**
  * Takes a divisor and a number, then computes the divisor without remainder based on that number that is closest
  * to the input divisor. Example: The number 21 and the divisor 6 will find that 7 is the closest even divisor.
  *
  * The result is found by brute-forcing divisor comparisons, looking at n+1, n-1, n+2, n-2 etc. where n is the initial
  * divisor.
  *
  * @constructor Creates a new divisor finder.
  */
class ClosestDivisor {

  /**
    * Finds the closest evenly divisor of a number.
    * @param value Value to divide.
    * @param initialDivisor The starting point of the divisor search.
    * @return The divisor that divides the input value without remainder and is closer to the initial divisor than
    *         any other number.
    */
  def findClosestDivisor(value: Int, initialDivisor: Int): Int = {
    if (initialDivisor == 0) return 1
    if (value % initialDivisor == 0) return initialDivisor

    var modification = 1

    /* No need to catch if the current divisor being tried is 0 here, as the divisor 1 or -1 will always yield a result
     * first.
     */
    while(true) {
      if (value % (initialDivisor + modification) == 0) return initialDivisor + modification
      else if (value % (initialDivisor - modification) == 0) return initialDivisor - modification
      else modification += 1
    }

    0 // This can't happen, but if it does it's sure to crash the application
  }
}
