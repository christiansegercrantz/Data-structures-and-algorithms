// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package nutsAndBolts

/**
 * An abstract base class for nuts.
 */
abstract class Nut(name: String) {
  /**
   * Compare this nut to a bolt.
   * Returns -1 if this nut is smaller than, 0 if the same size as,
   * and 1 if larger than the bolt.
   */
  def compare(bolt: Bolt): Int
}
