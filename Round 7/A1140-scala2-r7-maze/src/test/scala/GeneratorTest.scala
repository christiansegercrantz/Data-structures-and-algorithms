// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import maze._

class GeneratorSpec extends AnyFlatSpec with Matchers {
  "The generator object" should "generate mazes in which all cells are reachable from (0,0)" in {
    val maze = generator.generate(20, 30, 2105)
    maze.checkReachable should be (true)
  }

  /**
   * Check for acyclicity is not provided, feel free to write one if you like.
   * Acyclicity will be tested in the home assignment grader.
   */
}
