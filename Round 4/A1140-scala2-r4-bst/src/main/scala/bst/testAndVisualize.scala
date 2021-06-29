// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package bst

/**
 * A small helper for visualizing binary trees.
 * Inserting requires that both TreeMap.get and 
 * TreeMap.insert are implemented. Removing requires
 * only the proper method, however.
 */
object testAndVisualize {
  def main(args: Array[String]): Unit = {
    val gui = new GUI
    gui.visible = true
  }
}
