// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package diameter

object graphReader {
  /** A simple method for reading graphs from a file.
   * The first line gives the number of vertices.
   * The second line gives the edges in the for "
   * No error handling.
   */
  def apply(file: java.io.File): Graph = {
    val source = scala.io.Source.fromFile(file, "UTF-8")
    val lines = source.getLines()
    val nofVertices = lines.next().toInt
    require(nofVertices > 0)
    val pair = raw"(\d+),(\d+)".r
    val edges = lines.next().split(";").map(_ match {
      case pair(v1,v2) => (v1.toInt, v2.toInt)
    })                            
    source.close()
    new Graph(nofVertices, edges.toSeq)
  }
}
