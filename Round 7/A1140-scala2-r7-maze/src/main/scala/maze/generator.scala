// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package maze

object generator {
  import Maze.Direction._

  /**
   * Generate a (pseudo-random) maze of given dimensions.
   * The maze must not have any cycles and all the cells must be reachable
   * from the entry cell at (0,0).
   */
  def generate(rows: Int, columns: Int, seed: Int = System.nanoTime.toInt): Maze = {
    val rand = new scala.util.Random(seed)
    val maze = new Maze(rows, columns)
    val directions: Seq[Direction] = Seq(Up, Down, Left, Right)
    var visited = scala.collection.mutable.HashSet[(Int,Int)]()

    def hasVisited(row: Int, column: Int, dir: Direction): Boolean = {
      dir match {
        case Up => {
          visited(row+1,column)
        }
        case Down => {
          visited(row-1,column)
        }
        case Left => {
          visited(row,column-1)
        }
        case Right => {
          visited(row,column+1)
        }
      }
    }

    def visit(row: Int,col: Int): Unit = {
      println("Visiting (" + row + "," + col + ")")
      visited((row,col)) = true
      rand.shuffle(directions).foreach( dir => 
        dir match {
          case Up => {
            if(!hasVisited(row,col,Up) && (row+1 < rows)){
              maze.breakWall(row,col,dir)
              visit(row+1,col)
            }
          }
          case Down => {
            if(!hasVisited(row,col,Down) && (row-1 >= 0)){
              maze.breakWall(row,col,dir)
              visit(row-1,col)
            }
          }
          case Left => {
            if(!hasVisited(row,col,Left) && (col-1 >= 0)){
              maze.breakWall(row,col,dir)
              visit(row,col-1)
            }
          }
          case Right => {
            if(!hasVisited(row,col,Right) && (col+1 < columns)){
              maze.breakWall(row,col,dir)
              visit(row,col+1)
            }
          }
        }
      )
    }
    visit(0,0)
    maze
  }
}
