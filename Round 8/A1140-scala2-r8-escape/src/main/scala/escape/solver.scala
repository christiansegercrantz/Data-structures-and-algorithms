// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package escape

import scala.collection.mutable

object solver {
  /**
   * Find a shortest-duration way from the world.startPosition to
   * a safe position in the world and return the path in form of
   * a sequence of (row,column) coordinates.
   * You can use the World.neighbours method for finding the valid
   * neighbour coordinates of a coordinate.
   */
  def solve(world: World): Seq[(Int, Int)] = {
    val visited = new scala.collection.mutable.HashSet[(Int,Int)]()
    val distance = new scala.collection.mutable.HashMap[(Int,Int), Int]()
    val parent = new scala.collection.mutable.HashMap[(Int,Int), (Int, Int)]()
    val que = new scala.collection.mutable.PriorityQueue[((Int,Int),Int)]()(Ordering.by(-_._2))
    var path = scala.collection.mutable.ListBuffer[(Int,Int)]()
    
    for(r <- 0 until world.nofRows ; c <- 0 until world.nofColumns){
      if((r,c) != world.startPosition){
        distance((r,c)) = Int.MaxValue
        que.enqueue(((r,c), Int.MaxValue))
      }
    }
    que.enqueue(((world.startPosition),0))
    distance(world.startPosition) = 0

    while(!que.isEmpty && path == mutable.Seq[(Int,Int)]()){
      val (currentBlock, dist) = que.dequeue()
      if(world(currentBlock) == Safe){
        visited(currentBlock)
        path.prepend(currentBlock)
        var x = currentBlock
        while(parent.contains(x)){
          path.prepend(parent(x)) 
          x = parent(x)
        }
      }
      if(!visited(currentBlock)){
        visited(currentBlock) = true
        val neighbours = world.neighbours(currentBlock)
        neighbours.foreach(neighbour => {
          if(distance(currentBlock) + world(neighbour).duration < distance(neighbour)){
            distance(neighbour) = dist + world(neighbour).duration
            parent(neighbour) = currentBlock
            que.enqueue((neighbour, distance(neighbour)))
          }
        })
      }
    }

    path.toSeq
  }
}
