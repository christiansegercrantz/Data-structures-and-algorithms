// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package bellmanFord

import collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

/**
 * A simple class for directed, edge-weighted graphs.
 * The vertices are integers from 0 to nofVertices-1.
 * The edges are of form (source, weight, target).
 * Parallel edges between two vertices are allowed.
 * Self-loops are not allowed.
 */
class Graph(val nofVertices: Int, edges: Seq[(Int,Int,Int)]) {
  require(nofVertices > 0)

  /**
   * outEdges(u) is an array of pairs (w,v), each pair (w,v) describing
   * an edge from u to v with weight w.
   */
  val outEdges: Array[ArrayBuffer[(Int,Int)]] = Array.fill(nofVertices)(new ArrayBuffer[(Int,Int)]())

  /* Build the edge data structures and validate */
  def init(edges: Seq[(Int,Int,Int)]) = {
    var posEdgeWeights = 0
    var negEdgeWeights = 0
    for((source, weight, target) <- edges) {
      require(0 <= source && source < nofVertices)
      require(0 <= target && target < nofVertices)
      outEdges(source) += ((weight, target))
      if(weight >= 0) {
        require(Int.MaxValue - weight > posEdgeWeights, "The sum of positive edge weights must be less than Int.MaxValue")
        posEdgeWeights += weight
      } else {
        require(Int.MinValue - weight < negEdgeWeights, "The sum of negative edge weights must be more than Int.MinValue")
        negEdgeWeights += weight
      }
    }
  }
  init(edges)


  /**
   * Abstract base class for the result of the Bellman-Ford shortest paths
   * with negative cycle detection algorithm.
   */
  abstract class BellmanFordResult {}
  /**
   * The result when there are no negative cycles reachable from the source vertex.
   * The 'values' is a map where each mapping (v -> d) gives
   * the shortest distance d from the source vertex to the vertex v
   * for those vertices that are reachable from the source;
   * the values for those not reachable from the source vertex are undefined.
   */
  case class BellmanFordDistances(values: Map[Int,Int]) extends BellmanFordResult{}
  /**
   * The result when there is a negative cycle reachable from the source vertex.
   * The 'cycle' sequence gives a simple path that forms a negative cycle;
   * thus it should start and end with the same vertex.
   */
  case class BellmanFordNegativeCycle(cycle: Seq[Int]) extends BellmanFordResult{}
  /**
   * Compute the shortest distances from the source vertex or
   * return a negative cycle if such is reachable from the source vertex.
   * It is assumed that the shortest distances for the reachable vertices
   * are at most Int.Maxvalue.
   */
  def bellmanFord(source: Int): BellmanFordResult = {
    //val visited = new scala.collection.mutable.HashSet[(Int,Int)]()

    val distance = new scala.collection.mutable.HashMap[Int, Int]()
    val parent = new scala.collection.mutable.HashMap[Int, Int]()
    //val que = new scala.collection.mutable.PriorityQueue[((Int,Int),Int)]()(Ordering.by(-_._2))

    for(i <- 0 until nofVertices){
      distance(i) = Int.MaxValue
    }
    distance(source) = 0
    def _relax(u: Int, w: Int, v: Int): Unit = {
      if(distance(u) + w < distance(v)){
        distance(v) = distance(u) + w
        parent(v) = u
      }
    }
    for(n <- 0 until nofVertices){
      for((u,w,v) <- edges){
        _relax(u,w,v)
      }
    }
    for((u,w,v) <- edges){
      if(distance(u) + w < distance(v)){
        val visited = scala.collection.mutable.HashSet[Int](u)
        var current = parent(u)
        while(current != u && !visited(current)){
          visited(current) = true
          current = parent(current)
        }
        val startofLoop = current
        val cycle = ListBuffer[Int](startofLoop)
        current = parent(current)
        while(current != startofLoop){
          cycle.prepend(current)
          current = parent(current)
        }
        cycle.prepend(current)
        return BellmanFordNegativeCycle(cycle.toSeq)
      }
    }

    return BellmanFordDistances(distance.toMap)
  }

  /**
   * Check if 'path' is a negative cycle.
   * Slowish, mainly for debugging and validation purposes.
   */
  def isNegativeCycle(path: Seq[Int]): Boolean = {
    if(path.isEmpty) return false
    val iter = path.iterator
    var current = iter.next()
    assert(0 <= current && current < nofVertices)
    val first = current
    var length = 0
    while(iter.hasNext) {
      val next = iter.next()
      assert(0 <= next && next < nofVertices)
      val out = outEdges(current)
      var smallestWeight = Int.MaxValue
      for((w,t) <- out if t == next)
        if(w < smallestWeight)
          smallestWeight = w
      if(smallestWeight == Int.MaxValue) return false
      length += smallestWeight
      current = next
    }
    // Is it a cycle?
    if(current != first) return false
    // And a negative one?
    return length < 0
  }
}
