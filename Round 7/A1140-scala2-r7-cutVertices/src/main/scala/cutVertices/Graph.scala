// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package cutVertices

import collection.mutable.{ArrayBuffer,Queue}

/**
 * A simple immutable class for undirected graphs.
 * The vertices are integers from 0 to nofVertices-1.
 * The edges are of form (vertex1, vertex2).
 * Self-loops and parallel edges between two vertices are not supported.
 */
class Graph(val nofVertices: Int, edges: Seq[(Int,Int)]) {
  require(nofVertices > 0)

  /**
   * neighbours(u) is the list of vertices v such {u,v} is an edge.
   */
  val neighbours = Array.tabulate[ArrayBuffer[Int]](nofVertices)(i => new ArrayBuffer[Int]())

  /* Validate the input and build the neighbours data structure.
   * Duplicate edges are discarded. */
  private def init(edges: Seq[(Int,Int)]) = {
    val seen = collection.mutable.HashSet[(Int,Int)]()
    for((vertex1, vertex2) <- edges if !seen((vertex1, vertex2))) {
      require(0 <= vertex1 && vertex1 < nofVertices)
      require(0 <= vertex2 && vertex2 < nofVertices)
      neighbours(vertex1) += vertex2
      neighbours(vertex2) += vertex1
      seen((vertex1,vertex2)) = true
      seen((vertex2,vertex1)) = true
    }
  }
  init(edges)

  /**
   * Get the degree of a vertex.
   */
  def degree(v: Int): Int = {
    require(0 <= v && v < nofVertices)
    neighbours(v).length
  }

  /**
   * Get the maximum degree.
   * A constant time operation after the initialization phase.
   */
  val maxDegree = {
    (0 until nofVertices).map(v => degree(v)).max
  }

  /* As the graph data structure here is immutable and
   * we use connectedness as a requirement in many places,
   * compute this information once in the beginning. */
  val isConnected: Boolean = {
    if(nofVertices == 0) true
    else {
      val seen = new Array[Boolean](nofVertices)
      val q = new scala.collection.mutable.Queue[Int]()
      q.enqueue(0)
      seen(0) = true
      var nofSeen = 1
      while(q.nonEmpty) {
        val v: Int = q.dequeue()
        for(w <- neighbours(v) if !seen(w)) {
          q.enqueue(w)
          seen(w) = true
          nofSeen += 1
        }
      }
      nofSeen == nofVertices
    }
  }

  /**
   * Find the cut vertices in the graph.
   * A straightforward, O(|V|(|V|+|E|)) time solution:
   * for each vertex, virtually delete the vertex from the graph and
   * check whether the resulting graph has more connected components.
   */
  def cutVerticesSlow: Set[Int] = {
    val reachable = new Array[Boolean](nofVertices)
    val queue = new Queue[Int]()

    // Mark the vertices that are reachable from 'source'
    // without visiting 'exclude' in the array 'reachable'
    def reach(source: Int, exclude: Int) = {
      require(0 <= source && source < nofVertices)
      require(0 <= exclude && exclude < nofVertices)
      require(source != exclude)
      // Reset the 'reachable' array and the search queue
      for(v <- 0 until nofVertices) reachable(v) = false
      queue.clear()
      queue.enqueue(source)
      reachable(source) = true
      while(queue.nonEmpty) {
        val v = queue.dequeue()
        for(w <- neighbours(v) if w != exclude) {
          if(!reachable(w)) {
            reachable(w) = true
            queue.enqueue(w)
          }
        }
      }
    }

    val cutVertices = ArrayBuffer[Int]()
    for(v <- 0 until nofVertices if neighbours(v).nonEmpty) {
      reach(neighbours(v).head, v)
      if(!neighbours(v).forall(w => reachable(w)))
        cutVertices += v
    }
    cutVertices.toSet
  }

  /**
   * Find the cut vertices in the graph.
   * A faster, O(|V|+|E|) time algorithm.
   * http://en.wikipedia.org/wiki/Biconnected_component
   */
  def cutVertices: Set[Int] = {
    import scala.collection.mutable._

    val visited = HashSet[Int]()
    val depth = HashMap[Int,Int]()
    val low = HashMap[Int,Int]()
    val parent = HashMap[Int,Int]()
    val childCount = HashMap[Int,Int]()
    val isCutVertices = HashSet[Int]()

    class Frame(val currentNode: Int, val depth: Int){
      var iteration: Int = 0
      var isCV: Boolean = false
    }
    val stack = new collection.mutable.Stack[Frame]()
    stack.push(new Frame(0,0))
    while(stack.nonEmpty){
      val currentFrame = stack.top
      val v = currentFrame.currentNode
      visited(v) = true
      depth(v) = currentFrame.depth
      low.getOrElse(v, low(v) = currentFrame.depth)
      val neighbourIterator = neighbours(v).iterator
      var i = 0
      while(i < currentFrame.iteration && neighbourIterator.hasNext){
        neighbourIterator.next()
        i += 1
      }
      if(neighbourIterator.hasNext){
        val u = neighbourIterator.next()
        currentFrame.iteration += 1
        if(!visited(u)){
          if( v == 0){
            1+1
          }
          childCount(v) = childCount.getOrElse(v, 0) + 1 
          parent(u) = v
          stack.push(new Frame(u, currentFrame.depth + 1))
        } else if(u != parent.getOrElse(v, null)){
            low(v) = Math.min(low(v), depth(u))
        }
      } else{
          if(parent.getOrElse(v, null) != null){
            val vParent = parent(v)
            if(low(v) >= depth(vParent)){
              val lowTest = low(v)
              val depthTest = depth(vParent)
              if(vParent != 0){
                isCutVertices(vParent) = true
              }
            }
            low(vParent) = Math.min(low(vParent), low(v))
          }
          stack.pop()
      }
    }
    if(childCount(0) > 1){
      isCutVertices(0) = true
    }
    isCutVertices.toSet
  }


  /**
   * Output the graph in the JSON (JavaScript Object Notation,
   * http://www.json.org/) format that our simple visualizer understands.
   * - 'names' gives the name for each vertex, "" is used if missing for a vertex
   * - 'coloring' is a mapping that associates vertices in the graph to
   *   a Double in the range [-1.0,1.0]; if some vertex is not associated to
   *   any value by coloring, it is implicitly associated to 0.0.
   */
  def toJSON(names: Map[Int, String] = Map[Int, String](),
             coloring: Map[Int, Double] = Map[Int, Double]()): String = {
    coloring.foreach({ case (v, c) => require(-1.0 <= c && c <= 1.0, "The colors should be in range [-1.0,1.0] but the vertex '" + v + "' has color " + c) })
    def name(v: Int) = names.getOrElse(v, v.toString)
    def color(v: Int) = coloring.getOrElse(v, 0.0)
    val vString = "  " + (0 until nofVertices).map(v => s"""{"name":"${name(v)}","group":${color(v)}}""").mkString(",\n  ")
    val lString = "  " + (for (v <- 0 until nofVertices; w <- neighbours(v) if w <= v) yield (
      s"""{"source":${v},"target":${w},"value":1}""")).mkString(",\n  ")
    "{ \"nodes\":[\n" + vString + "\n], \"links\":[\n" + lString + "\n]}"
  }
}
