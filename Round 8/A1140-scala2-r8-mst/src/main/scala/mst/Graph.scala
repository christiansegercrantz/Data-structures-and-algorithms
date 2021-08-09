// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package mst

import collection.mutable.ArrayBuffer

/**
 * If you want to (but you don't have to) use your own data structure classes,
 * such as union-find, put them here
 */

 class UnionFind[E] {
  /* Insert your internal data structures here.
   * One possible choice is the following but
   * you are free to use your own choices as well:
   * protected val parent = new scala.collection.mutable.HashMap[E, E]()
   * protected val rank = new scala.collection.mutable.HashMap[E, Int]()
   * protected var _nofSets = 0
   */
  protected val parent = new scala.collection.mutable.HashMap[E, E]()
  protected val rank = new scala.collection.mutable.HashMap[E, Int]()
  protected var _nofSets = 0
  protected var _nofElements = 0 

  /**
   * Introduce a new element in this disjoint sets data structure and
   * put it into the set consisting only of the element itself.
   * Does nothing if the element is already in the disjoint sets data structure.
   * Returns true if the element was inserted (i.e., was not already in
   * the data structure), false otherwise.
   * A constant-time operation (actually only on average if hash map
   * searches and insertions are used in the code).
   */
  def makeSet(element: E): Boolean = {
    if(parent.get(element) != None){
      return false
    }
    parent(element) = element
    rank(element) = 0
    _nofElements += 1
    _nofSets += 1
    return true
  }
  
  /**
   * Get the representative element of the given element
   * in the current disjoint sets data structure.
   * Two elements are in the same set if and only if their representatives
   * are the same.
   * The representatives may change during union operations and
   * thus it is *not* safe to use previously calculated representatives
   * after an union operation has been performed.
   * Throws an exception if the element has not been introduced earlier
   * with makeSet.
   * An O(log n) operation on average,
   * where n is the number of elements in all the sets.
   */
  def findSet(element: E): E = {
    require(parent(element) != null)
    var belongsTo: E = parent(element)
    var currentElem = element
    while(belongsTo != currentElem){
      currentElem = belongsTo
      belongsTo = parent(belongsTo)
    }
    return belongsTo
  }

  /**
   * Merge (i.e., make union of) the sets containing the elements
   * element1 and element2.
   * An O(log n) operation, where n is the number of elements in all the sets.
   */
  def union(element1: E, element2: E): Unit = {
    val set1parent = findSet(element1) 
    val set2parent = findSet(element2)
    if(set1parent != set2parent){
      if(rank(set1parent) > rank(set2parent)){
        parent(set2parent) = set1parent
      } else{
        parent(set1parent) = set2parent
        if(rank(set1parent) == rank(set2parent)){
          rank(set2parent) +=  +1
        }
      }
      _nofSets -= 1 
    }
  }

  /** Get the number of elements in this disjoint sets data structure. */
  def nofElements: Int = {
    return _nofElements
  }

  /** Get the number of sets in this disjoint sets data structure. */
  def nofSets: Int = {
    return _nofSets
  }
}

/**
 * A simple immutable class for edge-weighted undirected graphs.
 * The vertices are integers from 0 to nofVertices-1.
 * The edges are of form (vertex1, weight, vertex2).
 * Self-loops are not supported but parallel edges between two vertices are.
 */
class Graph(val nofVertices: Int, edges: Seq[(Int,Int,Int)]) {
  require(nofVertices > 0)

  /**
   * neighbours(u) is the list of pairs (w,v) such {u,v} is an edge of weight w.
   */
  val neighbours = Array.tabulate[ArrayBuffer[(Int,Int)]](nofVertices)(i => new ArrayBuffer[(Int,Int)]())

  /* Validate the input and build the neighbours data structure.
   * Parallel and duplicate edges are allowed, self-loops are not. */
  private def init(edges: Seq[(Int,Int,Int)]) = {
    for((vertex1, w, vertex2) <- edges) {
      require(0 <= vertex1 && vertex1 < nofVertices)
      require(0 <= vertex2 && vertex2 < nofVertices)
      require(vertex1 != vertex2)
      neighbours(vertex1) += ((w,vertex2))
      neighbours(vertex2) += ((w,vertex1))
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
   * Get the maximum degree
   * Constant time operation after the initialization phase.
   */
  val maxDegree = {
    (0 until nofVertices).map(v => degree(v)).max
  }

  /* As the graph data structure here is immutable and we use
   * connectedness as a requirement in many places,
   * compute this information once in the beginning. */
  val isConnected: Boolean = {
    if(nofVertices == 0) true
    else {
      val seen = new Array[Boolean](nofVertices)
      val q = new collection.mutable.Queue[Int]()
      q.enqueue(0)
      seen(0) = true
      var nofSeen = 1
      while(q.nonEmpty) {
        val u: Int = q.dequeue()
        for((w,v) <- neighbours(u) if !seen(v)) {
          q.enqueue(v)
          seen(v) = true
          nofSeen += 1
        }
      }
      nofSeen == nofVertices
    }
  }


  /**
   * Find a minimum spanning tree (MST) of the graph.
   * Return the set of edges in the MST.
   */
  def minimumSpanningTree: Set[(Int,Int,Int)] = {
    require(isConnected)
    val union = new UnionFind[Int]
    val tree = collection.mutable.Set[(Int,Int,Int)]()
    (0 until nofVertices).map(union.makeSet(_))
    val _edges = edges.sortBy(_._2)
    for( (u,w,v) <- _edges){
      if(union.findSet(u) != union.findSet(v)){
        union.union(u,v)
        tree += ((u,w,v))
      }
    }
    tree.toSet
  }


  /**
   * Check if the edge set given as argument forms a spanning tree of the graph.
   * If this is not the case, return None.
   * If yes, return the sum of the weights of the edges in the set.
   */
  def isSpanningTree(treeEdges: Set[(Int,Int,Int)]): Option[Int] = {
    if(nofVertices == 0) {
      if(treeEdges.isEmpty) return Some(0)
      else return None
    }
    // A spanning tree of a graph of n vertices has exactly n-1 edges
    if(treeEdges.size != nofVertices - 1)
      return None

    // Check that all the vertices are reachable from the vertex 0
    // by only using the edges in the argument edge set
    val seen = new Array[Boolean](nofVertices)
    val q = new collection.mutable.Queue[Int]()
    q.enqueue(0)
    seen(0) = true
    var nofSeen = 1
    var weight = 0
    while(q.nonEmpty) {
      val u: Int = q.dequeue()
      for((w,v) <- neighbours(u) if !seen(v)) {
        if(treeEdges((v,w,u)) || treeEdges((u,w,v))) {
          q.enqueue(v)
          seen(v) = true
          nofSeen += 1
          weight += w
        }
      }
    }
    if(nofSeen != nofVertices)
      None
    else
      Some(weight)
 }

}
