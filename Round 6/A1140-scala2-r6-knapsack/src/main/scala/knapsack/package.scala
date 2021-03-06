import scala.collection.mutable
// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Teemu Pudas and Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object knapsack {
  /**
   * 
   * A recursive backtracking search algorithm that takes exponential time in
   * the number of items.
   * Given for reference and comparison purposes.
   */
  def solveRecursive(maxWeight: Int, items: Seq[Item]): (Int, Seq[Item]) = {
    // Check that all weights and values are positive
    items.foreach({case Item(weight, value) => {
      require(weight > 0)
      require(value > 0)
    }})
    /*
     * An inner function for performing the actual search
     */
    def search(currentMaxW: Int, remainingItems: List[Item]): (Int, List[Item]) = {
      if(remainingItems.isEmpty) (0, Nil)
      else {
        val item = remainingItems.head
        // Get the best value when the item is included
        val included =
          if(item.weight > currentMaxW) (0, Nil)
          else {
            val (subValue, subSol) = search(currentMaxW - item.weight, remainingItems.tail)
            (subValue + item.value, item :: subSol)
          }
        // Get the best value when the item is not included
        val excluded = search(currentMaxW, remainingItems.tail)
        if(included._1 > excluded._1) included else excluded
      }
    }
    // Do the actual recursive seach for the solution
    // A heuristic: choose heavy objects first
    search(maxWeight, items.sortBy(_.weight).reverse.toList)
  }

  /**
   * Given a maximum weight and a list of objects as (weight, value) pairs,
   * find the highest-value subset of the items that weighs at most maxWeight.
   * Uses dynamic programming and should be much better than the recursive
   * search approach for instances with many items but
   * relatively small maxWeight.
   */
  def solveDynProg(maxWeight: Int, items: Seq[Item]): (Int, Seq[Item]) = {
    // Check that all weights and values are positive
    items.foreach({case Item(weight, value) => {
      require(weight > 0)
      require(value > 0)
    }})
    val n = items.size
    val _items = items.toArray.sortBy(x => -x.value/x.weight)
    val m = new Array[Array[(Int)]](n+1)
    m(0) = Array.fill(maxWeight+1)(0)
    var i = 1
    while( i <= n){
      m(i) = Array.fill(maxWeight+1)(0)
      var j = 1
      while(j <= maxWeight){
        if(_items(i-1).weight > j){
          m(i)(j) = m(i-1)(j)
        }
        else{
          if(m(i-1)(j) >= (m(i-1)(j-_items(i-1).weight) + _items(i-1).value)){
            m(i)(j) = m(i-1)(j)
          }
          else{
            m(i)(j) = m(i-1)(j-_items(i-1).weight) + _items(i-1).value
            
          }
        }
        j += 1
      }
      i += 1
    }
    val solItems = new scala.collection.mutable.ArrayBuffer[Item]()
    var currentWeight = maxWeight
    var h = items.size
    while(currentWeight > 0 && h > 0 ){
      if(m(h)(currentWeight) == m(h-1)(currentWeight)){
        h -= 1
      } else{
        solItems += _items(h-1)
        currentWeight -= _items(h-1).weight
        h -= 1
      }
    }
    val test = solItems.toSeq
    (m(n)(maxWeight), test)
  }
}

/*
def solveDynProg(maxWeight: Int, items: Seq[Item]): (Int, Seq[Item]) = {
    // Check that all weights and values are positive
    items.foreach({case Item(weight, value) => {
      require(weight > 0)
      require(value > 0)
    }})
    val n = items.size
    val _items = items.toArray.sortBy(x => -x.value/x.weight)
    val m = new Array[Array[(Int, List[Item])]](n+1)
    m(0) = Array.fill(maxWeight+1)(0, Nil)
    var i = 1
    while( i <= n){
      m(i) = Array.fill(maxWeight+1)(0, Nil)
      var j = 1
      while(j <= maxWeight){
        if(_items(i-1).weight > j){
          m(i)(j) = m(i-1)(j)
        }
        else{
          if(m(i-1)(j)._1 >= (m(i-1)(j-_items(i-1).weight)._1 + _items(i-1).value)){
            m(i)(j) = (m(i-1)(j)._1,m(i-1)(j)._2)
          }
          else{
            m(i)(j) = (m(i-1)(j-_items(i-1).weight)._1 + _items(i-1).value, m(i-1)(j-_items(i-1).weight)._2 ++ List(_items(i-1)))
            
          }
        }
        j += 1
      }
      i += 1
    }
    (m(n)(maxWeight)._1, m(n)(maxWeight)._2.toSeq)
  }*/
