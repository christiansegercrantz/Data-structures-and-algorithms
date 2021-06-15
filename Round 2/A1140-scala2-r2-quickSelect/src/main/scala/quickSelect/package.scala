// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object quickSelect {
  // A pseudo-random number generator with a fixed seed
  // so that error situations can be reproduced easier
  val rand = new scala.util.Random(21)

  def swap(a: Array[Int], i: Int, j: Int): Unit = {
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  }

  def partition(a: Array[Int], lo: Int, hi: Int, pivot: Int): Int = {
    var i = lo
    var j = hi
    while(i <= j) {
      while(i < j && a(i) <= pivot) {
        i = i + 1
      }
      do {
        j = j - 1
      } while (i <= j && a(j) > pivot)
      if(i < j)
        swap(a, i, j)
    }
    swap(a, hi, i)
    i
  }

  def threePartPartition(a: Array[Int], lo: Int, hi: Int, pivot: Int): (Int,Int) = {
    var i = lo
    var lt = lo
    var gt = hi
    while (i <= gt){
      if( a(i) < pivot ){
        swap(a, lt, i)
        lt +=  1
        i +=  1
      } 
      else if ( a(i) == pivot){
      i += 1
      } 
      else if  ( a(i) > pivot) {
        swap(a, i, gt)
        gt -=  1
      }
    } 
    return (lt, gt)
  }

  /**
   * Find the k:th smallest (0 for the smallest) element in
   * the integer sequence seq by using the quickselect algorithm.
   */
  def find(seq: Seq[Int], k: Int): Int = {
    require(0 <= k && k < seq.length)
    // Make a working copy of the sequence as we cannot modify the argument sequence
    val a: Array[Int] = seq.toArray
    
    val left = 0
    val right = a.length - 1
    def quickSelect(left: Int, right: Int): Int = {
      if(left == right){
        return a(left)
      }
    val pivotIndex = left + rand.nextInt(right - left)
    swap(a, pivotIndex, left )
    var pivot = a(left)
    val (lb, ub) = threePartPartition(a, left, right, pivot) //for advanced partitioning

    if( lb <= k && k <= ub){
      return a(k)
    } else if ( k < lb){
      return quickSelect(left, lb -1)
    } else {
      return quickSelect(ub + 1, right)
    }
  }
  quickSelect(left, right)
}




}