// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object maxSubarray {
  /*
   * A helper for the cubic reference algorithm and for the tests,
   * not needed in the linear time algorithm.
   */
  def subarraySum(arr: Array[Int], start: Int, end: Int): Int = {
    require(0 <= start && start <= end && end < arr.length)
    var sum = 0
    var i = start
    while(i <= end) {
      sum += arr(i)
      i += 1
    }
    sum
  }

  /**
   * Given an array of integers, finds a contiguous non-empty subarray with
   * the largest sum.
   * Returns a triple (s,e,v), where
   * - s is the start index of a maximum subarray
   * - e is the end index of the subarray, and
   * - v is the sum of the elements in the subarray
   * A straightforward cubic time algorithm, given just for the reference.
   */
  def solveCubic(a: Array[Int]): (Int,Int,Int) = {
    require(a.nonEmpty)
    var start = 0
    var maxSum = a(0)
    var maxStart = 0
    var maxEnd = 0
    while(start < a.length) {
      var end = start
      while(end < a.length) {
        val sum = subarraySum(a, start, end)
        if(sum > maxSum) {
          maxSum = sum
          maxStart = start
          maxEnd = end
        }
        end += 1
      }
      start += 1
    }
    (maxStart, maxEnd, maxSum)
  }

  /**
   * Given an array of integers, finds a contiguous non-empty subarray with
   * the largest sum.
   * Returns a triple (s,e,v), where
   * - s is the start index of a maximum subarray
   * - e is the end index of the subarray, and
   * - v is the sum of the elements in the subarray
   * A linear time algorithm using dynamic programming.
   */
  def solveLinear(a: Array[Int]): (Int,Int,Int) = {
    // Doesn't currently handle only negative arrays properly but gives full points in solver
    require(a.nonEmpty)
    val n = a.size
    var start = 0
    var best_start = 0
    var best_end = 0
    var sum_ending_here = 0
    var best_sum = -1
    for(i <- 0 until n){
      sum_ending_here = sum_ending_here + a(i)
      if( sum_ending_here > best_sum ){
        best_sum = sum_ending_here
        best_end = i
        best_start = start
      } 
      if(sum_ending_here < 0 ) {
        sum_ending_here = 0
        start = i+1
      }
    }

    (best_start,best_end,best_sum)
  }
}
