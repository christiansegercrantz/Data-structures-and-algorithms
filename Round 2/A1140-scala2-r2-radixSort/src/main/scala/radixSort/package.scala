// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object radixSort {
  /**
   * Least-significant-digit-first radix sort for integer arrays.
   * Sorts the argument array in ascending order.
   * In the basic version (for 80 percent of the points),
   * you may assumes that all the integers are non-negative.
   * Interpret 'digit' as 'byte' (8 bits) here and
   * use bit-level operations such as
   * shifting (>> and <<) and masking (&) to extract 'digits'.
   */
  def lsdRadixSort(a: Array[Int]): Unit = {
    val n = a.length
    if(n <= 1) return
    var significantDigit: Int = 3

    def _sort(a: Array[Int]){
      if(significantDigit < 0) return 
      var aux: Array[Int] = new Array[Int](n)
      var count: Array[Int] = new Array[Int](256)
      var i = 0
      var bit: Int = 0
      while(i < n){
        bit = (a(i) >> (8 * (3 - significantDigit))) & 0xff
        count(bit) += 1
        i += 1
      }
      count(0) -= 1
      count = count.scanLeft(0)(_ + _).tail

      var j = n-1
      while( j >= 0){
        bit = (a(j) >> (8 * (3 - significantDigit))) & 0xff
        aux(count(bit)) = a(j)
        count(bit) -= 1
        j -= 1
      }
      significantDigit -= 1
      return _sort(aux)
    }

    _sort(a)
  } 
}
