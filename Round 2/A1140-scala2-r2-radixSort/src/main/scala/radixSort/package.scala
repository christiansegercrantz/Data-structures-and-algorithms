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
    var significantDigit = 3
    var stopAt = 0
    var aux: Array[Int] = a.clone
    var count: Array[Int] = new Array[Int](256)
    
    var k = n-1
    var toSigDig = 0
    var decrease = true
    while (toSigDig < significantDigit && decrease){
      while(k >= 0 && decrease){
        decrease = (a(k) >> (8 * (3 - toSigDig))) == 0 && decrease
        k-=1
      }
      if(decrease){
        stopAt += 1
        toSigDig += 1
      }
    }

    def _sort(b: Array[Int]){
      if(significantDigit < stopAt) return 
      
      k = 255
      while(k >= 0){
        count(k) = 0
        k-=1
      }
      var i = 0
      var byte: Int = 0
      while(i < n){
        byte = (b(i) >> (8 * (3 - significantDigit))) & 0xff
        count(byte) += 1
        i += 1
      }
      count(0) -= 1
      count = count.scanLeft(0)(_ + _).tail

      var j = n-1
      while( j >= 0){
        byte = (b(j) >> (8 * (3 - significantDigit))) & 0xff
        aux(count(byte)) = b(j)
        count(byte) -= 1
        j -= 1
      }
      significantDigit -= 1
      return _sort(aux.clone)
    }
    _sort(aux.clone)
    var positive = true
    k = 0
    while(k < n && positive){
      if(aux(k) < 0){
        positive = false
        aux = aux.slice(k, n) ++ aux.slice(0, k)
      } 
      k += 1
    }

    var j = n-1
      while( j >= 0){
        a(j) = aux(j)
        j -= 1
      }
  } 
}
