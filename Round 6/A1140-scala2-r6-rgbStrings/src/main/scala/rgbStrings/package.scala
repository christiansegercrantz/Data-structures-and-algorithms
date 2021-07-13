// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object rgbStrings {
  /**
   * Count the number of all strings of length n over
   * the characterst 'r', 'g', and 'b'
   * in which it holds that
   * 'r' is never next to 'g' and vise versa.
   * For instance, when n is 3, then the answer is 17 (
   * the strings are rrr, rrb, rbr, rbg, rbb, ggg, ggb, gbr, gbg, rbb,
   * brr, brb, bgg, bgb, bbr, bbg, and bbb ).
   * Note: the numbers can grow VERY large => use BigInt in all the computations.
   * Note: we are only interested in the number of such strings and one should NOT explicitly generate any of them. Thus your code should NOT contain any vars or vals of type String or Char.
   */
  def count(n: Int): BigInt = {
    require(n > 0)
    var r = BigInt(1)
    var prevR = BigInt(0)
    var g = BigInt(1)
    var prevG = BigInt(0)
    var b = BigInt(1)
    var prevB = BigInt(0)
    for( i <- 1 until n){
      var newR = r + b
      val newG = g + b
      val newB = r + g + b
      prevR = r 
      r = newR
      prevG = g
      g = newG
      prevB = b
      b = newB
    }
    r+g+b
  }
}
