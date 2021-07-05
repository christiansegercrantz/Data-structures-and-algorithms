// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package shortestSubstringMissing

/**
 * A helper iterator class that enumerates all the strings of length n
 * over the given alphabet.
 */
class StringEnumerator(n: Int, alphabet: Set[Char]) extends Iterator[String]
{
  require(n > 0)
  require(alphabet.nonEmpty)

  // Denote the size of the alphabet by m.
  private val m = alphabet.size

  // There are m^n strings of length n over the alphabet of size m.
  // The implementation here requires that m^n < 2^63, ie n < 63 / log2(m)
  require(n < 63.0*math.log(2) / math.log(m))

  // A private Array copy of the alphabet for fast random access
  private val alpha = alphabet.toArray.sorted

  // The "index" of the current string
  private var current: Long = 0L

  // There are m^n strings of length n over the alphabet of size m.
  // Denote this value by "end".
  private val end = (0 until n).foldLeft(1L)({case (prev,i) => prev*m})

  // Temporaty space for constring the next string of length n
  private val tmp = new Array[Char](n)

  def hasNext: Boolean = {
    current != end
  }

  def next(): String = {
    // The "index" of the current string
    var v = current
    // Make (an Array representation of) the string from v
    // by interpreting it as a base-m number
    for(i <- 0 until n) {
      val c = alpha((v % m).toInt)
      v = v / m
      tmp(n-i-1) = c
    }
    // Go to the next string
    current += 1
    // Make the String representation of tmp
    tmp.mkString("")
  }
}
