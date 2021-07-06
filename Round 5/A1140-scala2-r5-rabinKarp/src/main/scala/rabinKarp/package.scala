// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Authors: Markus Arlander and Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object rabinKarp {
  /**
   * A reference implementation of the naive substring search algorithm.
   * Returns the starting index of the first occurrence of the pattern
   * in the text, or -1 if the pattern does not occur in the text.
   * Works in time O(nm), where n is the lenght of the text string and
   * m is the lenght of the pattern string.
   */
  def findSubstringNaive(text: String, pattern: String): Int = {
    val n = text.size
    val m = pattern.size
    val end = n - m
    var i = 0
    while(i <= end) {
      var j = i
      var k = 0
      while(k < m && text(j) == pattern(k)) {
        j += 1
        k += 1
      }
      if(k == m)
        return i
      i += 1
    }
    -1
  }

  /**
   * Substring search with the Rabin-Karp algorithm.
   * Returns the starting index of the first occurrence of the pattern
   * in the text, or -1 if the pattern does not occur in the text.
   * Works in expected time O(n+m), where n is the lenght of the text string and
   * m is the lenght of the pattern string.
   */
  def findSubstring(text: String, pattern: String): Int = {
    val n = text.size
    val m = pattern.size
    if(m > n)
      return -1
    val end = n - m
    val prime = 101
    var patternHash: Int = 0
    var currentSubstring: String = text.substring(0,m)
    var rollingHash: Int = 0
    var total = 1
    var i = 0
    while(i < m){
      patternHash += pattern(m-1-i).toInt * total
      rollingHash += currentSubstring(m-1-i).toInt * total
      if( i+1 != m){
        total *= prime
      }
      i += 1
    }
    val maxPrime = total
    i = 0
    while(i <= end) {
      rollingHash = if(i==0) rollingHash else (rollingHash - text(i-1).toInt*maxPrime) * prime + text(i+m-1)
      if(patternHash == rollingHash){
        if(text.substring(i,i+m) == pattern){
          return i
        }
      }
      i += 1
    }
    -1
  }
}
