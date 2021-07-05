import scala.collection.mutable.ArrayBuffer
// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object shortestSubstringMissing {
  /**
   * Find a shortest string
   * - whose characters are from the alphabet and
   * - that is not a substring of the string str.
   * A slow method given here just for reference.
   */
  def findOneSlow(str: String, alphabet: Set[Char]): String = {
    require(alphabet.nonEmpty)
    require(str.forall(c => alphabet contains c))
    var length = 1
    while(true) {
      val iter = new StringEnumerator(length, alphabet)
      while(iter.hasNext) {
        val s = iter.next()
        if(!str.contains(s))
          return s
      }
      length += 1
    }
    "" // Just for type checking
  }




  /**
   * Find a shortest string
   * - whose characters are from the alphabet and
   * - that is not a substring of the string str.
   */
  def findOne(str: String, alphabet: Set[Char]): String = {
    require(alphabet.nonEmpty)
    require(str.forall(c => alphabet contains c))
    val n = str.length
    val m = alphabet.size
    
    def findSubstrings(substrlen: Int, possiblePermutations: Int): scala.collection.mutable.HashSet[String] = {
      val foundSubstrings = scala.collection.mutable.HashSet[String]()
      val n = str.length
      var j = 0
      var newSubstring = ""
      while(j+substrlen < n && foundSubstrings.size < possiblePermutations){
        newSubstring = str.substring(j, j + substrlen)
        if(!foundSubstrings.contains(newSubstring))
          foundSubstrings += newSubstring
        j += 1
      }
      foundSubstrings
    }
    
    var foundSubstrings = scala.collection.mutable.HashSet[String]()
    var possiblePermutations = 0
    var i = 1
    
    while( i < n ){
      possiblePermutations = Math.pow(m,i).toInt
      foundSubstrings = findSubstrings(i, possiblePermutations)
      if(possiblePermutations > foundSubstrings.size){
        val iter = new StringEnumerator(i, alphabet)
        while(iter.hasNext) {
          val s = iter.next()
          if(!str.contains(s))
            return s
        }
      }
      i += 1
    }
    "" //Type checking
  }
}
