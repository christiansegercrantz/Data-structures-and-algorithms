// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package allAnagrams


/**
 * A simple class for a tool that effciently finds anagrams
 * included in the given dictionary.
 * The initialization phase can take small amount of time but
 * the single queries with the 'find' method should be very fast.
 */
class AnagramFinder(val dictionary: Seq[String]) {
  // Because/if your solution needs initialization code or data structures
  // (a scala.collection.mutable.HashMap perhaps), insert them here
  //import scala.collection.mutable._

  val wordMap = scala.collection.mutable.HashMap[String,scala.collection.mutable.Set[String]]()
  val n = dictionary.length
  var i = 0
  while(i < n){
    if(wordMap.get(dictionary(i).sorted) != None ){
      wordMap(dictionary(i).sorted) += dictionary(i)
    } else{
    wordMap(dictionary(i).sorted) = scala.collection.mutable.Set(dictionary(i))
    }
    i += 1
  }

  /** Find all the anagrams of the the word in the dictionary */
  def find(word: String): Set[String] = {
    wordMap(word.sorted).toSet
    }
}
