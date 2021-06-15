// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object warmup {
  /**
   * Check whether the target string t is obtained by removing exactly one
   * character from the source string s.
   * - If this is the case, return (true, i), where i is an index of
   *   a character in s whose removal results in t.
   * - If this is not the case, return (false, 0).
   * This is a slow, quadratic time algorithm given for reference.
   */
  def isRemovedByOneSlow(s: String, t: String): (Boolean, Int) = {
    if(s.length != t.length + 1)
      return (false, 0)
    // For each index in the string s, test whether t equals to
    // the string formed by dropping the character at that index from s.
    // Note: take, substring, + and == on strings are linear time operations.
    for(i <- 0 until s.length)
      if(s.take(i) + s.substring(i+1) == t)
        return(true, i)
    // All tests failed, we cannot form t by removing one character from s
    return (false, 0)
  }

  /**
   * Check whether the target string t is obtained by removing exactly one
   * character from the source string s.
   * - If this is the case, return (true, i), where i is an index of
   *   a character in s whose removal results in t.
   * - If this is not the case, return (false, 0).
   * This algorithm should be fast, running in time O(|s|),
   * where |s| is the length of the string s.
   *
   * Hint: start comparing the strings from the beginning. If they differ
   *       at some point, what should hold for the rest of the strings?
   */
  def isRemovedByOne(s: String, t: String): (Boolean, Int) = {
    if(s.length != t.length + 1)
      return (false, 0) 
    
    for(i <- 0 until t.length){
      if(s(i) != t(i)){
        if(s.substring(i+1) == t.substring(i))
          return(true,i)
        else 
          return(false,0)
      }}
    return(true,s.length-1)
  }


  /** A helper function for checking whether a string is a palindrome. */
  def isPalindrome(s: String): Boolean = {
    // The current "beginning"
    var i = 0
    // The current "end"
    var j = s.length-1
    // Do the following as long as the substring between "beginning" and "end"
    // has at least two characters
    while(i < j) {
      // Different characters at "beginning" and "end": not a palindrome
      if(s(i) != s(j))
        return false
      // Move "beginning" forwards and "end" backwards one index
      i += 1
      j -= 1
    }
    return true
  }

  /**
   * Check whether the argument string is a palindrome or
   * can be made a palindrome by removing exactly one character from it.
   * - If it is not, return (false, None).
   * - If it is a palindrome, return (true, None).
   * - If it is a palindrome after removing one character,
   *   return (true, Some(i)), where i is an index to remove to make the string
   *   palindrome.
   * This is a slow, quadratic time algorithm given for reference.
   */
  def almostPalindromeSlow(s: String): (Boolean, Option[Int]) = {
    // Check whether s is a palindrome. A linear time operation.
    if(isPalindrome(s))
      return (true, None)
    // We now know that s is not a palindrome.
    // For each index of s, check whether the string formed by
    // removing the character from that index is a palindrome.
    // Performs at most |s| tests, each taking time O(|s|).
    // Worst case strings can be of the form a...abbcca...a
    for(i <- 0 until s.length)
      if(isPalindrome(s.take(i) + s.substring(i+1)))
        return(true, Some(i))
    // The string is not a palindrome or almost a palindrome
    return (false, None)
  }

  /**
   * Check whether the argument string is a palindrome or
   * can be made a palindrome by removing exactly one character from it.
   * - If it is not, return (false, None).
   * - If it is a palindrome, return (true, None).
   * - If it is a palindrome after removing one character,
   *   return (true, Some(i)) where i is an index to remove to make the string
   *   palindrome.
   * This algorithm should be fast, i.e. run in linear time.
   *
   * Hint: first compare the first and the last character in the string.
   *       If they are the same, then the string can be a palindrome and
   *          one can do, iteratively, similar analysis for the substring
   *          obtained by excluding the first and the last character.
   *          Do not use the "substring" method to obtain the substring but
   *          only use indices that point to the currently inspected
   *          first and last characters.
   *       If they are not the same, then what are the three possible cases?
   */
  def almostPalindrome(s: String): (Boolean, Option[Int]) = {
    if(isPalindrome(s))
      return (true, None)

    val strLen = s.length
    var i = 0
    var j = s.length-1
    while (i < j){
      if( s(i) != s(j) ){
        if(isPalindrome(s.take(i) + s.substring(i+1)))
          return (true, Some(i))
        if(isPalindrome(s.take(j) + s.substring(j+1)))
          return (true, Some(j))
        println("Not possible")
        return (false, None)
      }
    i+=1
    j-=1
    }

    return (false, None)
  }
}
