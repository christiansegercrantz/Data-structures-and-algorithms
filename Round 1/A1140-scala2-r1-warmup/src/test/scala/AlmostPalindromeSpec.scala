// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import warmup._

class AlmostPalindromeSpec extends AnyFlatSpec with Matchers {
  // The pseudo-random number generator
  val rand = new util.Random()

  /** Make a random palindrome of length n over the given alphabet. */
  def makePalindrome(n: Int, alphabet: Set[Char]): String = {
    require(n > 0)
    require(alphabet.nonEmpty)
    // The palindrome as an Array (Strings are immutable in Java and Scala)
    val a = new Array[Char](n)
    // The current "beginning" and "end" indices
    var i = 0
    var j = n-1
    // The alphabet as an indexed sequence,
    // allowing one to select random elements from it
    val chars = alphabet.toIndexedSeq
    // Form the random palindrome by inserting the same characters
    // at the "beginning" and the "end"
    while(i <= j) {
      // Get a random character
      val c = chars(rand.nextInt(chars.length))
      // Insert it at the current "beginning" and "end" indices
      a(i) = c
      a(j) = c
      // Move "beginning" forwards and "end" backwards
      i += 1
      j -= 1
    }
    // Make the String representation of the palindrome and return it
    a.mkString("")
  }

  /**
   * Make a random palindrome of length n over the given alphabet,
   * and "mutate" it by inserting a random character
   * before at most maxMutations indices.
   */
  def makeMutatedPalindrome(n: Int, alphabet: Set[Char], maxMutations: Int): String = {
    require(n > 0)
    require(alphabet.nonEmpty)
    // Get a random palindrome
    val palindrome = makePalindrome(n, alphabet)
    // The mutated result. Construct with StringBuilder as Strings are immutable
    val mutated = new collection.mutable.StringBuilder()
    // Select random indices before whom a random character is inserted
    val indices = (0 until rand.nextInt(maxMutations+1)).map(_ => rand.nextInt(n)).toSet
    // The alphabet as an indexed sequence,
    // allowing one to select random elements from it
    val chars = alphabet.toIndexedSeq
    for(i <- 0 until n) {
      // Insert a random character before the index i?
      if(indices(i)) {
        val c = chars(rand.nextInt(chars.length))
        mutated += c
      }
      // Insert the original palindrome character at index i
      mutated += palindrome(i)
    }
    // Return the string representation of the (possibly) mutated palindrome
    mutated.toString
  }

  "The almostPalindrome method" should "work correctly" in {
    val nofTests = 100
    val alphabet = Set('a','b','c','d')
    for(test <- 1 to nofTests) {
      val s = makeMutatedPalindrome(rand.nextInt(10)+1, alphabet, 3)
      withClue(s"On string '$s':") {
        // Run the slow reference
        val (refSol, _) = almostPalindromeSlow(s)
        // Run the fast algorithm
        val (sol, index) = almostPalindrome(s)
        // They should agree whether s is a palindrome or almost a palindrome
        sol should be (refSol)
        // If s is a pllindrome or almost a palindrome,
        // the index returned by the fast algorithm should be a correct one
        if(sol) {
          if(index == None)
            isPalindrome(s) should be (true)
          else {
            val i = index.get
            isPalindrome(s.take(i)+s.substring(i+1)) should be (true)
          }
        }
      }
    }
  }

  it should "be at least 100 times faster than the slow method on strings of size around 10000" in {
    import timer.measureCpuTime
    val nofTests = 50
    val alphabet = Set('a','b','c','d')
    var refCumuTime = 0.0
    var cumuTime = 0.0
    for(test <- 1 to nofTests) {
      val s = makeMutatedPalindrome(rand.nextInt(10)+10000, alphabet, 3)
      // Run the slow algorithm
      val ((refSol, _), refTime) = measureCpuTime { almostPalindromeSlow(s) }
      // Run the fast algorithm
      val ((sol, index), time) = measureCpuTime { almostPalindrome(s) }
      // Print the running times to the console
      println(f"slow vs yours: $refTime%.3g vs $time%.3g")
      // Update the cumulative times
      refCumuTime += refTime
      cumuTime += time
      // Check that the solutions agree
      sol should be (refSol)
      // If s is a palindrome or an almost palindrome,
      // check that the returned index is correct
      if(sol) {
        if(index == None)
          isPalindrome(s) should be (true)
        else {
          val i = index.get
          isPalindrome(s.take(i)+s.substring(i+1)) should be (true)
        }
      }
    }
    // Compute the speedup and print running times information to the console
    val speedup = refCumuTime / cumuTime
    println(f"Cumulative times:")
    println(f" almostPalindromeSlow: $refCumuTime%.3f")
    println(f" almostPalindrome:     $cumuTime%.3f")
    println(f" speedup: $speedup%.1f")
    speedup should be >= (100.0)
  }
}
