// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import warmup._

class IsRemovedbyOneSpec extends AnyFlatSpec with Matchers {
  // A pseudo-random number generator
  val rand = new scala.util.Random()

  /** Make a random string of length n over the given alphabet. */
  def makeRandomString(n: Int, alphabet: Set[Char]): String = {
    require(n > 1)
    require(alphabet.nonEmpty)
    // The alphabet as an indexed sequence,
    // allowing selection of random elements from it
    val chars = alphabet.toIndexedSeq
    // Use an Array to build the string as Strings are immutable in Scala
    val a = new Array[Char](n)
    // Build the string by selecting n random characters in it
    var i = 0
    while(i < n) {
      a(i) = chars(rand.nextInt(chars.length))
      i += 1
    }
    a.mkString("")
    // Note: the construction obove could be written simply as
    //   Array.fill[Char](n)(chars(rand.nextInt(chars.length))).mkString("")
    // but the while-loop version is shown above in order to
    // make the linear time loop more visible
  }

  /**
   * Make a random string of length n over the given alphabet, and
   * a string that is obtained from that by removing one random character.
   */
  def makeReducedString(n: Int, alphabet: Set[Char]): (String, String) = {
    require(n > 1)
    require(alphabet.nonEmpty)
    val s = makeRandomString(n, alphabet)
    val i = rand.nextInt(n)
    (s, s.take(i)+s.substring(i+1))
  }

  /**
   * Make a random string of length n over the given alphabet, and
   * a string that is obtained from that by removing one random character and
   * then mutating maxMutations indices to random characters.
   */
  def makeMutatedReducedString(n: Int, alphabet: Set[Char], maxMutations: Int): (String, String) = {
    require(n > 1)
    require(alphabet.nonEmpty)
    val (s,t) = makeReducedString(n, alphabet)
    // Select mutated indices
    val indices = (0 until rand.nextInt(maxMutations+1)).map(_ => rand.nextInt(n-1)).toSet
    // The alphabet as an indexed sequence,
    // allowing selection of random elements from it
    val chars = alphabet.toIndexedSeq
    // Mutate the t string
    val tMutated = t.toArray
    for(i <- indices)
      tMutated(i) = chars(rand.nextInt(chars.length))
    (s, tMutated.mkString(""))
  }

  /*
   * The actual tests
   */
  "The isRemovedByOne method" should "work correctly" in {
    val nofTests = 100
    val alphabet = Set('a','b','c','d')
    for(test <- 1 to nofTests) {
      val (s,t) = makeMutatedReducedString(rand.nextInt(10)+2, alphabet, 2)
      withClue(s"On strings s=$s and t=$t:") {
        // Run the slow algorithm
        val (refSol, _) = isRemovedByOneSlow(s, t)
        // Run the fast algorithm
        val (sol, index) = isRemovedByOne(s, t)
        // Check that the solutions match
        sol should be (refSol)
        if(sol) {
          // Check that the returned index is correct
          s.take(index)+s.substring(index+1) should be (t)
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
      val (s,t) = makeMutatedReducedString(rand.nextInt(10)+10000, alphabet, 2)
      // Run the slow algorithm
      val ((refSol, _), refTime) = measureCpuTime {isRemovedByOneSlow(s, t)}
      // Run the fast algorithm
      val ((sol, index), time) = measureCpuTime {isRemovedByOne(s, t) }
      // Print running times to the console
      println(f"slow vs yours: $refTime%.3g vs $time%.3g")
      // Update cumulative running times
      refCumuTime += refTime
      cumuTime += time
      // Check that the solutions match
      sol should be (refSol)
      if(sol) {
        // Check that the returned index is correct
        s.take(index)+s.substring(index+1) should be (t)
      }
    }
    // Compute speedup and print cumulative running time information
    val speedup = refCumuTime / cumuTime
    println(f"Cumulative times:")
    println(f" isRemovedByOneSlow: $refCumuTime%.3f")
    println(f" isRemovedByOne:     $cumuTime%.3f")
    println(f" speedup: $speedup%.1f")
    speedup should be >= (100.0)
  }
}
